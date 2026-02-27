# BandAid upload module.R  (DuckDB version - robust for messy numeric columns)
# i18n-ready: UI is re-rendered when language changes (lang reactive)
# MOD: Automatically merge Look Up CSVs (./Look Ups) into uploaded table before filtering
# MOD: Single-pass join of all lookups for performance
# MOD: DuckDB threads set via integer (no 'auto') to avoid crash
# MOD: Show/hide loading overlay from inside the module via shinyjs (guaranteed hide)

get_tr <- function() {
  if (exists("tr", envir = .GlobalEnv, inherits = TRUE)) {
    tr_fun <- get("tr", envir = .GlobalEnv, inherits = TRUE)
    if (is.function(tr_fun)) return(tr_fun)
  }
  if (exists("i18n", envir = .GlobalEnv, inherits = TRUE)) {
    i18n_obj <- get("i18n", envir = .GlobalEnv, inherits = TRUE)
    if (!is.null(i18n_obj) && is.function(i18n_obj$t)) {
      return(function(x) i18n_obj$t(x))
    }
  }
  function(x) x
}

# ---- Helpers for lookup merge ----

sql_ident <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))

sql_path_literal <- function(path) {
  # DuckDB likes forward slashes; also escape single quotes for SQL literal
  p <- normalizePath(path, winslash = "/", mustWork = TRUE)
  p <- gsub("'", "''", p)
  paste0("'", p, "'")
}

get_app_dir_fallback <- function() {
  # app.R defines app_dir globally; use it if present
  if (exists("app_dir", envir = .GlobalEnv, inherits = TRUE)) {
    ad <- get("app_dir", envir = .GlobalEnv, inherits = TRUE)
    if (is.character(ad) && length(ad) == 1 && nzchar(ad)) return(ad)
  }
  # fallback
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

# Canonicalize a column name: lower + remove separators (spaces, dots, underscores, etc.)
canon_cache <- new.env(parent = emptyenv())

.canon_name <- function(x) {
  if (length(x) == 0L) return(character(0))
  res <- character(length(x))
  for (i in seq_along(x)) {
    key <- x[[i]]
    if (!nzchar(key)) {
      res[[i]] <- ""
    } else if (exists(key, canon_cache, inherits = FALSE)) {
      res[[i]] <- canon_cache[[key]]
    } else {
      v <- trimws(key)
      v <- sub("^\ufeff", "", v)
      v <- tolower(gsub("[^a-z0-9]+", "", v))
      canon_cache[[key]] <- v
      res[[i]] <- v
    }
  }
  res
}


# Case/spacing-insensitive "any like this name?"
.has_col_like <- function(cols, target) {
  if (!length(cols)) return(FALSE)
  cn <- vapply(cols, .canon_name, character(1))
  .canon_name(target) %in% cn
}

# Tolerant resolver: find the actual column name in `cols` that matches the target label
.find_like <- function(cols, target) {
  key <- .canon_name(target)
  canon <- vapply(cols, .canon_name, character(1))
  hit <- cols[canon == key]
  if (length(hit)) return(hit[[1]])
  variants <- unique(c(target, gsub("\\.", " ", target), gsub("\\.", "_", target), gsub("\\.", "", target)))
  for (v in variants) {
    key2 <- .canon_name(v)
    hit2 <- cols[canon == key2]
    if (length(hit2)) return(hit2[[1]])
  }
  NA_character_
}


# ---- Single-pass lookup join (fast) ----
apply_lookup_joins_to_uploaded <- function(con, lookups_dir, tr = function(x) x) {
  
  # --- Normalization helpers (tolerant to space/dot/_/-, case, BOM) ---
  canon <- function(x) {
    x <- trimws(x)
    x <- sub("^\ufeff", "", x)       # remove BOM
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", " ", x)  # treat separators as spaces
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  compact <- function(x) gsub(" ", "", canon(x))
  
  # Find best matching column name in a vector of column names
  find_col <- function(cols, target) {
    if (is.null(cols) || !length(cols)) return(NA_character_)
    key <- canon(target)
    cmp <- gsub(" ", "", key)
    cols_key <- vapply(cols, canon, character(1))
    cols_cmp <- vapply(cols, compact, character(1))
    hit <- which(cols_key == key)
    if (!length(hit)) hit <- which(cols_cmp == cmp)
    if (!length(hit)) return(NA_character_)
    cols[[hit[[1]]]]
  }
  
  # Make unique output column name (avoid collisions)
  make_unique_name <- function(existing, proposed) {
    if (!(proposed %in% existing)) return(proposed)
    k <- 2
    alt <- paste0(proposed, " (", k, ")")
    while (alt %in% existing) {
      k <- k + 1
      alt <- paste0(proposed, " (", k, ")")
    }
    alt
  }
  
  if (!dir.exists(lookups_dir)) {
    message("[Lookups] Folder not found: ", lookups_dir)
    return(invisible(FALSE))
  }
  
  files <- list.files(lookups_dir, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) {
    message("[Lookups] No CSV files found in: ", lookups_dir)
    return(invisible(FALSE))
  }
  
  # Base schema
  uploaded_info <- DBI::dbGetQuery(con, "PRAGMA table_info('uploaded');")
  uploaded_cols <- uploaded_info$name
  uploaded_names <- uploaded_cols
  

  if (!length(uploaded_names)) return(invisible(FALSE))
  
  # --- Special rules (tolerant patterns) ---
  # add = character vector of columns to add, or "__ALL_NON_KEY__"
  special_rules <- list(
    list(pattern = "^gamebirds?_band_type",   main_key = "Band.Type.Current", lkp_key = "__FIRST__",            add = c("Code Description"),
         prefix_by_main = TRUE),
    list(pattern = "^gamebirds?_permits",     main_key = "Permit",            lkp_key = "__FIRST__",            add = c("Permittee"),
         prefix_by_main = FALSE),
    list(pattern = "^gamebirds?_bird_status", main_key = "Status",            lkp_key = "Status Code",          add = c("Code Description", "Code Description (detailed)"),
         prefix_by_main = TRUE),
    list(pattern = "^gamebirds?_pres_cond",   main_key = "Pres..Cond.",       lkp_key = "Pres. Cond. Code",     add = "__ALL_NON_KEY__",
         prefix_by_main = FALSE),
    list(pattern = "^gamebirds?_species",     main_key = "Sp..Num.",          lkp_key = "Sp. Num. (AOU Code)",  add = "__ALL_NON_KEY__",
         prefix_by_main = FALSE)
  )
  match_rule <- function(filename) {
    for (r in special_rules) if (grepl(r$pattern, filename, ignore.case = TRUE)) return(r)
    NULL
  }
  
  # Collect all joins and selections to do in ONE pass
  joins_sql  <- character(0)
  select_add <- character(0)
  existing_cols <- uploaded_names   # grows as we plan new output columns
  
  # We'll create one TEMP table per lookup (as before), but only ONE final CTAS
  lkp_count <- 0
  
  lookup_sql_batch <- character(0)
  for (f in files) {
    bn <- basename(f)
    
    hdr <- tryCatch(
      readr::read_csv(f, n_max = 0, show_col_types = FALSE),
      error = function(e) NULL
    )
    if (is.null(hdr)) { message("[Lookups] Skipping (cannot read): ", bn); next }
    
    lkp_cols <- names(hdr)
    if (length(lkp_cols) < 2) { message("[Lookups] Skipping (needs >= 2 columns): ", bn); next }
    
    rule <- match_rule(bn)
    
    if (!is.null(rule)) {
      # --- SPECIAL CASE ---
      main_key_actual <- find_col(uploaded_names, rule$main_key)
      if (is.na(main_key_actual)) { message("[Lookups] Skipping (no main col '", rule$main_key, "'): ", bn); next }
      
      lkp_key_actual <- if (identical(rule$lkp_key, "__FIRST__")) lkp_cols[[1]] else find_col(lkp_cols, rule$lkp_key)
      if (is.na(lkp_key_actual)) { message("[Lookups] Skipping (no lookup key '", rule$lkp_key, "'): ", bn); next }
      
      add_cols_actual <- if (identical(rule$add, "__ALL_NON_KEY__")) {
        setdiff(lkp_cols, lkp_key_actual)
      } else {
        vapply(rule$add, function(x) find_col(lkp_cols, x), character(1))
      }
      if (!length(add_cols_actual) || any(is.na(add_cols_actual))) {
        message("[Lookups] Skipping (missing add cols): ", bn); next
      }
      
      # Create TEMP lookup table
      lkp_tbl <- paste0(
        "lkp_",
        gsub("[^A-Za-z0-9]+", "_", tolower(canon(bn))),
        "_",
        as.integer(stats::runif(1, 1e6, 9e6))
      )
      f_sql <- sql_path_literal(f)
      lookup_sql_batch <- c(
        lookup_sql_batch,
        paste0(
          "CREATE TEMP TABLE ", lkp_tbl, " AS ",
          "SELECT * FROM read_csv_auto(", f_sql, ", header=TRUE, nullstr=['-', '--', 'NA', 'N/A', '']);"
        )
      )
      
      
      # Prepare SELECT additions
      for (ac in add_cols_actual) {
        out_name <- if (isTRUE(rule$prefix_by_main) && canon(ac) %in% c("code description","code description (detailed)")) {
          paste0(main_key_actual, " ", ac)
        } else {
          ac
        }
        out_name <- make_unique_name(existing_cols, out_name)
        existing_cols <- c(existing_cols, out_name)
        select_add <- c(
          select_add,
          paste0("l", lkp_count, ".", sql_ident(con, ac), " AS ", sql_ident(con, out_name))
        )
      }
      
      # Append JOIN
      joins_sql <- c(joins_sql, paste0(
        "LEFT JOIN ", lkp_tbl, " l", lkp_count, " ON ",
        "TRY_CAST(u.", sql_ident(con, main_key_actual), " AS BIGINT) = ",
        "TRY_CAST(l", lkp_count, ".", sql_ident(con, lkp_key_actual), " AS BIGINT)"
      ))
      
      lkp_count <- lkp_count + 1
      next
    }
    
    # --- STANDARD CASE ---
    code_col <- lkp_cols[[1]]
    desc_col <- lkp_cols[[2]]
    
    # standard rules (tolerant)
    if (!grepl("\\s+code\\s*$", canon(code_col))) {
      message("[Lookups] Skipping (1st col must end with ' Code'): ", bn, " [", code_col, "]")
      next
    }
    if (canon(desc_col) != "code description") {
      message("[Lookups] Skipping (2nd col not 'Code Description'): ", bn, " [", desc_col, "]")
      next
    }
    
    # Determine base field in uploaded
    base_field_raw <- sub("(?i)\\s*Code\\s*$", "", code_col, perl = TRUE)
    base_field <- find_col(uploaded_names, base_field_raw)
    if (is.na(base_field)) {
      message("[Lookups] Skipping (no main col matching '", base_field_raw, "'): ", bn)
      next
    }
    
    # Create lookup table name
    lkp_tbl <- paste0(
      "lkp_",
      gsub("[^A-Za-z0-9]+", "_", tolower(canon(base_field_raw))),
      "_",
      as.integer(stats::runif(1, 1e6, 9e6))
    )
    
    # Batch lookup table creation
    f_sql <- sql_path_literal(f)
    lookup_sql_batch <- c(
      lookup_sql_batch,
      paste0(
        "CREATE TEMP TABLE ", lkp_tbl, " AS ",
        "SELECT * FROM read_csv_auto(", f_sql, ", header=TRUE, nullstr=['-', '--', 'NA', 'N/A', '']);"
      )
    )
    
    # Add description column
    new_desc_name <- make_unique_name(existing_cols, paste0(base_field, " Code Description"))
    existing_cols <- c(existing_cols, new_desc_name)
    select_add <- c(
      select_add,
      paste0("l", lkp_count, ".", sql_ident(con, desc_col), " AS ", sql_ident(con, new_desc_name))
    )
    
    # Resolve join keys (STANDARD CASE)
    main_key_actual <- base_field
    lkp_key_actual  <- code_col
    
    # Build JOIN clause
    joins_sql <- c(
      joins_sql,
      paste0(
        "LEFT JOIN ", lkp_tbl, " l", lkp_count, " ON ",
        "TRY_CAST(u.", sql_ident(con, main_key_actual), " AS BIGINT) = ",
        "TRY_CAST(l", lkp_count, ".", sql_ident(con, lkp_key_actual), " AS BIGINT)"
      )
    )
    
    lkp_count <- lkp_count + 1
    
  }
  
  if (length(lookup_sql_batch)) {
    DBI::dbExecute(con, paste(lookup_sql_batch, collapse = "\n"))
  }
  
  if (!length(joins_sql)) {
    message("[Lookups] No applicable lookups to join.")
    return(invisible(TRUE))
  }
  
  # --- Corr.Year inline (robust resolution) ---
  r_month_col <- find_col(uploaded_names, "R.Month")
  if (is.na(r_month_col)) r_month_col <- find_col(uploaded_names, "R Month")
  
  r_year_col  <- find_col(uploaded_names, "R.Year")
  if (is.na(r_year_col)) r_year_col <- find_col(uploaded_names, "R Year")
  
  if (!is.na(r_month_col) && !is.na(r_year_col)) {
    
    corr_sql <- paste0(
      "CASE
       WHEN TRY_CAST(u.", sql_ident(con, r_month_col), " AS INTEGER) IS NOT NULL
            AND TRY_CAST(u.", sql_ident(con, r_month_col), " AS INTEGER) < 6
       THEN TRY_CAST(u.", sql_ident(con, r_year_col), " AS INTEGER) - 1
       ELSE TRY_CAST(u.", sql_ident(con, r_year_col), " AS INTEGER)
     END AS ", sql_ident(con, "Corr.Year")
    )
    
    select_add <- c(select_add, corr_sql)
    existing_cols <- c(existing_cols, "Corr.Year")
    
  } else {
    message("[Upload] Skipping Corr.Year (missing R.Month or R.Year)")
  }
  print(select_add)
  
  
  
  # ---- ONE PASS: materialize enriched uploaded
  tmp_tbl <- paste0("uploaded_tmp_", as.integer(stats::runif(1, 1e6, 9e6)))
  select_extra <- paste(select_add, collapse = ", ")
  
  sql <- paste0(
    "CREATE TABLE ", tmp_tbl, " AS ",
    "SELECT u.*", if (nchar(select_extra)) paste0(", ", select_extra) else "", " ",
    "FROM uploaded u ",
    paste(joins_sql, collapse = " ")
  )
  
  DBI::dbExecute(con, sql)
  DBI::dbExecute(con, "DROP TABLE uploaded;")
  DBI::dbExecute(con, paste0("ALTER TABLE ", tmp_tbl, " RENAME TO uploaded;"))
  
  message("[Lookups] Single-pass join complete: ", lkp_count, " lookup(s) applied.")
  invisible(TRUE)
}

# ---- Module UI / server ----

mod_upload_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("upload_ui"))
}

mod_upload_server <- function(id, lang) {
  moduleServer(id, function(input, output, session) {
    
    tr <- get_tr()
    
    sync_i18n_lang <- function(current_lang) {
      if (exists("i18n", envir = .GlobalEnv, inherits = TRUE)) {
        i18n_obj <- get("i18n", envir = .GlobalEnv, inherits = TRUE)
        if (!is.null(i18n_obj) && is.function(i18n_obj$set_translation_language)) {
          if (!is.null(current_lang) && nzchar(current_lang)) {
            i18n_obj$set_translation_language(current_lang)
          }
        }
      }
    }
    
    # Start timer and show overlay (both mechanisms; robust)
    session$userData$upload_start <- Sys.time()
    session$userData$upload_loading <- TRUE
    session$sendCustomMessage("setLoadingText", tr("Loading data... Please wait"))
    session$sendCustomMessage("setLoading", TRUE)
    shinyjs::show("loading-overlay")
    
    # Guarantee hide/stop even on
    
    # Re-render upload input when language changes
    output$upload_ui <- renderUI({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      fileInput(
        session$ns("file"),
        tr("Upload data (CSV or Excel)"),
        accept = c(".csv", ".xlsx")
      )
    })
    
    db_state <- reactiveVal(NULL)
    
    # Small defer to ensure filters UI is rendered, then hide overlay and stop timer
    shinyjs::delay(100, {
      session$userData$upload_loading <- FALSE
      session$sendCustomMessage("setLoading", FALSE)
      shinyjs::hide("loading-overlay")
    })
    
    observeEvent(input$file, {
      req(input$file)
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      
      # ➕ ADD: start timer + show overlay via BOTH mechanisms
      session$userData$upload_start <- Sys.time()
      session$sendCustomMessage("setLoading", TRUE)  # toggles display:flex
      shinyjs::show("loading-overlay")               # fallback; sets display:block
      on.exit({
        session$sendCustomMessage("setLoading", FALSE)
        shinyjs::hide("loading-overlay")
      }, add = TRUE)
      
      ext <- tolower(tools::file_ext(input$file$name))
      
      
      # --- SHOW overlay ASAP, and GUARANTEE HIDE on exit ---
      shinyjs::show("loading-overlay")
      on.exit({
        shinyjs::hide("loading-overlay")
      }, add = TRUE)
      
      ext <- tolower(tools::file_ext(input$file$name))
      
      # If user uploads again in same session, clean previous DB
      old <- db_state()
      if (!is.null(old) && !is.null(old$con)) {
        try(DBI::dbDisconnect(old$con, shutdown = TRUE), silent = TRUE)
        if (!is.null(old$db_file)) try(unlink(old$db_file), silent = TRUE)
      }
      
      db_file <- file.path(tempdir(), paste0("bandaid_", session$token, ".duckdb"))
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file)
      
      # Use integer threads (avoid 'auto' crash)
      try({
        n_threads <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
        if (is.na(n_threads) || n_threads < 1L) n_threads <- 1L
        n_threads <- max(1L, min(n_threads, 16L))
        DBI::dbExecute(con, sprintf("PRAGMA threads=%d;", n_threads))
      }, silent = TRUE)
      
      session$onSessionEnded(function() {
        try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
        try(unlink(db_file), silent = TRUE)
      })
      
      DBI::dbExecute(con, "DROP TABLE IF EXISTS uploaded;")
      
      if (ext == "csv") {
        
        csv_path <- normalizePath(input$file$datapath, winslash = "/", mustWork = TRUE)
        csv_path <- gsub("'", "''", csv_path)
        
        sample_size <- 1000000
        
        import_sql <- glue::glue("
          CREATE TABLE uploaded AS
          SELECT *
          FROM read_csv_auto(
            '{csv_path}',
            header = TRUE,
            all_varchar = TRUE,
            sample_size = {sample_size},
            nullstr = ['-', '--', 'NA', 'N/A', '']
          );
        ")
        
        
        
        ok <- TRUE
        tryCatch(
          DBI::dbExecute(con, import_sql),
          error = function(e) {
            ok <<- FALSE
            message('CSV import failed: ', conditionMessage(e))
            
            DBI::dbExecute(con, glue::glue("
              CREATE TABLE uploaded AS
              SELECT *
              FROM read_csv_auto(
                '{csv_path}',
                header = TRUE,
                all_varchar = TRUE,
                ignore_errors = TRUE,
                nullstr = ['-', '--', 'NA', 'N/A', '']
              );
            "))
            
          }
        )
        
        # Cache schema once (Improvement #2)
        uploaded_info <- DBI::dbGetQuery(con, "PRAGMA table_info('uploaded');")
        uploaded_cols <- uploaded_info$name
        
        
        if (!ok) {
          showNotification(
            tr("CSV had inconsistent values. Loaded with fallback mode: all columns as text + skipped faulty rows. If you need numeric filtering for some columns, we can force types."),
            type = "warning",
            duration = 12
          )
        }
        
      } else if (ext == "xlsx") {
        
        df <- readxl::read_excel(input$file$datapath)
        DBI::dbWriteTable(con, "uploaded", df, overwrite = TRUE)
        
      } else {
        stop(paste0(tr("Unsupported file type:"), " ", ext))
      }
      
      # ---- Merge lookup tables BEFORE returning db_state ----
      app_dir <- get_app_dir_fallback()
      lookups_dir <- file.path(app_dir, "Look Ups")
      
      # 1) Read current columns and decide whether to skip Look-Ups
      all_cols <- uploaded_cols
      
      # A marker we will add once merges were applied (survives into your filtered exports)
      marker_present <- .has_col_like(all_cols, "_lookups_applied")
      
      # A few "sentinel" columns you know come from the Look-Ups merge.
      # >>>> EDIT THIS LIST to match 3–5 columns that only appear AFTER merge:
      lookup_sentinels <- c(
        "Age Code Description",                 
        "Sex Code Description",   
        "Permittee"                   
      )
      
      sentinel_hits <- vapply(lookup_sentinels, function(s) .has_col_like(all_cols, s), logical(1))
      already_merged <- marker_present || sum(sentinel_hits) >= 2  # require at least 2 matches to be safe
      
      if (!already_merged) {
        message("[Upload] Applying Look-Ups merge...")
        apply_lookup_joins_to_uploaded(con, lookups_dir, tr = tr)
        
        # Refresh column list and add a marker so future re-uploads can be auto-detected
        DBI::dbExecute(con, 'ALTER TABLE uploaded ADD COLUMN IF NOT EXISTS "_lookups_applied" BOOLEAN;')
        DBI::dbExecute(con, 'UPDATE uploaded SET "_lookups_applied" = TRUE;')
        
      } else {
        message("[Upload] Skipping Look-Ups: looks like columns already merged in this file.")
      }
      
      # ---- Auto-merge SpeciesForMaps... like a Look-Up ----
      # 1) find the species file in "Look Ups" (accepts "SpeciesForMaps..." or "Species ForMaps...", any extension)
      sp_candidates <- list.files(lookups_dir, full.names = TRUE, ignore.case = TRUE)
      sp_candidates <- sp_candidates[grepl("^(?i)species\\s*for\\s*maps", basename(sp_candidates), perl = TRUE)]
      if (length(sp_candidates)) {
        # Prefer the most recent
        fi <- file.info(sp_candidates)
        sp_path <- sp_candidates[order(fi$mtime, decreasing = TRUE)][1]
        
        # Check whether species is already merged (marker or many species columns already present)
        all_cols <- uploaded_cols
        species_already <- .has_col_like(all_cols, "_species_merged")
        
        # Load species file header to decide skip via overlap (only if marker not present)
        if (!species_already) {
          ext <- tolower(tools::file_ext(sp_path))
          if (ext %in% c("csv", "txt")) {
            sp_df <- tryCatch(readr::read_csv(sp_path, show_col_types = FALSE, n_max = 10),
                              error = function(e) NULL)
          } else if (ext %in% c("xlsx", "xls")) {
            sp_df <- tryCatch(readxl::read_excel(sp_path, n_max = 10),
                              error = function(e) NULL)
          } else {
            sp_df <- NULL
          }
          if (!is.null(sp_df)) {
            sp_cols <- names(sp_df)
            # If >= 2 non-key species columns already exist in uploaded, assume merged
            sp_bbl_col <- .find_like(sp_cols, "BBL_Number")
            sp_nonkey  <- setdiff(sp_cols, sp_bbl_col)
            overlap    <- intersect(.canon_name(all_cols), .canon_name(sp_nonkey))
            species_already <- length(overlap) >= 2
          }
        }
        
        if (species_already) {
          message("[Upload] Skipping Species merge: looks already present.")
        } else {
          message("[Upload] Applying Species merge from: ", basename(sp_path))
          
          # 2) Load the full species file into DuckDB (fast path for CSV)
          ext <- tolower(tools::file_ext(sp_path))
          
          if (ext %in% c("csv", "txt")) {
            
            # DuckDB reads CSV directly (much faster than readr + dbWriteTable)
            DBI::dbExecute(con, sprintf("
              CREATE TEMP TABLE species_lu AS
              SELECT *
              FROM read_csv_auto(%s,
                       header=TRUE,
                       all_varchar=TRUE,
                       nullstr=['-', '--', 'NA', 'N/A', '']);
  ", sql_path_literal(sp_path)))
            
            spcols <- DBI::dbGetQuery(con, "PRAGMA table_info('species_lu');")$name
            
          } else if (ext %in% c("xlsx", "xls")) {
            
            # Excel fallback
            sp_df <- readxl::read_excel(sp_path)
            if (is.null(sp_df) || !nrow(sp_df)) {
              warning("[Upload] Species file found but could not be read: ", sp_path)
              spcols <- character(0)
            } else {
              DBI::dbWriteTable(con, "species_lu", sp_df, overwrite = TRUE)
              spcols <- names(sp_df)
            }
            
          } else {
            
            warning("[Upload] Species file has unsupported extension: ", sp_path)
            spcols <- character(0)
          }
          
          if (!length(spcols)) {
            warning("[Upload] Species merge skipped: no usable species columns.")
          } else {
            
            ucols <- uploaded_cols
            
            spnum_col <- .find_like(ucols, "Sp.Num")
            bbl_col   <- .find_like(spcols, "BBL_Number")
            
            if (is.na(spnum_col) || is.na(bbl_col)) {
              warning("[Upload] Species merge skipped: could not resolve join keys (Sp.Num / BBL_Number).")
            } else {
              
              spnum_id <- as.character(DBI::dbQuoteIdentifier(con, spnum_col))
              bbl_id   <- as.character(DBI::dbQuoteIdentifier(con, bbl_col))
              
              sp_cols_db <- DBI::dbGetQuery(con, "PRAGMA table_info('species_lu');")$name
              sp_cols_keep <- setdiff(sp_cols_db, bbl_col)
              
              collide <- .canon_name(sp_cols_keep) %in% .canon_name(ucols)
              sel_sp  <- character(0)
              
              for (j in seq_along(sp_cols_keep)) {
                sc <- sp_cols_keep[j]
                sc_q <- as.character(DBI::dbQuoteIdentifier(con, sc))
                if (collide[j]) {
                  alias <- paste0("sp.", sc)
                  alias_q <- as.character(DBI::dbQuoteIdentifier(con, alias))
                  sel_sp <- c(sel_sp, sprintf("s.%s AS %s", sc_q, alias_q))
                } else {
                  sel_sp <- c(sel_sp, sprintf("s.%s", sc_q))
                }
              }
              
              sel_sp_sql <- paste(sel_sp, collapse = ",\n          ")
              
              DBI::dbExecute(con, sprintf("
      CREATE TABLE uploaded__sp AS
      SELECT
        u.*%s%s
      FROM uploaded u
      LEFT JOIN species_lu s
        ON TRY_CAST(%s AS BIGINT) = TRY_CAST(s.%s AS BIGINT);
    ",
                                          if (length(sel_sp)) ",\n          " else "",
                                          if (length(sel_sp)) sel_sp_sql else "",
                                          spnum_id, bbl_id))
              
              DBI::dbExecute(con, "DROP TABLE uploaded;")
              DBI::dbExecute(con, "ALTER TABLE uploaded__sp RENAME TO uploaded;")
              DBI::dbExecute(con, 'ALTER TABLE uploaded ADD COLUMN IF NOT EXISTS "_species_merged" BOOLEAN;')
              DBI::dbExecute(con, 'UPDATE uploaded SET "_species_merged" = TRUE;')
            }
          }
          
        }
      } else {
        message("[Upload] No SpeciesForMaps* file found in Look Ups; skipping species merge.")
      }
      
      # 2) (Re)compute derived fields that must exist before filtering (e.g., Corr.Year)
      #    Corr.Year := ifelse(R.Month < 6, R.Year - 1, R.Year)
      #    We resolve the actual column names tolerantly in case the CSV used spaces/underscores instead of dots.
      all_cols <- uploaded_cols
      
      find_like <- function(cols, target) {
        key <- .canon_name(target)
        canon <- vapply(cols, .canon_name, character(1))
        hit <- cols[canon == key]
        if (length(hit)) return(hit[[1]])
        variants <- unique(c(target, gsub("\\.", " ", target), gsub("\\.", "_", target), gsub("\\.", "", target)))
        for (v in variants) {
          key2 <- .canon_name(v)
          hit2 <- cols[canon == key2]
          if (length(hit2)) return(hit2[[1]])
        }
        NA_character_
      }
      

      
      # 3) Proceed with state return
      cols <- DBI::dbGetQuery(con, "PRAGMA table_info('uploaded');")
      validate(need(nrow(cols) > 0, tr("Uploaded table has no columns")))
      
      db_state(list(con = con, table = "uploaded", cols = cols, db_file = db_file))
      
      shinyjs::delay(100, {
        session$sendCustomMessage("setLoading", FALSE)
        shinyjs::hide("loading-overlay")
      })
      
    })
    
    reactive({
      req(db_state())
      db_state()
    })
  })
}

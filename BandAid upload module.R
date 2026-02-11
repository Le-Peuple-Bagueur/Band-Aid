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
  uploaded_names <- DBI::dbGetQuery(con, "PRAGMA table_info('uploaded');")$name
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
      DBI::dbExecute(con, paste0(
        "CREATE TEMP TABLE ", lkp_tbl, " AS ",
        "SELECT * FROM read_csv_auto(", f_sql, ", header=TRUE, nullstr=['-', '--', 'NA', 'N/A', '']);"
      ))
      
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
        "CAST(u.", sql_ident(con, main_key_actual), " AS VARCHAR) = CAST(l", lkp_count, ".", sql_ident(con, lkp_key_actual), " AS VARCHAR)"
      ))
      lkp_count <- lkp_count + 1
      next
    }
    
    # --- STANDARD CASE ---
    code_col <- lkp_cols[[1]]
    desc_col <- lkp_cols[[2]]
    
    # standard rules (tolerant)
    if (!grepl("\\s+code\\s*$", canon(code_col))) { message("[Lookups] Skipping (1st col must end with ' Code'): ", bn, " [", code_col, "]"); next }
    if (canon(desc_col) != "code description")       { message("[Lookups] Skipping (2nd col not 'Code Description'): ", bn, " [", desc_col, "]"); next }
    
    base_field_raw <- sub("(?i)\\s*Code\\s*$", "", code_col, perl = TRUE)
    base_field <- find_col(uploaded_names, base_field_raw)
    if (is.na(base_field)) { message("[Lookups] Skipping (no main col matching '", base_field_raw, "'): ", bn); next }
    
    lkp_tbl <- paste0(
      "lkp_",
      gsub("[^A-Za-z0-9]+", "_", tolower(canon(base_field_raw))),
      "_",
      as.integer(stats::runif(1, 1e6, 9e6))
    )
    f_sql <- sql_path_literal(f)
    DBI::dbExecute(con, paste0(
      "CREATE TEMP TABLE ", lkp_tbl, " AS ",
      "SELECT * FROM read_csv_auto(", f_sql, ", header=TRUE, nullstr=['-', '--', 'NA', 'N/A', '']);"
    ))
    
    new_desc_name <- make_unique_name(existing_cols, paste0(base_field, " Code Description"))
    existing_cols <- c(existing_cols, new_desc_name)
    select_add <- c(
      select_add,
      paste0("l", lkp_count, ".", sql_ident(con, desc_col), " AS ", sql_ident(con, new_desc_name))
    )
    
    joins_sql <- c(joins_sql, paste0(
      "LEFT JOIN ", lkp_tbl, " l", lkp_count, " ON ",
      "CAST(u.", sql_ident(con, base_field), " AS VARCHAR) = CAST(l", lkp_count, ".", sql_ident(con, code_col), " AS VARCHAR)"
    ))
    lkp_count <- lkp_count + 1
  }
  
  if (!length(joins_sql)) {
    message("[Lookups] No applicable lookups to join.")
    return(invisible(TRUE))
  }
  
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
      
      apply_lookup_joins_to_uploaded(con, lookups_dir, tr = tr)
      
      cols <- DBI::dbGetQuery(con, "PRAGMA table_info('uploaded');")
      validate(need(nrow(cols) > 0, tr("Uploaded table has no columns")))
      
      db_state(list(con = con, table = "uploaded", cols = cols, db_file = db_file))
      
      
      # 🔚 ADD: small delay then hide overlay (ensures UI had time to render)
      shinyjs::delay(100, {
        session$sendCustomMessage("setLoading", FALSE)
        shinyjs::hide("loading-overlay")
      })
    
      
      # Defer the hide slightly to ensure UI has time to render downstream
      shinyjs::delay(50, shinyjs::hide("loading-overlay"))
    })
    
    reactive({
      req(db_state())
      db_state()
    })
  })
}

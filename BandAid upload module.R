# BandAid upload module.R  (DuckDB version - robust for messy numeric columns)
# i18n-ready: UI is re-rendered when language changes (lang reactive)

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
    
    observeEvent(input$file, {
      req(input$file)
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      ext <- tolower(tools::file_ext(input$file$name))
      
      db_file <- file.path(tempdir(), paste0("bandaid_", session$token, ".duckdb"))
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_file)
      
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
            message("CSV import failed: ", conditionMessage(e))
            
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
      
      cols <- DBI::dbGetQuery(con, "PRAGMA table_info('uploaded');")
      validate(need(nrow(cols) > 0, tr("Uploaded table has no columns")))
      
      db_state(list(con = con, table = "uploaded", cols = cols, db_file = db_file))
    })
    
    reactive({
      req(db_state())
      db_state()
    })
  })
}

`%||%` <- function(x, y) if (is.null(x)) y else x

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

is_numeric_duckdb <- function(type_str) {
  t <- toupper(type_str)
  t %in% c("INTEGER","BIGINT","SMALLINT","TINYINT",
           "DOUBLE","REAL","FLOAT","DECIMAL","NUMERIC",
           "HUGEINT","UBIGINT","UINTEGER","USMALLINT","UTINYINT")
}

sql_ident <- function(con, x) as.character(DBI::dbQuoteIdentifier(con, x))
sql_lit   <- function(con, x) as.character(DBI::dbQuoteLiteral(con, x))

mod_filters_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filters_ui"))
}

mod_filters_summary_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("summary_ui"))
}

mod_filters_server <- function(id, data_source, lang) {
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
    
    PREVIEW_LIMIT  <- 200000
    #DISTINCT_LIMIT <- 5000
    limit_n <- reactive({
      # input$distinct_limit may be NULL until UI is rendered
      x <- input$distinct_limit %||% 20000
      # optional safety clamp:
      x <- max(4000, min(100000, as.integer(x)))
      x
    })
    
    result <- reactiveVal(NULL)
    
    # Show elapsed time in a toast when data source first becomes available
    observeEvent(data_source(), {
      req(data_source())
      t0 <- session$userData$upload_start
      if (!is.null(t0)) {
        elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        showNotification(sprintf(tr("Data ready in %.1f seconds"), elapsed),
                         type = "message", duration = 8)
        session$userData$upload_start <- NULL
      }
    }, ignoreInit = TRUE)
    
    
    # ➕ ADD: show elapsed time from file pick to filters becoming available
    observeEvent(data_source(), {
      req(data_source())
      t0 <- session$userData$upload_start
      if (!is.null(t0)) {
        elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
        showNotification(sprintf(tr("Data ready in %.1f seconds"), elapsed),
                         type = "message", duration = 8)
        session$userData$upload_start <- NULL
      }
    }, ignoreInit = TRUE)
    
    
    output$filters_ui <- renderUI({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      n <- input$n_filters %||% 1
      
      tagList(
        numericInput(session$ns("n_filters"), tr("Number of filters"), value = n, min = 1, max = 6),
        numericInput(
          session$ns("distinct_limit"),
          tr("Max values shown in lists"),
          value = input$distinct_limit %||% 4000,
          min = 4000, max = 100000, step = 1000
        ),
        lapply(seq_len(n), function(i) {
          fluidRow(
            if (i > 1)
              column(
                2,
                selectInput(
                  session$ns(paste0("logic_", i)),
                  tr("Logic"),
                  choices = setNames(c("AND", "OR"), c(tr("AND"), tr("OR")))
                )
              ),
            
            column(
              3,
              selectInput(
                session$ns(paste0("col_", i)),
                paste(tr("Field"), i),
                choices = {
                  if (!is.null(data_source())) {
                    nm <- data_source()$cols$name
                    nm[order(tolower(nm), nm, na.last = TRUE)]
                  } else character(0)
                }
              )
            ),
            
            column(3, uiOutput(session$ns(paste0("op_ui_", i)))),
            column(if (i > 1) 4 else 6, uiOutput(session$ns(paste0("val_ui_", i))))
          )
        }),
        
        fluidRow(
          column(6, actionButton(session$ns("apply"), tr("Apply"), class = "btn-primary")),
          column(6, actionButton(session$ns("reset"), tr("Reset")))
        )
      )
    })
    
    observe({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      ds <- data_source()
      if (is.null(ds)) return()
      req(input$n_filters)
      
      for (i in seq_len(input$n_filters)) {
        local({
          idx <- i
          output[[paste0("op_ui_", idx)]] <- renderUI({
            current_lang <- lang()
            req(current_lang)
            sync_i18n_lang(current_lang)
            
            ds <- data_source()
            req(ds)
            
            col <- input[[paste0("col_", idx)]]
            req(col)
            
            col_type <- ds$cols$type[match(col, ds$cols$name)]
            req(col_type)
            
            if (is_numeric_duckdb(col_type)) {
              selectInput(
                session$ns(paste0("op_", idx)),
                tr("Operator"),
                choices = setNames(
                  c("=", "!=", "<", "<=", ">", ">=", "between"),
                  c("=", "!=", "<", "<=", ">", ">=", tr("between"))
                )
              )
            } else {
              selectInput(
                session$ns(paste0("op_", idx)),
                tr("Operator"),
                choices = setNames(c("in", "not in"), c(tr("in"), tr("not in")))
              )
            }
          })
        })
      }
    })
    
    observe({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      ds <- data_source()
      if (is.null(ds)) return()
      req(input$n_filters)
      
      for (i in seq_len(input$n_filters)) {
        local({
          idx <- i
          output[[paste0("val_ui_", idx)]] <- renderUI({
            current_lang <- lang()
            req(current_lang)
            sync_i18n_lang(current_lang)
            
            ds <- data_source()
            req(ds)
            
            con <- ds$con
            tbl <- ds$table
            
            col <- input[[paste0("col_", idx)]]
            op  <- input[[paste0("op_", idx)]]
            req(col, op)
            
            col_type <- ds$cols$type[match(col, ds$cols$name)]
            req(col_type)
            
            col_sql <- sql_ident(con, col)
            
            if (is_numeric_duckdb(col_type)) {
              
              rng <- DBI::dbGetQuery(con, glue::glue("SELECT min({col_sql}) AS mn, max({col_sql}) AS mx FROM {tbl};"))
              mn <- rng$mn[[1]]
              mx <- rng$mx[[1]]
              
              if (is.null(mn) || is.null(mx) || is.na(mn) || is.na(mx)) {
                return(numericInput(session$ns(paste0("val_", idx)), tr("Value"), value = 0))
              }
              
              if (op == "between") {
                sliderInput(session$ns(paste0("val_", idx)), tr("Value"),
                            min = mn, max = mx, value = c(mn, mx))
              } else {
                med <- DBI::dbGetQuery(con, glue::glue("SELECT approx_quantile({col_sql}, 0.5) AS med FROM {tbl};"))$med[[1]]
                if (is.null(med) || is.na(med)) med <- mn
                numericInput(session$ns(paste0("val_", idx)), tr("Value"), value = med)
              }
              
            } else {
              
              vals <- DBI::dbGetQuery(con, glue::glue("
                SELECT DISTINCT {col_sql} AS v
                FROM {tbl}
                WHERE {col_sql} IS NOT NULL
                ORDER BY lower(CAST({col_sql} AS VARCHAR))
                LIMIT {limit_n()};
              "))
              
              vals$v <- vals$v[order(tolower(as.character(vals$v)), as.character(vals$v), na.last = TRUE)]
              
              selectizeInput(
                session$ns(paste0("val_", idx)),
                tr("Value (limited list; type to search)"),
                choices = vals$v,
                multiple = TRUE,
                options = list(placeholder = tr("Type to search…"))
              )
            }
          })
        })
      }
    })
    
    output$summary_ui <- renderUI({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      req(compute_result())
      tags$div(
        tags$strong(tr("Active filters: ")),
        tags$code(compute_result()$filter_summary)
      )
    })
    
    # Compute the filtered result ONLY when Apply is clicked (allows re-apply)
    compute_result <- eventReactive(input$apply, {
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      ds <- data_source()
      req(ds)
      req(input$n_filters)
      
      con <- ds$con
      tbl <- ds$table
      
      where_sql <- NULL
      summary_txt <- NULL
      
      for (i in seq_len(input$n_filters)) {
        col <- input[[paste0("col_", i)]]
        op  <- input[[paste0("op_",  i)]]
        val <- input[[paste0("val_", i)]]
        req(col, op, val)
        
        col_type <- ds$cols$type[match(col, ds$cols$name)]
        col_sql  <- sql_ident(con, col)
        
        clause <- if (is_numeric_duckdb(col_type)) {
          if (op == "between") glue::glue("{col_sql} BETWEEN {val[1]} AND {val[2]}")
          else glue::glue("{col_sql} {op} {val}")
        } else {
          # guard: forbid empty IN () which causes SQL error if user clears selection
          validate(need(length(val) > 0, tr("Choose at least one value.")))
          vals_sql <- paste(vapply(val, function(x) sql_lit(con, x), character(1)), collapse = ", ")
          if (op == "in") glue::glue("{col_sql} IN ({vals_sql})")
          else glue::glue("{col_sql} NOT IN ({vals_sql})")
        }
        
        pretty_val <- if (length(val) > 1) paste0("{", paste(val, collapse = ", "), "}") else as.character(val)
        one_summary <- paste(col, tr(op), pretty_val)
        
        if (i == 1) {
          where_sql <- as.character(clause)
          summary_txt <- paste0("(", one_summary, ")")
        } else {
          logic <- input[[paste0("logic_", i)]]
          where_sql <- as.character(glue::glue("({where_sql}) {logic} ({clause})"))
          summary_txt <- paste(summary_txt, tr(logic), paste0("(", one_summary, ")"))
        }
      }
      
      preview <- DBI::dbGetQuery(con, glue::glue("SELECT * FROM {tbl} WHERE {where_sql} LIMIT {PREVIEW_LIMIT};"))
      
      # Close the details panel (same behavior as before)
      session$sendCustomMessage("closeFilters", TRUE)
      
      list(where_sql = where_sql, filter_summary = summary_txt, preview = preview)
    }, ignoreInit = TRUE)
    
    # output$download_csv <- downloadHandler(
    #   filename = function() paste0("filtered_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    #   content = function(file) {
    #     ds <- data_source(); req(ds)
    #     res <- compute_result(); req(res)
    #     ...
    #     DBI::dbExecute(ds$con, glue::glue("
    #       COPY (SELECT * FROM {ds$table} WHERE {res$where_sql})
    #       TO '{out}' (HEADER, DELIMITER ',');
    #     "))
    #   }
    # )
    # 
    # output$download_xlsx <- downloadHandler(
    #   filename = function() paste0("filtered_preview_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    #   content = function(file) {
    #     res <- compute_result(); req(res)
    #     wb <- openxlsx::createWorkbook()
    #     openxlsx::addWorksheet(wb, "Preview")
    #     openxlsx::writeDataTable(wb, "Preview", res$preview)
    #     openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    #   }
    # )
    
    # observe({
    #   enabled <- !is.null(compute_result())
    #   shinyjs::toggleState(session$ns("download_csv"), enabled)
    #   shinyjs::toggleState(session$ns("download_xlsx"), enabled)
    # })
    
    observeEvent(input$reset, {
      updateNumericInput(session, "n_filters", value = 1)
      result(NULL)
    })
    
    reactive({
      req(compute_result())
      compute_result()$preview
    })
  })
}

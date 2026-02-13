# BandAid Table module.R
# i18n-safe: merge panel rendered ABOVE tabs via a separate UI output
# Table output stays in the Table tab.
# Includes outputOptions() set onFlushed to avoid "not in list" warnings.

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

# ---------------------------
# UI: merge panel only (place above tabs in app.R)
# ---------------------------
mod_table_merge_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("merge_ui"))
}

# ---------------------------
# UI: table + downloads only (place inside Table tab)
# ---------------------------
mod_table_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("table_ui"))
}

# ---------------------------
# SERVER
# ---------------------------
mod_table_server <- function(id, filtered_data, lang) {
  moduleServer(id, function(input, output, session) {
    
    tr <- get_tr()
    
    # Guarded sync so it never throws
    sync_i18n_lang <- function(current_lang) {
      if (exists("i18n", envir = .GlobalEnv, inherits = TRUE)) {
        i18n_obj <- get("i18n", envir = .GlobalEnv, inherits = TRUE)
        if (!is.null(i18n_obj) && is.function(i18n_obj$set_translation_language)) {
          langs <- tryCatch(i18n_obj$get_languages(), error = function(e) character(0))
          if (!is.null(current_lang) && nzchar(current_lang) && current_lang %in% langs) {
            i18n_obj$set_translation_language(current_lang)
          }
        }
      }
    }
    
    # ===== Helpers =====
    normalize_num <- function(x) as.numeric(gsub(",", ".", as.character(x)))
    
    read_any <- function(file) {
      ext <- tolower(tools::file_ext(file$name))
      if (ext == "csv") {
        read.csv(file$datapath, stringsAsFactors = FALSE)
      } else if (ext == "xlsx") {
        readxl::read_excel(file$datapath)
      } else {
        NULL
      }
    }
    
    strip_internal_cols <- function(df) {
      drop <- intersect(names(df), c("_lookups_applied", "_species_merged", "lat_norm", "lon_norm"))
      if (length(drop)) df <- df[, setdiff(names(df), drop), drop = FALSE]
      df
    }
    
    # ===== Status (persist across UI re-renders) =====
    stations_status_txt <- reactiveVal("")
    
    # ===== MERGE PANEL UI (collapsed by default) =====
    output$merge_ui <- renderUI({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      ns <- session$ns
      
      # IMPORTANT: omit the `open` attribute entirely so <details> starts collapsed.
      tags$details(
        tags$summary(tags$strong(tr("Add Station Names"))),
        br(),
        
        # =====================
        # STATIONS MERGE (always-on)
        # =====================
        h4(tr("Stations")),
        
        div(
          style = "margin-left: 12px;",
          
          fileInput(
            ns("stations_file"),
            tr("Upload stations file (CSV or Excel)"),
            accept = c(".csv", ".xlsx")
          ),
          
          selectInput(
            ns("stations_lat"),
            tr("Latitude field (stations)"),
            choices = NULL
          ),
          
          selectInput(
            ns("stations_long"),
            tr("Longitude field (stations)"),
            choices = NULL
          ),
          
          br(),
          tags$small(tags$em(
            tr("The merge runs automatically whenever the station file or the selections change.")
          )),
          br(), br(),
          textOutput(ns("stations_status"))
        )
      )
    })
    
    # ===== TABLE UI (datatable + downloads) =====
    output$table_ui <- renderUI({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      ns <- session$ns
      
      tagList(
        DT::DTOutput(ns("table")),
        hr(),
        fluidRow(
          column(6, downloadButton(ns("download_csv"),  tr("Download CSV"))),
          column(6, downloadButton(ns("download_xlsx"), tr("Download Excel")))
        )
      )
    })
    
    # ===== Read stations file =====
    stations_data <- reactive({
      req(input$stations_file)
      read_any(input$stations_file)
    })
    
    # Auto-fill likely lat/long field names when the file changes
    observeEvent(stations_data(), {
      st <- stations_data()
      req(st)
      
      st_names <- names(st)
      lat_guess <- c("Latitude", "LATITUDE", "Lat", "LAT", "Y", "y")
      lon_guess <- c("Longitude", "LONGITUDE", "Long", "LON", "Lon", "X", "x")
      
      updateSelectInput(session, "stations_lat",
                        choices = st_names,
                        selected = {
                          hit <- lat_guess[lat_guess %in% st_names]
                          if (length(hit)) hit[1] else st_names[1]
                        }
      )
      updateSelectInput(session, "stations_long",
                        choices = st_names,
                        selected = {
                          hit <- lon_guess[lon_guess %in% st_names]
                          if (length(hit)) hit[1] else st_names[min(2, length(st_names))]
                        }
      )
    })
    
    # ===== Always-on STATIONS MERGE (reactive) =====
    merged_stations <- reactive({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      base <- filtered_data()
      req(base)
      
      # If no station file provided, just pass through
      if (is.null(input$stations_file)) {
        stations_status_txt("")
        return(base)
      }
      
      st <- stations_data()
      req(st)
      
      # Validate base has coords
      if (!all(c("GISBLat", "GISBLong") %in% names(base))) {
        stations_status_txt(paste0("❌ ", tr("Base data missing GISBLat / GISBLong")))
        return(base)
      }
      
      # Validate station lat/long picks
      lat_col <- input$stations_lat
      lon_col <- input$stations_long
      if (!length(lat_col) || !length(lon_col) || !(lat_col %in% names(st)) || !(lon_col %in% names(st))) {
        stations_status_txt(paste0("ℹ️ ", tr("Select station latitude/longitude fields to merge.")))
        return(base)
      }
      
      # Normalize numerics for a stable join
      base$lat_norm <- normalize_num(base$GISBLat)
      base$lon_norm <- normalize_num(base$GISBLong)
      
      st$lat_norm <- normalize_num(st[[lat_col]])
      st$lon_norm <- normalize_num(st[[lon_col]])
      
      merged <- merge(
        base,
        st,
        by = c("lat_norm", "lon_norm"),
        all.x = TRUE
      )
      merged[ c(lat_col, lon_col) ] <- NULL
      
      stations_status_txt(
        paste0("✅ ", tr("Stations merge successful"), " (", nrow(merged), " ", tr("rows"), ")")
      )
      
      merged
    })
    
    # ===== Final data (station-only, internal columns stripped) =====
    final_data <- reactive({
      out <- merged_stations()
      strip_internal_cols(out)
    })
    
    # ===== Table =====
    output$table <- DT::renderDT({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      df <- final_data()
      req(df)
      
      DT::datatable(
        df,
        options = list(scrollX = TRUE, pageLength = 25)
      )
    })
    
    # ===== Downloads (strip internal columns just in case) =====
    output$download_csv <- downloadHandler(
      filename = function() paste0("table_", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(strip_internal_cols(final_data()), file, row.names = FALSE)
      }
    )
    
    output$download_xlsx <- downloadHandler(
      filename = function() paste0("table_", Sys.Date(), ".xlsx"),
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Data")
        openxlsx::writeDataTable(wb, "Data", strip_internal_cols(final_data()))
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Prevent suspension issues
    session$onFlushed(function() {
      outputOptions(output, "merge_ui",        suspendWhenHidden = FALSE)
      outputOptions(output, "table_ui",        suspendWhenHidden = FALSE)
      outputOptions(output, "table",           suspendWhenHidden = FALSE)
    }, once = TRUE)
    
    return(final_data)
  })
}
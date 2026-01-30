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
    
    # ===== Status (persist across UI re-renders) =====
    species_status_txt  <- reactiveVal("")
    stations_status_txt <- reactiveVal("")
    
    # ===== MERGE PANEL UI (collapsed by default) =====
    output$merge_ui <- renderUI({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      ns <- session$ns
      
      # IMPORTANT: omit the `open` attribute entirely so <details> starts collapsed.
      tags$details(
        tags$summary(tags$strong(tr("Add Species and Station Names"))),
        br(),
        
        # =====================
        # SPECIES MERGE
        # =====================
        h4(tr("Species")),
        
        checkboxInput(
          ns("merge_species"),
          tr("Merge with species file"),
          value = FALSE
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("merge_species")),
          div(
            style = "margin-left: 12px;",
            
            fileInput(
              ns("species_file"),
              tr("Upload species file (CSV or Excel)"),
              accept = c(".csv", ".xlsx")
            ),
            
            selectInput(
              ns("base_species_join"),
              tr("Base join field (base data)"),
              choices = NULL
            ),
            
            selectInput(
              ns("species_join"),
              tr("Species join field (species file)"),
              choices = NULL
            ),
            
            actionButton(
              ns("apply_species"),
              tr("Apply species merge"),
              class = "btn-primary"
            ),
            
            br(), br(),
            textOutput(ns("species_status"))
          )
        ),
        
        hr(),
        
        # =====================
        # STATIONS MERGE
        # =====================
        h4(tr("Stations")),
        
        checkboxInput(
          ns("merge_stations"),
          tr("Merge with station file"),
          value = FALSE
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("merge_stations")),
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
            
            actionButton(
              ns("apply_stations"),
              tr("Apply stations merge"),
              class = "btn-primary"
            ),
            
            br(), br(),
            textOutput(ns("stations_status"))
          )
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
          column(6, downloadButton(ns("download_csv"), tr("Download CSV"))),
          column(6, downloadButton(ns("download_xlsx"), tr("Download Excel")))
        )
      )
    })
    
    # ===== Read files =====
    species_data <- reactive({
      req(input$species_file)
      read_any(input$species_file)
    })
    
    stations_data <- reactive({
      req(input$stations_file)
      read_any(input$stations_file)
    })
    
    # ===== Update dropdowns =====
    observeEvent(filtered_data(), {
      df <- filtered_data()
      req(df)
      
      updateSelectInput(
        session,
        "base_species_join",
        choices = names(df),
        selected = {
          prefs <- c("Sp..Num.", "Sp.Num.", "SpNum", "Sp_Num", "SpeciesID", "Species_Id")
          hit <- prefs[prefs %in% names(df)]
          if (length(hit) > 0) hit[1] else names(df)[1]
        }
      )
    })
    
    observeEvent(species_data(), {
      sp <- species_data()
      req(sp)
      
      updateSelectInput(
        session,
        "species_join",
        choices = names(sp),
        selected = {
          prefs <- c("Sp..Num.", "Sp.Num.", "SpNum", "Sp_Num", "SpeciesID", "Species_Id")
          hit <- prefs[prefs %in% names(sp)]
          if (length(hit) > 0) hit[1] else names(sp)[1]
        }
      )
    })
    
    observeEvent(stations_data(), {
      st <- stations_data()
      req(st)
      updateSelectInput(session, "stations_lat",  choices = names(st))
      updateSelectInput(session, "stations_long", choices = names(st))
    })
    
    merged_species  <- reactiveVal(NULL)
    merged_stations <- reactiveVal(NULL)
    
    # ===== SPECIES MERGE =====
    observeEvent(input$apply_species, {
      
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      req(filtered_data(), species_data(), input$base_species_join, input$species_join)
      
      df <- filtered_data()
      sp <- species_data()
      
      base_key <- input$base_species_join
      sp_key   <- input$species_join
      
      if (!(base_key %in% names(df))) {
        species_status_txt(paste0("❌ ", tr("Base data missing selected join field:"), " ", base_key))
        return()
      }
      
      if (!(sp_key %in% names(sp))) {
        species_status_txt(paste0("❌ ", tr("Species file missing selected join field:"), " ", sp_key))
        return()
      }
      
      required_sp_cols <- c("English_Name", "French_Name")
      if (!all(required_sp_cols %in% names(sp))) {
        species_status_txt(
          paste0("❌ ", tr("Species file must contain:"), " ", paste(required_sp_cols, collapse = ", "))
        )
        return()
      }
      
      merged <- merge(
        df,
        sp[, c(sp_key, "English_Name", "French_Name")],
        by.x = base_key,
        by.y = sp_key,
        all.x = TRUE
      )
      
      merged_species(merged)
      
      species_status_txt(
        paste0(
          "✅ ",
          tr("Species merge successful"),
          " (", nrow(merged), " ", tr("rows"), ") ",
          tr("using base"), "=", base_key, " ↔ ",
          tr("species"), "=", sp_key
        )
      )
    })
    
    # ===== STATIONS MERGE =====
    observeEvent(input$apply_stations, {
      
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      base <- if (!is.null(merged_species())) merged_species() else filtered_data()
      st   <- stations_data()
      req(base, st, input$stations_lat, input$stations_long)
      
      if (!all(c("GISBLat", "GISBLong") %in% names(base))) {
        stations_status_txt(paste0("❌ ", tr("Base data missing GISBLat / GISBLong")))
        return()
      }
      
      base$lat_norm <- normalize_num(base$GISBLat)
      base$lon_norm <- normalize_num(base$GISBLong)
      
      st$lat_norm <- normalize_num(st[[input$stations_lat]])
      st$lon_norm <- normalize_num(st[[input$stations_long]])
      
      merged <- merge(
        base,
        st,
        by = c("lat_norm", "lon_norm"),
        all.x = TRUE
      )
      
      merged_stations(merged)
      
      stations_status_txt(
        paste0("✅ ", tr("Stations merge successful"), " (", nrow(merged), " ", tr("rows"), ")")
      )
    })
    
    # ===== Status outputs =====
    output$species_status <- renderText({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      species_status_txt()
    })
    
    output$stations_status <- renderText({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      stations_status_txt()
    })
    
    # ===== Final data =====
    final_data <- reactive({
      if (isTRUE(input$merge_stations) && !is.null(merged_stations())) {
        merged_stations()
      } else if (isTRUE(input$merge_species) && !is.null(merged_species())) {
        merged_species()
      } else {
        filtered_data()
      }
    })
    
    # ===== Table =====
    output$table <- DT::renderDT({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      req(final_data())
      DT::datatable(
        final_data(),
        options = list(scrollX = TRUE, pageLength = 25)
      )
    })
    
    # ===== Downloads =====
    output$download_csv <- downloadHandler(
      filename = function() paste0("table_", Sys.Date(), ".csv"),
      content = function(file) write.csv(final_data(), file, row.names = FALSE)
    )
    
    output$download_xlsx <- downloadHandler(
      filename = function() paste0("table_", Sys.Date(), ".xlsx"),
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Data")
        openxlsx::writeDataTable(wb, "Data", final_data())
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # Prevent suspension issues
    session$onFlushed(function() {
      outputOptions(output, "merge_ui",        suspendWhenHidden = FALSE)
      outputOptions(output, "table_ui",        suspendWhenHidden = FALSE)
      outputOptions(output, "table",           suspendWhenHidden = FALSE)
      outputOptions(output, "species_status",  suspendWhenHidden = FALSE)
      outputOptions(output, "stations_status", suspendWhenHidden = FALSE)
    }, once = TRUE)
    
    return(final_data)
  })
}

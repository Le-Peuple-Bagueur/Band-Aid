# =========================================================
# BandAid Plot module.R
# i18n-hardened + French species names in checkbox + legend + draggable draw order (French-only)
# Station marker mode: Centroids vs Most recent (based on B.Day/B.Month/B.Year)
# Export uses the same station mode.
# Uses stable tab ID: active_tab() == "map"
#
# ADDED (as requested):
# - Species list sorted alphabetically by current language label (EN/FR)
# - Station selector includes "No station" category (blank/NA station names)
# - Species + station selectors interact (AND) to filter encounter markers
# - "No station" is selectable but does NOT produce a station marker
# =========================================================

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

# ---- webshot/chrome helpers ----
ensure_chromote_chrome <- function() {
  # If already set and exists, keep it
  cur <- Sys.getenv("CHROMOTE_CHROME", "")
  if (nzchar(cur) && file.exists(cur)) return(cur)
  
  candidates <- c(
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit)) {
    Sys.setenv(CHROMOTE_CHROME = hit[[1]])
    return(hit[[1]])
  }
  # Nothing found; let chromote try its own discovery
  return(NULL)
}

win_normpath <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)



# =========================================================
# UI
# =========================================================
mod_plot_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("plot_ui"))
}

# =========================================================
# SERVER
# =========================================================
mod_plot_server <- function(id, final_data, active_tab, lang) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$download_map_jpeg, {
      message("[Export] download_map_jpeg clicked at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    }, ignoreInit = TRUE)
    
    # Minimal 1x1 JPEG writer (absolute last-resort fallback)
    .write_placeholder_jpeg <- function(path) {
      path <- normalizePath(path, winslash = "/", mustWork = FALSE)
      grDevices::jpeg(filename = path, width = 2, height = 2, units = "px", quality = 90)
      plot.new(); grDevices::dev.off()
      invisible(TRUE)
    }
    
    tr <- get_tr()
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    
    # Guarded sync: never throws
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
    
    MAX_POINTS <- 100000
    to_num <- function(x) as.numeric(gsub(",", ".", as.character(x)))
    
    # Web Mercator approximation
    meters_per_pixel <- function(lat_deg, zoom) {
      156543.03392 * cos(lat_deg * pi / 180) / (2^zoom)
    }
    
    # More-contrast palette
    get_contrast_colors <- function(n) {
      if (requireNamespace("viridisLite", quietly = TRUE)) {
        viridisLite::turbo(n)
      } else {
        grDevices::hcl.colors(n, palette = "Dark 3")
      }
    }
    
    # Internal ID for missing/blank station
    NO_STATION_ID <- "__NO_STATION__"
    
    # -----------------------------------------------------
    # Render full UI on lang() changes (robust i18n)
    # -----------------------------------------------------
    output$plot_ui <- renderUI({
      current_lang <- lang()
      req(current_lang)
      sync_i18n_lang(current_lang)
      
      ns <- session$ns
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          tags$style(HTML(paste0("
            #", ns("sidebar_root"), " .shiny-input-checkboxgroup .shiny-options-group {
              max-height: 240px;
              overflow-y: auto;
              overflow-x: hidden;
              border: 1px solid #e5e5e5;
              padding: 6px;
              border-radius: 4px;
              background: #fff;
            }
            #", ns("sidebar_root"), " details summary { cursor: pointer; margin-bottom: 6px; }

            #", ns("sidebar_root"), " .jqui-orderInput {
              width: 100% !important;
              max-width: 100% !important;
              max-height: 240px;
              overflow-y: auto;
              overflow-x: hidden !important;
              border: 1px solid #e5e5e5;
              padding: 6px;
              border-radius: 4px;
              background: #fff;
              box-sizing: border-box;
            }

            #", ns("sidebar_root"), " .jqui-orderInput ul,
            #", ns("sidebar_root"), " .jqui-orderInput ol {
              padding-left: 0 !important;
              margin: 0 !important;
            }

            #", ns("sidebar_root"), " .jqui-orderInput li,
            #", ns("sidebar_root"), " .jqui-orderInput .ui-sortable-handle,
            #", ns("sidebar_root"), " .jqui-orderInput .list-group-item {
              display: block !important;
              width: 100% !important;
              max-width: 100% !important;
              box-sizing: border-box !important;
              white-space: nowrap !important;
              overflow: hidden !important;
              text-overflow: ellipsis !important;
            }

            #", ns("sidebar_root"), " .btn { width: 100%; }
          "))),
          
          
          div(
            id = ns("sidebar_root"),
            
            h4(tr("Species")),
            uiOutput(ns("species_selector")),
            fluidRow(
              column(6, actionButton(ns("species_all"), tr("Select all"))),
              column(6, actionButton(ns("species_none"), tr("Select none")))
            ),
            tags$details(
              open = FALSE,
              tags$summary(tags$strong(tr("Species draw order"))),
              uiOutput(ns("species_order_ui"))
            ),
            
            hr(),
            
            h4(tr("Stations")),
            uiOutput(ns("station_selector")),
            
            # Station marker mode control
            radioButtons(
              ns("station_mode"),
              label = tr("Station markers"),
              choices = setNames(
                c("centroid", "recent"),
                c(tr("Centroids"), tr("Most recent"))
              ),
              selected = "centroid",
              inline = TRUE
            ),
            
            fluidRow(
              column(6, actionButton(ns("station_all"), tr("Select all"))),
              column(6, actionButton(ns("station_none"), tr("Select none")))
            ),
            tags$details(
              open = FALSE,
              tags$summary(tags$strong(tr("Station draw order"))),
              uiOutput(ns("station_order_ui"))
            ),
            
            hr(),
            
            selectInput(
              ns("export_size"),
              tr("Export size"),
              choices = setNames(
                c("match", "1080p", "4k"),
                c(tr("Match app"), "1080p", "4K")
              ),
              selected = "match"
            ),
            
            downloadButton(ns("download_map_jpeg"), tr("Download map (JPEG)"))
          )
        ),
        
        mainPanel(
          width = 9,
          
          tags$style(HTML(paste0("
            .top-controls-", ns("map"), " .shiny-input-container { margin-bottom: 6px; }
            .jitter-wrap-", ns("map"), " { width: 100%; }
            .jitter-header-", ns("map"), " {
              display: flex;
              align-items: center;
              justify-content: space-between;
              gap: 10px;
              width: 100%;
              margin-bottom: 4px;
            }
            .jitter-header-", ns("map"), " .jitter-title { font-weight: 600; white-space: nowrap; }
            .jitter-header-", ns("map"), " .shiny-input-container { margin: 0 !important; }

            .leaflet-control.leaflet-legend, .leaflet-control .legend {
              background: #ffffff !important;
              opacity: 1 !important;
            }
          "))),
          
          
          div(
            class = paste0("top-controls-", ns("map")),
            fluidRow(
              column(4, sliderInput(ns("marker_size"), tr("Marker size"), min = 1, max = 8, value = 3)),
              column(
                4,
                tags$div(
                  class = paste0("jitter-wrap-", ns("map")),
                  tags$div(
                    class = paste0("jitter-header-", ns("map")),
                    tags$span(class = "jitter-title", tr("Jitter overlapping points")),
                    bslib::input_switch(ns("jitter_on"), label = NULL, value = FALSE)
                  ),
                  sliderInput(ns("jitter_strength"), tr("Jitter strength"), min = 0, max = 3, value = 1, step = 0.25)
                )
              ),
              column(4, sliderInput(ns("jitter_px_max"), tr("Max jitter (pixels)"), min = 0, max = 30, value = 10, step = 1))
            )
          ),
          
          leaflet::leafletOutput(ns("map"), height = "calc(100vh - 170px)"),
          
          # capture map size for export
          tags$script(HTML(sprintf("
            (function() {
              function updateMapSize() {
                var el = document.getElementById('%s');
                if (!el) return;
                var w = el.clientWidth || 0;
                var h = el.clientHeight || 0;
                if (w > 0 && h > 0) {
                  Shiny.setInputValue('%s', {w: w, h: h}, {priority: 'event'});
                }
              }
              document.addEventListener('DOMContentLoaded', function() { setTimeout(updateMapSize, 700); });
              window.addEventListener('resize', updateMapSize);
              setInterval(updateMapSize, 2000);
            })();
          ", ns("map"), ns("map_dim"))))
        )
      )
    })
    
    # Keep outputs active even if tab hidden
    session$onFlushed(function() {
      outputOptions(output, "plot_ui",          suspendWhenHidden = FALSE)
      outputOptions(output, "species_selector", suspendWhenHidden = FALSE)
      outputOptions(output, "station_selector", suspendWhenHidden = FALSE)
      outputOptions(output, "species_order_ui", suspendWhenHidden = FALSE)
      outputOptions(output, "station_order_ui", suspendWhenHidden = FALSE)
      outputOptions(output, "map",              suspendWhenHidden = FALSE)
      outputOptions(output, "download_map_jpeg", suspendWhenHidden = FALSE)
    }, once = TRUE)
    
    # -----------------------------------------------------
    # Data prep
    # -----------------------------------------------------
    stations_ready <- reactive({
      df <- final_data()
      if (is.null(df)) return(FALSE)
      all(c("station", "GISBLat", "GISBLong") %in% names(df))
    })
    
    view_locked <- reactiveVal(FALSE)
    observeEvent(list(input$map_center, input$map_zoom), {
      if (!is.null(input$map_center) && !is.null(input$map_zoom)) view_locked(TRUE)
    }, ignoreInit = TRUE)
    
    plot_data <- reactive({
      req(active_tab() == "map")
      req(final_data())
      
      current_lang <- lang(); req(current_lang)
      sync_i18n_lang(current_lang)
      
      df <- final_data()
      required <- c("GISRLat", "GISRLong", "English_Name")
      validate(need(all(required %in% names(df)),
                    paste(tr("Missing required fields:"), paste(setdiff(required, names(df)), collapse = ", "))))
      
      df$GISRLat  <- to_num(df$GISRLat)
      df$GISRLong <- to_num(df$GISRLong)
      
      if (all(c("GISBLat", "GISBLong") %in% names(df))) {
        df$GISBLat  <- to_num(df$GISBLat)
        df$GISBLong <- to_num(df$GISBLong)
      }
      
      if ("station" %in% names(df)) {
        df$station <- as.character(df$station)
        df$station_id <- ifelse(is.na(df$station) | trimws(df$station) == "", NO_STATION_ID, df$station)
      } else {
        df$station_id <- NA_character_
      }
      
      df <- df |> dplyr::filter(!is.na(GISRLat), !is.na(GISRLong))
      if (nrow(df) > MAX_POINTS) df <- df[sample.int(nrow(df), MAX_POINTS), , drop = FALSE]
      df
    })
    
    # -----------------------------------------------------
    # Species labels (French/English)
    # -----------------------------------------------------
    make_unique_labels <- function(x) {
      ave_idx <- ave(seq_along(x), x, FUN = seq_along)
      ifelse(ave_idx == 1, x, paste0(x, " (", ave_idx, ")"))
    }
    
    species_labels <- reactive({
      df <- plot_data(); req(df)
      ids <- sort(unique(as.character(df$English_Name)))
      lab_en <- setNames(ids, ids)
      
      if ("French_Name" %in% names(df)) {
        tmp <- df |> dplyr::select(English_Name, French_Name) |> dplyr::distinct()
        tmp$English_Name <- as.character(tmp$English_Name)
        tmp$French_Name  <- as.character(tmp$French_Name)
        tmp$French_Name[is.na(tmp$French_Name) | tmp$French_Name == ""] <- tmp$English_Name[is.na(tmp$French_Name) | tmp$French_Name == ""]
        lab_fr <- tmp$French_Name
        names(lab_fr) <- tmp$English_Name
      } else {
        lab_fr <- lab_en
      }
      
      fr_vec <- unname(lab_fr[ids])
      fr_vec[is.na(fr_vec) | fr_vec == ""] <- ids[is.na(fr_vec) | fr_vec == ""]
      fr_unique <- make_unique_labels(fr_vec)
      lab_fr_unique <- setNames(fr_unique, ids)
      
      list(ids = ids, lab_en = lab_en, lab_fr = lab_fr_unique)
    })
    
    current_species_label_vec <- reactive({
      sp <- species_labels()
      if (identical(lang(), "fr")) sp$lab_fr else sp$lab_en
    })
    
    # Language-based alphabetical species ordering (you already confirmed this worked)
    species_ids_sorted <- reactive({
      sp <- species_labels()
      ids <- sp$ids
      disp <- current_species_label_vec()
      lbl <- unname(disp[ids])
      lbl[is.na(lbl) | lbl == ""] <- ids[is.na(lbl) | lbl == ""]
      ids[order(tolower(lbl), lbl)]
    })
    
    # Species selector
    output$species_selector <- renderUI({
      req(lang()); sync_i18n_lang(lang())
      ids <- species_ids_sorted()
      disp <- current_species_label_vec()
      choices <- setNames(ids, unname(disp[ids]))
      checkboxGroupInput(session$ns("species_selected"), label = NULL, choices = choices, selected = ids)
    })
    
    observeEvent(input$species_all, {
      ids <- species_ids_sorted()
      updateCheckboxGroupInput(session, "species_selected", selected = ids)
    })
    observeEvent(input$species_none, {
      updateCheckboxGroupInput(session, "species_selected", selected = character(0))
    })
    
    # -----------------------------------------------------
    # Stations selector with "No station" option (but no marker for it)
    # -----------------------------------------------------
    output$station_selector <- renderUI({
      req(lang()); sync_i18n_lang(lang())
      df <- plot_data()
      
      if (!stations_ready()) {
        return(tags$div(tags$small(tags$em(tr("Stations not shown (no station merge applied).")))))
      }
      
      st_ids <- sort(unique(df$station_id))
      st_ids <- st_ids[!is.na(st_ids)]
      
      labels <- st_ids
      labels[st_ids == NO_STATION_ID] <- tr("No station")
      
      choices <- setNames(st_ids, labels)
      
      checkboxGroupInput(session$ns("station_selected"), label = NULL, choices = choices, selected = st_ids)
    })
    
    observeEvent(input$station_all, {
      df <- plot_data(); if (!stations_ready()) return()
      st_ids <- sort(unique(df$station_id))
      st_ids <- st_ids[!is.na(st_ids)]
      updateCheckboxGroupInput(session, "station_selected", selected = st_ids)
    })
    observeEvent(input$station_none, {
      if (!stations_ready()) return()
      updateCheckboxGroupInput(session, "station_selected", selected = character(0))
    })
    
    # -----------------------------------------------------
    # Species order IDs (default = sorted list)
    # -----------------------------------------------------
    species_order_ids <- reactiveVal(NULL)
    
    observeEvent(species_labels(), {
      sp <- species_labels()
      cur <- species_order_ids()
      if (is.null(cur) || length(cur) == 0) {
        species_order_ids(species_ids_sorted())
      } else {
        keep <- intersect(cur, sp$ids)
        add  <- setdiff(sp$ids, keep)
        species_order_ids(c(keep, add))
      }
    }, ignoreInit = TRUE)
    
    observeEvent(input$species_selected, {
      sp <- species_labels()
      sel <- input$species_selected %||% sp$ids
      cur <- species_order_ids() %||% character(0)
      keep <- intersect(cur, sel)
      add  <- setdiff(sel, keep)
      species_order_ids(c(keep, add))
    }, ignoreInit = TRUE)
    
    output$species_order_ui <- renderUI({
      req(lang()); sync_i18n_lang(lang())
      sp <- species_labels()
      ids <- sp$ids
      sel <- input$species_selected %||% ids
      cur_ids <- species_order_ids() %||% sel
      cur_ids <- unique(c(intersect(cur_ids, sel), setdiff(sel, cur_ids)))
      disp_map <- current_species_label_vec()
      items_disp <- unname(disp_map[cur_ids])
      shinyjqui::orderInput(session$ns("species_order"), tr("Draw order (top → bottom)"), items = items_disp)
    })
    
    observeEvent(input$species_order, {
      req(lang()); sync_i18n_lang(lang())
      sp <- species_labels()
      sel <- input$species_selected %||% sp$ids
      disp_map <- current_species_label_vec()
      inv <- setNames(names(disp_map), unname(disp_map))  # display -> id
      
      ordered_ids <- unname(inv[input$species_order])
      ordered_ids <- ordered_ids[!is.na(ordered_ids)]
      ordered_ids <- intersect(ordered_ids, sel)
      ordered_ids <- unique(c(ordered_ids, setdiff(sel, ordered_ids)))
      species_order_ids(ordered_ids)
    }, ignoreInit = TRUE)
    
    # Station draw order UI (kept simple; uses station_id strings)
    output$station_order_ui <- renderUI({
      req(lang()); sync_i18n_lang(lang())
      df <- plot_data()
      if (!stations_ready()) return(NULL)
      
      all_st <- sort(unique(df$station_id))
      all_st <- all_st[!is.na(all_st)]
      sel <- unique(input$station_selected %||% all_st)
      cur <- input$station_order %||% character(0)
      items <- unique(c(intersect(cur, sel), setdiff(sel, cur)))
      
      # show friendly label for No station
      items_disp <- items
      items_disp[items_disp == NO_STATION_ID] <- tr("No station")
      
      shinyjqui::orderInput(session$ns("station_order"), tr("Draw order (top → bottom)"), items = items_disp)
    })
    
    # -----------------------------------------------------
    # Station points builder (mode dependent)
    # Uses station_id, and EXCLUDES NO_STATION_ID so it never plots a marker for that category.
    # -----------------------------------------------------
    station_points <- function(df, station_ids, mode) {
      
      station_ids <- setdiff(station_ids, NO_STATION_ID)
      if (length(station_ids) == 0) {
        return(df[0, c("station_id", "GISBLat", "GISBLong"), drop = FALSE])
      }
      
      df2 <- df |>
        dplyr::filter(!is.na(GISBLat), !is.na(GISBLong)) |>
        dplyr::filter(station_id %in% station_ids)
      
      if (nrow(df2) == 0) return(df2[0, c("station_id", "GISBLat", "GISBLong"), drop = FALSE])
      
      if (identical(mode, "recent")) {
        needed <- c("B.Year", "B.Month", "B.Day")
        if (all(needed %in% names(df2))) {
          y <- suppressWarnings(as.integer(df2[["B.Year"]]))
          m <- suppressWarnings(as.integer(df2[["B.Month"]]))
          d <- suppressWarnings(as.integer(df2[["B.Day"]]))
          dt <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d)))
          df2$.dt <- dt
          
          recent <- df2 |>
            dplyr::filter(!is.na(.dt)) |>
            dplyr::group_by(station_id) |>
            dplyr::slice_max(order_by = .dt, n = 1, with_ties = FALSE) |>
            dplyr::ungroup() |>
            dplyr::select(station_id, GISBLat, GISBLong)
          
          missing <- setdiff(station_ids, recent$station_id)
          if (length(missing) > 0) {
            cent <- df2 |>
              dplyr::filter(station_id %in% missing) |>
              dplyr::group_by(station_id) |>
              dplyr::summarise(
                GISBLat  = mean(GISBLat,  na.rm = TRUE),
                GISBLong = mean(GISBLong, na.rm = TRUE),
                .groups = "drop"
              )
            return(dplyr::bind_rows(recent, cent))
          }
          return(recent)
        }
        mode <- "centroid"
      }
      
      df2 |>
        dplyr::group_by(station_id) |>
        dplyr::summarise(
          GISBLat  = mean(GISBLat,  na.rm = TRUE),
          GISBLong = mean(GISBLong, na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    # Initial map
    output$map <- leaflet::renderLeaflet({
      req(active_tab() == "map")
      leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap)
    })
    
    # Debounce high-frequency inputs
    marker_size_r     <- shiny::debounce(reactive(input$marker_size), 120)
    jitter_strength_r <- shiny::debounce(reactive(input$jitter_strength), 120)
    jitter_px_r       <- shiny::debounce(reactive(input$jitter_px_max), 120)
    
    # -----------------------------------------------------
    # Map updates
    # IMPORTANT: Species + station selectors INTERACT (AND)
    # Encounter points are filtered by BOTH species and selected station_id when stations_ready().
    # -----------------------------------------------------
    observeEvent(
      list(
        plot_data(),
        input$species_selected,
        species_order_ids(),
        input$jitter_on,
        marker_size_r(),
        jitter_strength_r(),
        jitter_px_r(),
        input$station_selected,
        input$station_mode,
        lang()
      ),
      {
        req(active_tab() == "map")
        req(lang()); sync_i18n_lang(lang())
        df <- plot_data(); req(df)
        
        sp <- species_labels()
        ids <- sp$ids
        sp_selected <- input$species_selected %||% ids
        
        sp_order <- species_order_ids() %||% sp_selected
        sp_order <- intersect(sp_order, sp_selected)
        if (length(sp_order) == 0) sp_order <- sp_selected
        
        # Encounter points: species AND station selection (if station merge exists)
        if (stations_ready()) {
          st_levels <- sort(unique(df$station_id))
          st_levels <- st_levels[!is.na(st_levels)]
          st_selected <- input$station_selected %||% st_levels
          
          vp <- df |> dplyr::filter(English_Name %in% sp_selected, station_id %in% st_selected)
        } else {
          vp <- df |> dplyr::filter(English_Name %in% sp_selected)
          st_selected <- NULL
        }
        
        cols <- get_contrast_colors(length(ids))
        names(cols) <- ids
        pal_fun <- leaflet::colorFactor(palette = cols, domain = ids, na.color = "#808080")
        
        # Jitter
        vp$GISRLat_j  <- vp$GISRLat
        vp$GISRLong_j <- vp$GISRLong
        
        if (isTRUE(input$jitter_on) &&
            nrow(vp) > 1 &&
            (jitter_strength_r() %||% 0) > 0 &&
            (jitter_px_r() %||% 0) > 0) {
          
          z <- isolate(input$map_zoom %||% 8)
          lat_ref <- isolate(if (!is.null(input$map_center) && !is.null(input$map_center$lat))
            input$map_center$lat else mean(vp$GISRLat, na.rm = TRUE))
          
          marker_radius_px <- marker_size_r() %||% 3
          marker_diam_px <- (2 * marker_radius_px) + 2
          strength <- jitter_strength_r() %||% 1
          mpp <- meters_per_pixel(lat_ref, z)
          
          desired_px <- marker_diam_px * strength
          jitter_px <- min(desired_px, jitter_px_r())
          jitter_m <- jitter_px * mpp
          
          dlat <- jitter_m / 111320
          cosref <- cos(lat_ref * pi / 180)
          if (!is.finite(cosref) || abs(cosref) < 1e-8) cosref <- 1e-8
          dlng <- jitter_m / (111320 * cosref)
          if (!is.finite(dlng) || dlng <= 0) dlng <- dlat
          
          gx <- floor(vp$GISRLong / dlng)
          gy <- floor(vp$GISRLat / dlat)
          cell <- paste(gx, gy, sep = "_")
          tab <- table(cell)
          overlap_cells <- names(tab)[tab > 1]
          idx <- which(cell %in% overlap_cells)
          
          if (length(idx) > 0) {
            base_seed <- (sum(utf8ToInt(session$token)) + as.integer(z) * 1000 + marker_radius_px * 10) %% .Machine$integer.max
            set.seed(base_seed)
            
            theta <- runif(length(idx), 0, 2*pi)
            r <- sqrt(runif(length(idx), 0, 1)) * jitter_m
            dlat_i <- (r / 111320) * sin(theta)
            
            coslat <- cos(vp$GISRLat[idx] * pi / 180)
            coslat[is.na(coslat) | abs(coslat) < 1e-8] <- 1e-8
            dlng_i <- (r * cos(theta)) / (111320 * coslat)
            
            vp$GISRLat_j[idx]  <- vp$GISRLat[idx] + dlat_i
            vp$GISRLong_j[idx] <- vp$GISRLong[idx] + dlng_i
          }
        }
        
        vp <- vp[order(factor(vp$English_Name, levels = sp_order)), , drop = FALSE]
        vp$col <- pal_fun(vp$English_Name)
        
        proxy <- leaflet::leafletProxy("map", session) |>
          leaflet::clearMarkers() |>
          leaflet::clearControls()
        
        # Encounter markers
        proxy <- proxy |>
          leaflet::addCircleMarkers(
            data = vp,
            lng = ~GISRLong_j,
            lat = ~GISRLat_j,
            radius = marker_size_r(),
            color = ~col,
            fillOpacity = 0.85,
            stroke = FALSE
          )
        
        # Station markers (exclude "No station")
        if (stations_ready()) {
          st_levels <- sort(unique(df$station_id))
          st_levels <- st_levels[!is.na(st_levels)]
          st_selected2 <- input$station_selected %||% st_levels
          
          mode <- input$station_mode %||% "centroid"
          st_pts <- station_points(df, st_selected2, mode)
          
          if (nrow(st_pts) > 0) {
            proxy <- proxy |>
              leaflet::addCircleMarkers(
                data = st_pts,
                lng = ~GISBLong,
                lat = ~GISBLat,
                radius = (marker_size_r() %||% 3) + 2,
                color = "black",
                fillOpacity = 1,
                stroke = FALSE
              )
          }
        }
        
        # Legend
        disp_map <- current_species_label_vec()
        legend_labels <- unname(disp_map[sp_order])
        
        proxy <- proxy |>
          leaflet::addLegend(
            position = "bottomright",
            colors = cols[sp_order],
            labels = legend_labels,
            title = tr("Species"),
            opacity = 1
          )
        
        if (!isTRUE(view_locked()) && nrow(vp) > 0) {
          proxy <- proxy |>
            leaflet::fitBounds(
              lng1 = min(vp$GISRLong, na.rm = TRUE),
              lat1 = min(vp$GISRLat,  na.rm = TRUE),
              lng2 = max(vp$GISRLong, na.rm = TRUE),
              lat2 = max(vp$GISRLat,  na.rm = TRUE)
            )
        }
      },
      ignoreInit = FALSE
    )
    
    # ---- Preflight: make sure webshot2/chromote sees a Chrome/Edge binary ----
    ensure_chrome <- local({
      ran <- FALSE
      function() {
        if (ran) return(invisible(TRUE))
        ran <<- TRUE
        # If chromote doesn't find a browser, try the two common Edge paths on Windows.
        ok <- tryCatch(!is.null(chromote::find_chrome()), error = function(e) FALSE)
        if (!ok) {
          paths <- c(
            "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
            "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
          )
          for (p in paths) {
            if (file.exists(p)) {
              Sys.setenv(CHROMOTE_CHROME = p)
              break
            }
          }
        }
        invisible(TRUE)
      }
    })
    
    # -----------------------------------------------------
    # JPEG Export (matches on-screen logic, including station filtering AND no-station marker exclusion)
    # -----------------------------------------------------
    output$download_map_jpeg <- downloadHandler(
      filename = function() paste0("BandAid_map_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".jpg"),
      contentType = "image/jpeg",
      content = function(file) {
        # (A) don’t block on tab; we log if needed
        if (!identical(active_tab(), "map")) {
          message("[Export] Warning: active_tab() != 'map' (got: ", as.character(active_tab()), "). Continuing.")
        }
        req(lang()); sync_i18n_lang(lang())
        
        # (B) normalize output path
        file_out <- win_normpath(file)
        
        # (C) make sure Chrom(e/ium) is resolvable
        ensure_chrome()
        used <- try(chromote::find_chrome(), silent = TRUE)
        message("[Export] Chrom(e/ium) used by webshot2: ", as.character(used))  # may show NULL if none found
        
        # (D) build the same data as on-screen
        df <- plot_data(); req(df)
        sp <- species_labels()
        ids <- sp$ids
        sp_selected <- input$species_selected %||% ids
        
        sp_order <- species_order_ids() %||% sp_selected
        sp_order <- intersect(sp_order, sp_selected)
        if (length(sp_order) == 0) sp_order <- sp_selected
        
        cols <- get_contrast_colors(length(ids))
        names(cols) <- ids
        pal_fun <- leaflet::colorFactor(palette = cols, domain = ids, na.color = "#808080")
        
        if (stations_ready()) {
          st_levels <- sort(unique(df$station_id)); st_levels <- st_levels[!is.na(st_levels)]
          st_selected <- input$station_selected %||% st_levels
          vp <- df |> dplyr::filter(English_Name %in% sp_selected, station_id %in% st_selected)
        } else {
          vp <- df |> dplyr::filter(English_Name %in% sp_selected)
        }
        
        vp <- vp[order(factor(vp$English_Name, levels = sp_order)), , drop = FALSE]
        vp$col <- pal_fun(vp$English_Name)
        
        disp_map <- current_species_label_vec()
        legend_labels <- unname(disp_map[sp_order])
        
        m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
          leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap) |>
          leaflet::addCircleMarkers(
            data = vp,
            lng = ~GISRLong,
            lat = ~GISRLat,
            radius = input$marker_size,
            color = ~col,
            fillOpacity = 0.85,
            stroke = FALSE
          ) |>
          leaflet::addLegend(
            position = "bottomright",
            colors = cols[sp_order],
            labels = legend_labels,
            title = tr("Species"),
            opacity = 1
          )
        
        if (stations_ready()) {
          st_levels <- sort(unique(df$station_id)); st_levels <- st_levels[!is.na(st_levels)]
          st_selected2 <- input$station_selected %||% st_levels
          mode <- input$station_mode %||% "centroid"
          st_pts <- station_points(df, st_selected2, mode)
          if (nrow(st_pts) > 0) {
            m <- m |>
              leaflet::addCircleMarkers(
                data = st_pts,
                lng = ~GISBLong,
                lat = ~GISBLat,
                radius = (input$marker_size %||% 3) + 2,
                color = "black",
                fillOpacity = 1,
                stroke = FALSE
              )
          }
        }
        
        if (!is.null(input$map_center) && !is.null(input$map_zoom)) {
          m <- m |> leaflet::setView(lng = input$map_center$lng, lat = input$map_center$lat, zoom = input$map_zoom)
        }
        
        # (E) viewport: integers + safety caps (Chrome requires ints; extreme sizes can fail)
        dim <- input$map_dim
        w0 <- if (!is.null(dim) && !is.null(dim$w)) as.numeric(dim$w) else 1400
        h0 <- if (!is.null(dim) && !is.null(dim$h)) as.numeric(dim$h) else 900
        if (is.na(w0) || w0 <= 0) w0 <- 1400
        if (is.na(h0) || h0 <= 0) h0 <- 900
        
        aspect <- w0 / h0
        if (input$export_size == "1080p") {
          vheight <- 1080; vwidth <- round(vheight * aspect)
        } else if (input$export_size == "4k") {
          vheight <- 2160; vwidth <- round(vheight * aspect)
        } else {
          vheight <- h0;   vwidth <- w0
        }
        vwidth  <- as.integer(round(vwidth))
        vheight <- as.integer(round(vheight))
        vwidth  <- max(100L, min(4096L, vwidth))
        vheight <- max(100L, min(4096L, vheight))
        
        # Optional Chromote stabilization (safe no-op if unnecessary)
        try(chromote::set_chrome_args("--disable-crash-reporter"), silent = TRUE)
        
        # (G) try mapshot2 with increasing delays, then fallback to webshot2::webshot
        delays <- c(2, 4, 6)  # seconds
        err_last <- NULL
        ok <- FALSE
        for (d in delays) {
          ok <- tryCatch({
            mapview::mapshot2(
              m,
              file = file_out,
              vwidth  = vwidth,
              vheight = vheight,
              delay   = d,
              remove_controls = c("zoomControl","layersControl","homeButton","scaleBar","easyButton","control")
            )
            file.exists(file_out) && file.info(file_out)$size > 0
          }, error = function(e) {
            err_last <<- conditionMessage(e); FALSE
          })
          if (ok) break
        }
        
        if (!ok) {
          html_tmp <- tempfile(fileext = ".html")
          htmlwidgets::saveWidget(m, html_tmp, selfcontained = TRUE)
          try({
            webshot2::webshot(
              url     = html_tmp,
              file    = file_out,
              vwidth  = vwidth,
              vheight = vheight,
              delay   = 4
            )
          }, silent = TRUE)
          ok <- file.exists(file_out) && file.info(file_out)$size > 0
        }
        
        if (!ok) {
          # Produce a tiny placeholder so the browser still offers a download
          .write_placeholder_jpeg(file)
          msg <- paste0("Map export failed; placeholder delivered. ",
                        if (!is.null(err_last)) paste0("Last error: ", err_last))
          showNotification(tr("Map export failed; a placeholder image was downloaded. Please try again."),
                           type = "error", duration = 8)
          warning(msg)
        }
      }
    )
    
  })
}
# Cross-platform browser detection for chromote/webshot2
set_chromium_path <- function() {
  # Try automatic detection first
  path <- try(chromote::find_chrome(), silent = TRUE)
  
  # If automatic detection fails, try common macOS and Windows paths
  if (is.null(path) || inherits(path, "try-error")) {
    
    # macOS Chrome
    mac_chrome <- "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
    if (file.exists(mac_chrome)) path <- mac_chrome
    
    # macOS Chromium
    mac_chromium <- "/Applications/Chromium.app/Contents/MacOS/Chromium"
    if (file.exists(mac_chromium)) path <- mac_chromium
    
    # macOS Brave
    mac_brave <- "/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
    if (file.exists(mac_brave)) path <- mac_brave
    
    # Windows Chrome
    win_chrome <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
    if (file.exists(win_chrome)) path <- win_chrome
    
    # Windows Edge
    win_edge <- "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
    if (file.exists(win_edge)) path <- win_edge
  }
  
  # If still nothing found → stop
  if (is.null(path) || !file.exists(path)) {
    stop(
      "No Chromium-based browser found.\n",
      "Install Chrome, Chromium, Edge, Brave, or Vivaldi.\n",
      "Safari cannot be used for screenshots."
    )
  }
  
  Sys.setenv("CHROMOTE_CHROME" = path)
  message("[Startup] Using browser for screenshots: ", path)
}

set_chromium_path()

# ===============================
# 0) Ensure required packages are installed
# ===============================
required_pkgs <- c(
  # Core app
  "shiny", "bslib", "DT", "readr", "readxl", "shinyjs", "openxlsx",
  "dplyr", "leaflet", "leaflet.extras2", "shinyjqui", "viridisLite",
  # i18n + supporting
  "shiny.i18n",
  # DB
  "DBI", "duckdb", "glue",
  # Plot export (used in Plot module for mapshot2)
  "webshot2",
  # For includeMarkdown() + markdownToHTML fallback
  "markdown"
)

ensure_packages <- function(pkgs, repos = "https://cloud.r-project.org") {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = repos, dependencies = TRUE)
  }
  invisible(TRUE)
}

ensure_packages(required_pkgs)


# ===============================
# 1) Normal app code starts here
# ===============================

options(shiny.fullstacktrace = TRUE)
options(warn = 1)

library(shiny)
library(bslib)
library(DT)
library(readr)
library(readxl)
library(shinyjs)
library(openxlsx)
library(dplyr)
library(leaflet)
library(leaflet.extras2)
library(shinyjqui)
library(viridisLite)

# --- i18n ---
library(shiny.i18n)

# DuckDB
library(DBI)
library(duckdb)
library(glue)

# modules
library(parallel)
library(grDevices)
library(chromote)
library(htmlwidgets)
library(webshot2)

# Fallback markdown conversion (only used if files are missing)
library(markdown)


options(shiny.maxRequestSize = 1500 * 1024^2)
options(shiny.launch.browser = TRUE)

#################################################
get_app_dir <- function() {
  # 1) Rscript --file=... case
  cmdArgs   <- commandArgs(trailingOnly = FALSE)
  fileArg   <- "--file="
  scriptArg <- cmdArgs[grep(fileArg, cmdArgs)]
  if (length(scriptArg) == 1L) {
    script_path <- sub(fileArg, "", scriptArg)
    if (nzchar(script_path)) {
      return(dirname(normalizePath(script_path, winslash = "/", mustWork = TRUE)))
    }
  }
  
  # 2) Try to discover the file from the call stack (e.g., source())
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
  if (is.character(of) && length(of) == 1L && nzchar(of)) {
    return(dirname(normalizePath(of, winslash = "/", mustWork = TRUE)))
  }
  
  # 3) RStudio IDE – active document (works interactively)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path)) {
      return(dirname(normalizePath(ctx$path, winslash = "/", mustWork = TRUE)))
    }
  }
  
  # 4) Fallback: working directory (common for Shiny in prod)
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

# Example usage
APP_DIR <- get_app_dir()
message("APP_DIR = ", APP_DIR)
################################################

json_path <- file.path(APP_DIR, "translations", "translation.json")
message("translation.json path = ", json_path)

i18n <- shiny.i18n::Translator$new(translation_json_path = json_path)
langs <- i18n$get_languages()
message("Loaded languages: ", paste(langs, collapse = ", "))

if (!all(c("en", "fr") %in% langs)) {
  stop("translation.json must include languages 'en' and 'fr'. Found: ", paste(langs, collapse = ", "))
}

i18n$set_translation_language("en")
tr <- function(x) i18n$t(x)

# expose to GlobalEnv (modules read from there)
assign("i18n", i18n, envir = .GlobalEnv)
assign("tr", tr, envir = .GlobalEnv)

source(file.path(APP_DIR, "BandAid upload module.R"), local = FALSE)
source(file.path(APP_DIR, "BandAid filter module.R"), local = FALSE)
source(file.path(APP_DIR, "BandAid Table module.R"),  local = FALSE)
source(file.path(APP_DIR, "BandAid Plot module.R"),   local = FALSE)

# --- Embedded English fallbacks (used if Markdown files are missing) ---
embedded_instructions_en <- "
# Band-Aid App - User Guide

Welcome to Band-Aid! This guide explains how to use each feature of the app.

## Table of Contents
1. [Upload Your Data](#upload-your-data)
2. [Apply Filters](#apply-filters)
3. [View Your Data in a Table](#view-your-data-in-a-table)
4. [Create a Map](#create-a-map)

---

## Upload Your Data

The **Upload** section is your starting point. This is where you load your GameBird data into the app.

### How to Upload:
1. Click the **Upload** tab when the app starts
2. Click **Browse** or **Choose File**
3. Select your GameBird CSV file from your computer
4. The app automatically reads your file and prepares it for analysis.

### Automatic Lookup Merging:
- The app will automatically merge any lookup files in the **Look Ups** folder
- These might include reference tables for species codes, age, or other reference data
- You don't need to do anything—this happens automatically!
- **The main file upload + lookup table merge take ~15 minutes**. If you always use the same subset (e.g., regional data for one permit), **after the first upload, create that subset and download it**, then upload only that subset for faster operations.
- Once uploaded, your data will be processed and the Filters will become available

## Apply Filters
The **Filters** section lets you refine your data by selecting specific records.

### Available Filters:
The app will automatically create filters based on the columns in your data. Common filters include:
- **Date range**
- **Species**
- **Numeric columns** (min/max)
- **Text columns** (search or select)

### How to Use Filters:
1. Click the **Filters** tab
2. Use the per-column controls
3. Dropdowns: expand and select values
4. Numeric: enter min/max or use sliders
5. Dates: pick a range
6. Click **Apply**
7. The table updates with matching records

### Filter Tips:
- Multiple filters are combined (AND)
- Matching record count shows at the bottom
- Clear any filter to reset

---

## View Your Data in a Table
The **Table** section displays your filtered data.

### Features:
- Horizontal scroll to see all columns
- Sort by clicking column headers
- Search box to find records

### Merge with a Station File (optional)
1. Expand **Add Station Names**
2. Upload a CSV/Excel with station info
3. After choosing latitude/longitude fields, the merge runs automatically
4. The table gets enriched

### Download Your Data
- Use the **Download** buttons under the table (CSV/XLSX)

### Table Navigation
- Pagination controls
- Rows per page
- Export buttons at top-right

---

## Create a Map
The **Map** section visualizes your observations geographically.

### What You'll See:
- **Encounter markers**
- **Station markers** (larger black symbols)
- A legend

### Species Selector
- Use the checkbox menu

### Station Selector
- Use the checkbox menu
- **No Station** shows observations without station assignment

### Interactions
- Zoom (wheel/pinch)
- Pan (drag)

### Station Marker Mode
- **Centroid mode**
- **Most recent**

### Export Your Map
- Click **Download Map** (JPEG/PNG)
"

embedded_version_en <- "
## VERSION HISTORY

V260212 – first version.

V260213 – modifications:
- corrected bug preventing to download a map;
- limit field list default value now 4000;
- variable Corr.Year created;
- Lookup table merge skipped if fields already in uploaded subset;
- merge species field before filtering, at the same time as the lookup tables;
- station merge now run as soon as the lat - lon fields are selected;
- removed download buttons under the filters as already existing under the data table view;
- added version history and instruction to users, available via a window by the filter fields.
"

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  shiny.i18n::usei18n(i18n),
  
  tags$head(
    tags$style(HTML("
      .app-container { --app-font-size: 12px; font-size: var(--app-font-size); }

      .app-container p,
      .app-container label,
      .app-container span { font-size: var(--app-font-size); }

      .app-container .form-control,
      .app-container .selectize-input,
      .app-container .selectize-dropdown,
      .app-container .btn { font-size: var(--app-font-size); }

      .app-container .irs--shiny .irs-single,
      .app-container .irs--shiny .irs-min,
      .app-container .irs--shiny .irs-max { font-size: var(--app-font-size); }

      .filters-panel { background: #f8f9fa; padding: 10px; border-radius: 6px; margin-bottom: 10px; }

      .app-container.compact-mode .form-control,
      .app-container.compact-mode .selectize-input { padding: 2px 6px; height: auto; }

      .app-container.compact-mode label { margin-bottom: 2px; }
      .app-container.compact-mode .btn { padding: 2px 8px; }

      .filter-summary { background: #eef3f7; padding: 6px 10px; border-radius: 4px; font-size: 0.95em; }
      .filter-summary code { background: transparent; }

      /* Centered header */
      .app-header { text-align: center; margin: 10px 0 18px 0; }
      h1.app-title {
        font-size: clamp(51px, 6.75vw, 90px) !important;
        font-weight: 800;
        line-height: 0.95;
        margin: 0;
        letter-spacing: 0.5px;
      }
      .app-lang { display: flex; justify-content: center; margin-top: 12px; }

      /* Segmented language control */
      .lang-toggle .shiny-options-group {
        display: inline-flex;
        border: 1px solid #ced4da;
        border-radius: 10px;
        overflow: hidden;
      }
      .lang-toggle .radio { margin: 0; position: relative; }
      .lang-toggle .radio input[type='radio'] { position: absolute; opacity: 0; pointer-events: none; }
      .lang-toggle .radio label {
        margin: 0; padding: 8px 16px;
        background: #fff; color: #212529;
        font-weight: 600; cursor: pointer; user-select: none;
        border-right: 1px solid #ced4da;
      }
      .lang-toggle .radio:last-child label { border-right: none; }
      .lang-toggle .radio label.lang-active { background: #0d6efd; color: #fff; }

      /* Thicker separators + tab lines */
      .app-container { --separator-thickness: 4px; --tab-border-thickness: 4px; --separator-color: var(--bs-border-color, #cfd4da); }

      .app-container hr {
        border: 0 !important;
        border-top: var(--separator-thickness) solid var(--separator-color) !important;
        opacity: 1 !important;
        margin: 14px 0 !important;
      }

      .app-container .nav-tabs {
        border-bottom: var(--tab-border-thickness) solid var(--separator-color) !important;
      }

      .app-container .nav-tabs .nav-link {
        border-width: var(--tab-border-thickness) !important;
        border-style: solid !important;
        border-color: transparent !important;
        margin-bottom: calc(-1 * var(--tab-border-thickness)) !important;
      }

      .app-container .nav-tabs .nav-link:hover {
        border-color: var(--separator-color) var(--separator-color) transparent var(--separator-color) !important;
      }

      .app-container .nav-tabs .nav-link.active,
      .app-container .nav-tabs .nav-item.show .nav-link {
        border-color: var(--separator-color) var(--separator-color) #fff var(--separator-color) !important;
        border-bottom-color: #fff !important;
      }

      .app-container .tab-content {
        border-top: var(--tab-border-thickness) solid var(--separator-color);
        padding-top: 10px;
      }

      /* ==========================================
         DT cell width control (prevents huge rows)
         ========================================== */

      .app-container { --dt-max-col-width: 240px; }  /* default */

      .app-container .dataTables_wrapper table.dataTable th,
      .app-container .dataTables_wrapper table.dataTable td {
        max-width: var(--dt-max-col-width);
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      /* Allow user to switch to wrapped text */
      .app-container.dt-wrap .dataTables_wrapper table.dataTable th,
      .app-container.dt-wrap .dataTables_wrapper table.dataTable td {
        white-space: normal;
        overflow: visible;
        text-overflow: clip;
        word-break: break-word;
      }

      /* Make sure wide tables remain usable */
      .app-container .dataTables_wrapper {
        overflow-x: auto;
      }

      /* ==========================================
         Full-screen Loading Overlay
         ========================================== */
      .loading-overlay {
        position: fixed;
        inset: 0;
        background: rgba(255,255,255,0.85);
        display: none;
        align-items: center;
        justify-content: center;
        z-index: 9999;
        backdrop-filter: blur(1px);
      }
      .loading-box {
        text-align: center;
        color: #0d6efd;
        font-weight: 600;
      }
      .loading-box img {
        width: 140px;
        height: auto;
        display: block;
        margin: 0 auto 10px auto;
        animation: floaty 1.8s ease-in-out infinite;
        filter: drop-shadow(0 4px 10px rgba(0,0,0,0.15));
      }
      .loading-box .loading-text {
        font-size: 1.1rem;
        letter-spacing: 0.3px;
      }
      @keyframes floaty {
        0%   { transform: translateY(0); }
        50%  { transform: translateY(-6px); }
        100% { transform: translateY(0); }
      }

      /* ======================
         Right-side Help panel
         ====================== */
      .help-details summary {
        cursor: pointer;
        font-weight: 600;
        color: #0d6efd;
        outline: none;
        list-style: none;
      }
      .help-details summary::-webkit-details-marker { display: none; }
      .help-details summary:before {
        content: '›';
        display: inline-block;
        margin-right: 6px;
        transition: transform 0.15s ease;
      }
      .help-details[open] summary:before { transform: rotate(90deg); }

      .help-panel {
        background: #ffffff;
        border: 1px solid #dee2e6;
        border-radius: 6px;
        padding: 10px;
        height: 100%;
        display: flex;
        flex-direction: column;
        gap: 8px;
        margin-top: 6px;
        font-size: calc(var(--app-font-size) * 1.0);
      }
      .help-content {
        flex: 1 1 auto;
        overflow-y: auto;
        max-height: 460px;
        padding-right: 4px;
      }
      .help-toggle .shiny-options-group {
        display: inline-flex;
        border: 1px solid #ced4da;
        border-radius: 10px;
        overflow: hidden;
      }
      .help-toggle .radio { margin: 0; position: relative; }
      .help-toggle .radio input[type='radio'] { position: absolute; opacity: 0; pointer-events: none; }
      .help-toggle .radio label {
        margin: 0; padding: 6px 12px;
        background: #fff; color: #212529;
        font-weight: 600; cursor: pointer; user-select: none;
        border-right: 1px solid #ced4da;
        font-size: var(--app-font-size);
      }
      .help-toggle .radio:last-child label { border-right: none; }
      .help-toggle .radio label.help-active { background: #0d6efd; color: #fff; }
    ")),
    
    # Existing handlers
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setFontSize', function(size) {
        var el = document.querySelector('.app-container');
        if (el) el.style.setProperty('--app-font-size', size);
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setCompact', function(compact) {
        var el = document.querySelector('.app-container');
        if (el) el.classList.toggle('compact-mode', compact);
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('closeFilters', function(_) {
        var el = document.querySelector('.filters-panel');
        if (el) el.open = false;
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setDTMaxColWidth', function(px) {
        var el = document.querySelector('.app-container');
        if (el) el.style.setProperty('--dt-max-col-width', px);
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setDTWrap', function(wrap) {
        var el = document.querySelector('.app-container');
        if (el) el.classList.toggle('dt-wrap', !!wrap);
      });
    ")),
    
    # Loading overlay toggler + text updates (client-side timer version)
    tags$script(HTML("
      (function(){
        var loaderTimerId = null;
        Shiny.addCustomMessageHandler('setLoading', function(isLoading) {
          var overlay = document.getElementById('loading-overlay');
          if (!overlay) return;
          var elapsedEl = document.getElementById('loading-elapsed');

          if (isLoading) {
            overlay.style.display = 'flex';
            var startTs = Date.now();
            overlay.dataset.startTs = startTs;

            if (elapsedEl) {
              if (loaderTimerId) { clearInterval(loaderTimerId); loaderTimerId = null; }
              loaderTimerId = setInterval(function(){
                var sec = (Date.now() - startTs) / 1000;
                elapsedEl.textContent = 'Elapsed: ' + sec.toFixed(1) + ' s';
              }, 200);
            }
          } else {
            overlay.style.display = 'none';
            if (loaderTimerId) { clearInterval(loaderTimerId); loaderTimerId = null; }
            if (elapsedEl) elapsedEl.textContent = '';
            delete overlay.dataset.startTs;
          }
        });
      })();
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setLoadingText', function(txt) {
        var el = document.getElementById('loading-text-msg');
        if (el) el.textContent = txt;
      });
    ")),
    
    # Force-hide overlay on initial load
    tags$script(HTML("
      (function() {
        function hideLoader() {
          var el = document.getElementById('loading-overlay');
          if (el) el.style.display = 'none';
        }
        document.addEventListener('DOMContentLoaded', hideLoader);
        if (window.jQuery) {
          jQuery(document).on('shiny:connected', hideLoader);
        } else if (window.$) {
          $(document).on('shiny:connected', hideLoader);
        }
      })();
    ")),
    
    # Sync language segmented control
    tags$script(HTML("
      function syncLangToggleActive() {
        var container = document.getElementById('lang');
        if (!container) return;
        var labels = container.querySelectorAll('.radio label');
        labels.forEach(function(l) { l.classList.remove('lang-active'); });
        var checked = container.querySelector('input[type=radio]:checked');
        if (checked && checked.parentElement && checked.parentElement.tagName.toLowerCase() === 'label') {
          checked.parentElement.classList.add('lang-active');
        }
      }
      document.addEventListener('change', function(e) {
        if (e && e.target && e.target.name === 'lang') syncLangToggleActive();
      });
      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(syncLangToggleActive, 400);
      });
    ")),
    
    # Sync help segmented control
    tags$script(HTML("
      function syncHelpToggleActive() {
        var container = document.getElementById('help_mode_container');
        if (!container) return;
        var labels = container.querySelectorAll('.radio label');
        labels.forEach(function(l) { l.classList.remove('help-active'); });
        var checked = container.querySelector('input[type=radio]:checked');
        if (checked && checked.parentElement && checked.parentElement.tagName.toLowerCase() === 'label') {
          checked.parentElement.classList.add('help-active');
        }
      }
      document.addEventListener('change', function(e) {
        if (e && e.target && e.target.name === 'help_mode') syncHelpToggleActive();
      });
      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(syncHelpToggleActive, 400);
      });
    ")),
    tags$script(HTML("
  document.addEventListener('DOMContentLoaded', function() {
    const helpDetails = document.querySelector('.help-details');
    if (helpDetails) {
      helpDetails.open = false;   // force closed on initial load
    }
  });

  // Also ensure Shiny re-rendering doesn't open it
  document.addEventListener('shiny:idle', function() {
    const helpDetails = document.querySelector('.help-details');
    if (helpDetails && helpDetails.dataset.userToggled !== 'true') {
      helpDetails.open = false;
    }
  });

  // Track if user manually opens/closes it
  document.addEventListener('click', function(e) {
    let d = e.target.closest('.help-details');
    if (d) {
      // prevent Shiny from auto-overriding manual toggle
      d.dataset.userToggled = 'true';
    }
  });
"))
  ),
  
  useShinyjs(),
  
  div(
    class = "app-container",
    
    # Loading overlay HTML (bird GIF in /www/bird.gif)
    div(
      id = "loading-overlay", class = "loading-overlay", style = "display:none;",
      div(
        class = "loading-box",
        tags$img(src = "bird.gif", alt = "Loading..."),
        div(
          class = "loading-text",
          span(id = "loading-text-msg", tr("Loading data... Please wait")),
          div(id = "loading-elapsed", style = "margin-top:4px; font-weight:600;")
        )
      )
    ),
    
    div(
      class = "app-header",
      tags$h1(class = "app-title", "BAND-AID"),
      
      
      div(
        style = "font-size: 1.1rem; font-weight: 600; margin-top: 4px; color: #0d6efd;",
        textOutput("app_credit", inline = TRUE)
      ),
      
      
      
      div(
        class = "app-lang",
        div(
          class = "lang-toggle",
          radioButtons(
            inputId = "lang",
            label = NULL,
            choices = c("English" = "en", "Français" = "fr"),
            selected = "en",
            inline = TRUE
          )
        )
      )
    ),
    
    # Upload/filters + collapsible Help panel
    tags$details(
      class = "filters-panel",
      open = FALSE,
      tags$summary(uiOutput("upload_filters_summary")),
      br(),
      fluidRow(
        # Left: Upload + display controls
        column(
          4,
          mod_upload_ui("upload"),
          sliderInput("font_size", tr("Font size"), min = 10, max = 18, value = 14, step = 1),
          
          # DT display controls
          sliderInput("dt_col_width", tr("Max column width (px)"),
                      min = 80, max = 600, value = 240, step = 10),
          
          checkboxInput("dt_wrap", tr("Wrap cell text"), value = FALSE)
        ),
        # Middle: Filters
        column(5, mod_filters_ui("filters")),
        # Right: Collapsible Help panel (NEW)
        column(
          3,
          tags$details(
            class = "help-details",
            open = FALSE,  # collapsed by default
            tags$summary(textOutput("help_summary_label")),
            div(
              class = "help-panel",
              uiOutput("help_selector"),
              div(class = "help-content", uiOutput("help_panel"))
            )
          )
        )
      )
    ),
    
    fluidRow(
      column(12, div(class = "filter-summary", mod_filters_summary_ui("filters")))
    ),
    
    hr(),
    
    # Merge panel ABOVE tabs, only on Table tab
    conditionalPanel(
      condition = "input.main_tabs === 'table'",
      mod_table_merge_ui("table"),
      hr()
    ),
    
    tabsetPanel(
      id = "main_tabs",
      tabPanel(tr("Table"), value = "table", mod_table_ui("table")),
      tabPanel(tr("Encounter Map"), value = "map", mod_plot_ui("plot"))
    )
  )
)

server <- function(input, output, session) {
  
  output$app_credit <- renderText({
    req(input$lang)
    i18n$set_translation_language(input$lang)
    tr("Created by François Bolduc, Canadian Wildlife Service, Quebec Region")
  })
  
  session$sendCustomMessage("setCompact", TRUE)
  
  observeEvent(input$lang, {
    req(input$lang)
    i18n$set_translation_language(input$lang)
    shiny.i18n::update_lang(input$lang, session = session)
    
    # keep .GlobalEnv in sync (modules read from there)
    assign("i18n", i18n, envir = .GlobalEnv)
    assign("tr", tr, envir = .GlobalEnv)
  }, ignoreInit = TRUE)
  
  output$upload_filters_summary <- renderUI({
    req(input$lang)
    i18n$set_translation_language(input$lang)
    tags$strong(tr("Upload & Filters"))
  })
  
  # Collapsible help summary text (reactive to language)
  output$help_summary_label <- renderText({
    req(input$lang)
    i18n$set_translation_language(input$lang)
    # Keep it short; this is the clickable summary text
    paste0("ℹ️ ", tr("Help (Instructions / Version history)"))
  })
  
  # --- Help selector (reactive to language) ---
  output$help_selector <- renderUI({
    req(input$lang)
    i18n$set_translation_language(input$lang)
    
    labels <- c(tr("Instructions"), tr("Version history"))
    values <- c("instructions", "version")
    
    div(
      id = "help_mode_container",
      class = "help-toggle",
      radioButtons(
        inputId = "help_mode",
        label = NULL,
        choices = setNames(values, labels),
        selected = "instructions",
        inline = TRUE
      )
    )
  })
  
  # --- Help content with file-based + fallback logic ---
  output$help_panel <- renderUI({
    req(input$lang)
    mode <- if (is.null(input$help_mode)) "instructions" else input$help_mode
    lang <- input$lang
    
    # ./help/<mode>_<lang>.md ; fallback to en; else fallback to embedded English
    md_lang <- file.path(APP_DIR, "help", sprintf("%s_%s.md", mode, lang))
    md_en   <- file.path(APP_DIR, "help", sprintf("%s_en.md", mode))
    
    if (file.exists(md_lang)) {
      includeMarkdown(md_lang)
    } else if (lang != "en" && file.exists(md_en)) {
      includeMarkdown(md_en)
    } else {
      md_text <- if (identical(mode, "instructions")) embedded_instructions_en else embedded_version_en
      HTML(markdown::markdownToHTML(text = md_text, fragment.only = TRUE))
    }
  })
  outputOptions(output, "help_panel", suspendWhenHidden = FALSE)
  
  # Modules
  data_source <- mod_upload_server("upload", lang = reactive(input$lang))
  filtered_preview <- mod_filters_server("filters", data_source, lang = reactive(input$lang))
  final_data <- mod_table_server("table", filtered_preview, lang = reactive(input$lang))
  
  mod_plot_server(
    id = "plot",
    final_data = final_data,
    active_tab = reactive(input$main_tabs),
    lang = reactive(input$lang)
  )
  
  # Font size -> CSS var
  observeEvent(input$font_size, {
    session$sendCustomMessage("setFontSize", paste0(input$font_size, "px"))
  }, ignoreInit = TRUE)
  
  # DT width / wrap -> CSS var / class
  observeEvent(input$dt_col_width, {
    w <- max(80, as.integer(input$dt_col_width))
    session$sendCustomMessage("setDTMaxColWidth", paste0(w, "px"))
  }, ignoreInit = TRUE)
  
  observeEvent(input$dt_wrap, {
    session$sendCustomMessage("setDTWrap", isTRUE(input$dt_wrap))
  }, ignoreInit = TRUE)
  
  # Initialize DT UI defaults once
  observe({
    req(input$dt_col_width)
    session$sendCustomMessage("setDTMaxColWidth", paste0(max(80, as.integer(input$dt_col_width)), "px"))
    session$sendCustomMessage("setDTWrap", isTRUE(input$dt_wrap))
  })
}

shinyApp(ui, server)

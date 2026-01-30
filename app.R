# ===============================
# 0) Ensure required packages are installed
# ===============================
required_pkgs <- c(
  # Core app
  "shiny", "bslib", "DT", "readr", "readxl", "shinyjs", "openxlsx",
  "dplyr", "leaflet", "leaflet.extras", "shinyjqui", "viridisLite",
  # i18n + supporting
  "shiny.i18n",
  # DB
  "DBI", "duckdb", "glue",
  # Plot export (used in Plot module for mapshot2)
  "mapview", "webshot2",
  # Optional but used in get_app_dir (only if in RStudio)
  "rstudioapi"
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

Sys.setenv(
  CHROMOTE_CHROME = "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe"
)

library(shiny)
library(bslib)
library(DT)
library(readr)
library(readxl)
library(shinyjs)
library(openxlsx)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(shinyjqui)
library(viridisLite)

# --- i18n ---
library(shiny.i18n)

# DuckDB
library(DBI)
library(duckdb)
library(glue)

options(shiny.maxRequestSize = 1500 * 1024^2)
options(shiny.launch.browser = TRUE)

get_app_dir <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (is.character(p) && length(p) == 1 && nzchar(p)) {
      return(dirname(normalizePath(p, winslash = "/", mustWork = TRUE)))
    }
  }
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) "")
  if (is.character(of) && length(of) == 1 && nzchar(of)) {
    return(dirname(normalizePath(of, winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

app_dir <- get_app_dir()
message("app_dir = ", app_dir)

json_path <- file.path(app_dir, "translations", "translation.json")
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

source(file.path(app_dir, "BandAid upload module.R"), local = FALSE)
source(file.path(app_dir, "BandAid filter module.R"), local = FALSE)
source(file.path(app_dir, "BandAid Table module.R"),  local = FALSE)
source(file.path(app_dir, "BandAid Plot module.R"),   local = FALSE)

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
        font-weight: 900;
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
        font-weight: 700; cursor: pointer; user-select: none;
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
    ")),
    
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
    "))
  ),
  
  useShinyjs(),
  
  div(
    class = "app-container",
    
    div(
      class = "app-header",
      tags$h1(class = "app-title", "BAND-AID"),
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
    
    # Upload/filters
    tags$details(
      class = "filters-panel",
      open = FALSE,
      tags$summary(uiOutput("upload_filters_summary")),
      br(),
      fluidRow(
        column(
          4,
          mod_upload_ui("upload"),
          sliderInput("font_size", tr("Font size"), min = 10, max = 18, value = 14, step = 1)
        ),
        column(8, mod_filters_ui("filters"))
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
  
  data_source <- mod_upload_server("upload", lang = reactive(input$lang))
  filtered_preview <- mod_filters_server("filters", data_source, lang = reactive(input$lang))
  final_data <- mod_table_server("table", filtered_preview, lang = reactive(input$lang))
  
  mod_plot_server(
    id = "plot",
    final_data = final_data,
    active_tab = reactive(input$main_tabs),
    lang = reactive(input$lang)
  )
  
  observeEvent(input$font_size, {
    session$sendCustomMessage("setFontSize", paste0(input$font_size, "px"))
  })
}

shinyApp(ui, server)
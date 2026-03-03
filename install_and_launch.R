# install_and_launch.R
# Run from the app root: Rscript --vanilla install_and_launch.R

# ---------- CONFIG ----------
`%||%` <- function(a, b) if (is.null(a)) b else a

# Determine app dir (works from Rscript or source)
app_dir <- tryCatch(
  normalizePath(dirname(sys.frame(1)$ofile), winslash = "/", mustWork = FALSE),
  error = function(e) getwd()
)
app_dir <- if (!nzchar(app_dir)) getwd() else app_dir

www_dir     <- file.path(app_dir, "www")
status_html <- file.path(www_dir, "install_status.html")

# Fixed host/port so we can redirect to it
host <- "127.0.0.1"
port <- 8787
redirect_url <- sprintf("http://%s:%d", host, port)

# Same required packages list as your app (kept in one place here)
required_pkgs <- c(
  # Core app
  "shiny", "bslib", "DT", "readr", "readxl", "shinyjs", "openxlsx",
  "dplyr", "leaflet", "leaflet.extras2", "shinyjqui", "viridisLite",
  # i18n + supporting
  "shiny.i18n",
  # DB
  "DBI", "duckdb", "glue",
  # Plot export
  "webshot2", "chromote", "htmlwidgets",
  # Markdown fallback
  "markdown"
)

# ---------- HELPERS ----------
safe_toJSON <- function(x) {
  # Avoid locale issues on Windows
  jsonlite::toJSON(x, auto_unbox = TRUE, pretty = FALSE, null = "null")
}

write_status <- function(msg, pct = NA, logs = NULL, redirect = NULL) {
  hash_obj <- list(msg = msg, pct = pct, logs = logs, redirect = redirect)
  hash <- utils::URLencode(as.character(safe_toJSON(hash_obj)))
  # Rebuild status HTML each time so it will refresh even from file://
  html <- sprintf(
    '<!doctype html>
<html><head>
<meta charset="utf-8">
<meta http-equiv="refresh" content="5">
<title>%s</title>
<style>
:root{--blue:#0d6efd;--bg:#f8f9fa;--border:#dee2e6;--muted:#6c757d}
body{font-family:system-ui,Arial,sans-serif;margin:3rem;color:var(--blue)}
h1{margin-top:0;font-size:2rem}
.box{background:var(--bg);border:1px solid var(--border);border-radius:8px;padding:1rem 1.25rem}
.muted{color:var(--muted)}
.progress{width:100%%;background:#e9ecef;border-radius:6px;height:14px;overflow:hidden;margin-top:.5rem}
.bar{height:100%%;background:var(--blue);width:0;transition:width .2s ease}
.logs{margin-top:1rem;font-family:ui-monospace,SFMono-Regular,Consolas,monospace;font-size:.92rem;white-space:pre-wrap;color:#0b4e9a}
</style>
</head>
<body>
<h1>Band‑Aid – Preparing your first run</h1>
<div class="box">
  <p id="msg">Starting…</p>
  <div class="progress"><div id="bar" class="bar" style="width:0%%"></div></div>
  <p class="muted">Please keep this tab open. The first setup can take 20–40 minutes depending on your network speed.</p>
  <div id="logs" class="logs"></div>
</div>
<script>
  const data = %s;
  if (data.msg) document.getElementById("msg").textContent = data.msg;
  if (data.pct != null) document.getElementById("bar").style.width = (Math.max(0, Math.min(100, data.pct)) + "%%");
  if (data.logs) document.getElementById("logs").textContent = data.logs;
  if (data.redirect) { setTimeout(() => { location.href = data.redirect; }, 1500); }
</script>
</body></html>', msg, safe_toJSON(list(msg = msg, pct = pct, logs = logs, redirect = redirect))
  )
  
  dir.create(dirname(status_html), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, status_html, useBytes = TRUE)
}

append_log <- function(lines) {
  # In-memory aggregation for the short session
  assign(".install_log", c(get(".install_log", envir = .GlobalEnv, ifnotfound = character()), lines),
         envir = .GlobalEnv)
  paste(get(".install_log", envir = .GlobalEnv, ifnotfound = character()), collapse = "\n")
}

install_one <- function(pkg, i, n) {
  logs <- append_log(sprintf("[%02d/%02d] %s", i, n, pkg))
  write_status(sprintf("Checking %s (%d/%d)…", pkg, i, n), round((i - 1) / n * 100), logs)
  if (!requireNamespace(pkg, quietly = TRUE)) {
    write_status(sprintf("Installing %s (%d/%d)…", pkg, i, n), round((i - 1) / n * 100), logs)
    utils::install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
    logs <- append_log(sprintf("  ✓ Installed %s", pkg))
    write_status(sprintf("Installed %s", pkg), round(i / n * 100), logs)
  } else {
    logs <- append_log(sprintf("  ✓ Already present: %s", pkg))
    write_status(sprintf("Already installed: %s", pkg), round(i / n * 100), logs)
  }
}

# ---------- RUN ----------
assign(".install_log", character(), envir = .GlobalEnv)

write_status("Starting installation checks…", 0, logs = "")

# CRAN mirror hint (optional) – uncomment to enforce:
# options(repos = c(CRAN = "https://cloud.r-project.org"))

n <- length(required_pkgs)
for (i in seq_along(required_pkgs)) {
  install_one(required_pkgs[i], i, n)
}

# Optional: verify chromote/browser after install (your app also handles this)
# if (requireNamespace("chromote", quietly = TRUE)) { chromote::find_chrome() }

# Ready – write redirect and start the app
logs <- append_log("All dependencies are ready.")
write_status("All set! Launching the app…", 100, logs = logs, redirect = redirect_url)

options(shiny.launch.browser = FALSE)
message(sprintf("Starting Shiny on %s", redirect_url))
shiny::runApp(appDir = app_dir, host = host, port = port, launch.browser = FALSE)






# # ===============================
# # 0) Ensure required packages are installed
# # ===============================
# required_pkgs <- c(
#   # Core app
#   "shiny", "bslib", "DT", "readr", "readxl", "shinyjs", "openxlsx",
#   "dplyr", "leaflet", "leaflet.extras2", "shinyjqui", "viridisLite",
#   # i18n + supporting
#   "shiny.i18n",
#   # DB
#   "DBI", "duckdb", "glue",
#   # Plot export (used in Plot module for mapshot2)
#   "webshot2", 'chromote', 'htmlwidgets',
#   # For includeMarkdown() + markdownToHTML fallback
#   "markdown"
# )
# 
# ensure_packages <- function(pkgs, repos = "https://cloud.r-project.org") {
#   missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
#   if (length(missing)) {
#     message("Installing missing packages: ", paste(missing, collapse = ", "))
#     install.packages(missing, repos = repos, dependencies = TRUE)
#   }
#   invisible(TRUE)
# }
# 
# ensure_packages(required_pkgs)

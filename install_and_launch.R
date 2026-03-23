# install_and_launch.R
# Package-free installer/launcher for your Shiny app.

# ---------------- CONFIG ----------------
# Fixed host/port (change if needed)
host <- "127.0.0.1"
port <- 8787
redirect_url <- sprintf("http://%s:%d", host, port)

# Your required packages (same list as in app.R)
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

# Packages that have a known GitHub fallback if CRAN install fails/stalls
github_fallbacks <- list(
  "leaflet.extras2" = "trafficonweb/leaflet.extras2"
)

# Enforce CRAN mirror and generous timeout (seconds) for slow connections
options(
  repos    = c(CRAN = "https://cloud.r-project.org"),
  timeout  = 300L   # 5 minutes per download; raise if on very slow link
)

# --------------- HELPERS ---------------
pf <- function(...) file.path(...)
now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Determine app directory robustly
get_app_dir <- function() {
  script_path <- tryCatch(normalizePath(dirname(sys.frame(1)$ofile), winslash = "/", mustWork = FALSE),
                          error = function(e) "")
  if (nzchar(script_path)) return(script_path)
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

app_dir    <- get_app_dir()
www_dir    <- pf(app_dir, "www")
status_html <- pf(www_dir, "install_status.html")
log_file   <- pf(app_dir, "install_and_launch.log")

# Append to a text log we can inspect
log_append <- function(line) {
  dir.create(app_dir, showWarnings = FALSE, recursive = TRUE)
  cat(sprintf("[%s] %s\n", now(), line), file = log_file, append = TRUE)
}

# Write the status page with simple inline data (no jsonlite)
# We inline msg/pct/logs directly into the HTML to avoid any dependencies.
write_status <- function(msg, pct = NA, logs = "", redirect = NULL) {
  dir.create(www_dir, showWarnings = FALSE, recursive = TRUE)
  safe <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;",  x, fixed = TRUE)
    x <- gsub(">", "&gt;",  x, fixed = TRUE)
    x
  }
  msg_safe  <- safe(msg %||% "")
  logs_safe <- safe(logs %||% "")
  pct_css   <- if (!is.na(pct)) sprintf("%d%%", max(0L, min(100L, as.integer(pct)))) else "0%"

  redirect_js <- if (!is.null(redirect) && nzchar(redirect)) {
    sprintf('setTimeout(function(){ location.href = "%s"; }, 1500);', redirect)
  } else {
    ""
  }

  html <- sprintf(
    '<!doctype html>
<html><head>
<meta charset="utf-8">
<meta http-equiv="refresh" content="5">
<title>Band&#x2011;Aid &#x2013; Initial Setup</title>
<style>
:root{--blue:#0d6efd;--bg:#f8f9fa;--border:#dee2e6;--muted:#6c757d}
body{font-family:system-ui,Arial,sans-serif;margin:3rem;color:var(--blue)}
h1{margin-top:0;font-size:2rem}
.box{background:var(--bg);border:1px solid var(--border);border-radius:8px;padding:1rem 1.25rem}
.muted{color:var(--muted)}
.progress{width:100%%;background:#e9ecef;border-radius:6px;height:14px;overflow:hidden;margin-top:.5rem}
.bar{height:100%%;background:var(--blue);width:%s;transition:width .2s ease}
.logs{margin-top:1rem;font-family:ui-monospace,SFMono-Regular,Consolas,monospace;font-size:.92rem;white-space:pre-wrap;color:#0b4e9a}
</style>
</head>
<body>
<h1>Band&#x2011;Aid &#x2013; Preparing your first run</h1>
<div class="box">
  <p id="msg">%s</p>
  <div class="progress"><div class="bar"></div></div>
  <p class="muted">Please keep this tab open. The first setup can take 20&#x2013;40 minutes depending on your network speed.</p>
  <div id="logs" class="logs">%s</div>
</div>
<script>
%s
</script>
</body></html>', pct_css, msg_safe, logs_safe, redirect_js)
  
  writeLines(html, status_html, useBytes = TRUE)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

append_log <- function(lines) {
  current <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character())
  all <- c(current, paste0("  ", lines))
  log_append(paste(lines, collapse = "\n"))
  paste(all, collapse = "\n")
}

# --------------- INSTALL HELPERS ---------------

# Try installing from CRAN with a hard wall-clock timeout
install_cran <- function(pkg) {
  result <- tryCatch({
    utils::install.packages(pkg, dependencies = TRUE, quiet = FALSE)
    requireNamespace(pkg, quietly = TRUE)   # TRUE = success
  }, error   = function(e) { log_append(sprintf("CRAN error for %s: %s", pkg, conditionMessage(e))); FALSE },
     warning = function(w) { log_append(sprintf("CRAN warning for %s: %s", pkg, conditionMessage(w))); requireNamespace(pkg, quietly = TRUE) })
  isTRUE(result)
}

# Try installing from GitHub via remotes (only if remotes is available)
install_github <- function(pkg, repo) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    log_append("remotes not available – skipping GitHub fallback")
    return(FALSE)
  }
  result <- tryCatch({
    remotes::install_github(repo, dependencies = TRUE, upgrade = "never")
    requireNamespace(pkg, quietly = TRUE)
  }, error   = function(e) { log_append(sprintf("GitHub error for %s: %s", pkg, conditionMessage(e))); FALSE },
     warning = function(w) { log_append(sprintf("GitHub warning for %s: %s", pkg, conditionMessage(w))); requireNamespace(pkg, quietly = TRUE) })
  isTRUE(result)
}

# Check or install one package and update status page
install_one <- function(pkg, i, n) {
  logs <- append_log(sprintf("[%02d/%02d] %s", i, n, pkg))
  write_status(sprintf("Checking %s (%d/%d)…", pkg, i, n), round((i - 1) / n * 100), logs)

  if (suppressWarnings(requireNamespace(pkg, quietly = TRUE))) {
    logs <- append_log(sprintf("✓ Already present: %s", pkg))
    write_status(sprintf("Already installed: %s", pkg), round(i / n * 100), logs)
    return(invisible(TRUE))
  }

  # --- attempt 1: CRAN ---
  write_status(sprintf("Installing %s from CRAN (%d/%d)…", pkg, i, n), round((i - 1) / n * 100), logs)
  ok <- install_cran(pkg)

  # --- attempt 2: GitHub fallback (for known problematic packages) ---
  if (!ok && pkg %in% names(github_fallbacks)) {
    logs <- append_log(sprintf("CRAN install failed/stalled for %s – trying GitHub…", pkg))
    write_status(sprintf("Retrying %s from GitHub…", pkg), round((i - 1) / n * 100), logs)
    ok <- install_github(pkg, github_fallbacks[[pkg]])
  }

  if (ok) {
    logs <- append_log(sprintf("✓ Installed %s", pkg))
    write_status(sprintf("Installed %s", pkg), round(i / n * 100), logs)
  } else {
    logs <- append_log(sprintf("✗ FAILED to install %s – app may not work correctly", pkg))
    write_status(sprintf("WARNING: could not install %s", pkg), round(i / n * 100), logs)
  }
  invisible(ok)
}

# --------------- RUN ----------------
unlink(log_file, force = TRUE)
log_append(sprintf("App dir: %s", app_dir))
log_append(sprintf("R version: %s", R.version.string))
log_append(sprintf("CRAN mirror: %s", getOption("repos")[["CRAN"]]))
log_append(sprintf("Download timeout: %ds", getOption("timeout")))
write_status("Starting installation checks…", 0, logs = "")

# Install/check dependencies
n <- length(required_pkgs)
for (i in seq_along(required_pkgs)) {
  install_one(required_pkgs[i], i, n)
}

logs <- append_log("All dependencies are ready. Launching the app…")
write_status("All set! Launching the app…", 100, logs = logs, redirect = redirect_url)

options(shiny.launch.browser = FALSE)
message(sprintf("Starting Shiny on %s", redirect_url))
shiny::runApp(appDir = app_dir, host = host, port = port, launch.browser = FALSE)

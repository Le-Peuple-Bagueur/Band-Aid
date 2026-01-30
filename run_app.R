# run_app.R - one script that launches your Shiny app locally

# Always run from the directory where this script lives
args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- sub(file_arg, "", args[grep(file_arg, args)])
app_dir <- normalizePath(dirname(script_path), winslash = "/", mustWork = TRUE)
setwd(app_dir)

message("Starting BandAid from: ", app_dir)

# Optional: Use renv if present (recommended)
if (file.exists(file.path(app_dir, "renv.lock"))) {
  if (!requireNamespace("renv", quietly = TRUE)) {
    install.packages("renv", repos = "https://cloud.r-project.org")
  }
  # This will use renv.lock to install exact dependencies (first run only)
  renv::restore(prompt = FALSE)
  renv::activate()
}

# Safety: ensure shiny is present
if (!requireNamespace("shiny", quietly = TRUE)) {
  install.packages("shiny", repos = "https://cloud.r-project.org")
}

# Your app already sets this; keep here if you want a single place
options(shiny.maxRequestSize = 1500 * 1024^2)

# Launch
shiny::runApp(appDir = app_dir, launch.browser = TRUE)
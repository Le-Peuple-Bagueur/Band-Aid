##if you get the following error, run the following code in console:
#error: variable of type NULL

# Erreur dans source(file.path(app_dir, "BandAid Plot module.R"), local = FALSE) : 
#   tentative d'utilisation de nom de variable de longueur nulle


sanitize_r_file <- function(path) {
  x <- readLines(path, warn = FALSE)
  file.copy(path, paste0(path, ".bak"), overwrite = TRUE)
  
  x2 <- x
  x2 <- gsub("&lt;", "<", x2, fixed = TRUE)
  x2 <- gsub("&gt;", ">", x2, fixed = TRUE)
  x2 <- gsub("&amp;", "&", x2, fixed = TRUE)
  x2 <- x2[!grepl("^\\s*`{2,3}\\s*$", x2)]
  
  writeLines(x2, path, useBytes = TRUE)
  
  # Test parse
  parse(path)
  message("✅ OK: ", basename(path))
}

base <- "C:/Users/BolducF/Documents/ShinyApps/BandAid/MainApp"
files <- c("app.R",
           "BandAid upload module.R",
           "BandAid filter module.R",
           "BandAid Table module.R",
           "BandAid Plot module.R")

invisible(lapply(file.path(base, files), sanitize_r_file))
message("🎉 Tous les fichiers parsés avec succès.")

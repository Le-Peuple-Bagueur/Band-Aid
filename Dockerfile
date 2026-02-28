# Image de base optimisée pour Shiny
FROM rocker/shiny:4.3.2

# Mise à jour et installation des dépendances système
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Définir le répertoire de travail...
WORKDIR /srv/shiny-server/app

# Copier ton app dans l'image
COPY . /srv/shiny-server/app

# Installer les packages R nécessaires
RUN R -e "install.packages(c(
    'shiny', 'bslib', 'readxl', 'openxlsx',
    'leaflet', 'leaflet.extras2', 'viridislite', 'shiny.i18n',
    'webshot2', 'markdown', 'shinydashboard', 'DT',
    'dplyr', 'readr', 'stringr', 'glue', 'purrr', 'tidyr',
    'duckdb', 'DBI', 'shinyjs', 'shinyWidgets', 'chromote',
    'htmlwidgets'
))"


# Cloud Run exige que l'app écoute sur le port 8080
EXPOSE 8080

# Modifier la config Shiny Server pour écouter sur 8080
RUN sed -i 's|site_dir .*|site_dir /srv/shiny-server/app;|' /etc/shiny-server/shiny-server.conf \
    && sed -i 's/3838/8080/' /etc/shiny-server/shiny-server.conf


# Lancer Shiny Server
CMD ["/usr/bin/shiny-server"]

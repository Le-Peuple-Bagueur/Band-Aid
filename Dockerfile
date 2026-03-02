# Base image with Shiny Server
FROM rocker/shiny:4.3.2

# System deps (minimal + chromium for webshot2/chromote screenshots)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    chromium \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*


# Install Google Chrome (headless-capable) from Google's repo
RUN set -eux; \
    mkdir -p /usr/share/keyrings; \
    curl -fsSL https://dl.google.com/linux/linux_signing_key.pub \
      | gpg --dearmor -o /usr/share/keyrings/google-linux-keyring.gpg; \
    echo "deb [arch=amd64 signed-by=/usr/share/keyrings/google-linux-keyring.gpg] http://dl.google.com/linux/chrome/deb/ stable main" \
      > /etc/apt/sources.list.d/google-chrome.list; \
    apt-get update; \
    apt-get install -y --no-install-recommends google-chrome-stable fonts-liberation; \
    # Symlinks so your existing CHROMOTE_CHROME=/usr/bin/google-chrome works
    if [ ! -e /usr/bin/google-chrome ]; then ln -s /usr/bin/google-chrome-stable /usr/bin/google-chrome; fi; \
    # Optional: also provide a chromium path if anything probes it
    if [ ! -e /usr/bin/chromium ]; then ln -s /usr/bin/google-chrome-stable /usr/bin/chromium; fi; \
    rm -rf /var/lib/apt/lists/*


# Ensure Shiny Server can read $PORT from env; provide our own config later
ENV PORT=8080
ENV CHROMOTE_CHROME=/usr/bin/chromium

# Workdir for the app
WORKDIR /srv/shiny-server/app

# Copy app source
COPY . /srv/shiny-server/app

# Install required R packages (extend as needed)
RUN R -e "install.packages(c( \
    'shiny','bslib','readr','readxl','shinyjs','openxlsx','shinyjqui', \
    'leaflet','leaflet.extras2','viridislite','shiny.i18n', \
    'webshot2','markdown','shinydashboard','DT', \
    'dplyr','stringr','glue','purrr','tidyr', \
    'duckdb','DBI','shinyWidgets','chromote','htmlwidgets' \
  ), repos='https://cran.rstudio.com/')"

# Supply a clean Shiny Server config that listens on $PORT and serves /srv/shiny-server/app
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Cloud Run sends traffic to $PORT (default 8080)
EXPOSE 8080

# Run Shiny Server
CMD ["/usr/bin/shiny-server"]

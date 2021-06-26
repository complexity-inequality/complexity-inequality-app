# get shiny serves plus tidyverse packages image
FROM rocker/shiny:latest

LABEL mantainer=guilhermeviegas1993@gmail.com

# system dependencies
RUN apt-get update && apt-get install -y \
  sudo \
  pandoc \
  pandoc-citeproc \
  libcurl4-gnutls-dev \
  libcairo2-dev \
  libxt-dev \
  libssl-dev \
  libssh2-1-dev \
  xorg-dev \
  libxml2-dev \
  libmpfr-dev \
  libsasl2-dev \
  libudunits2-dev \
  libgdal-dev

## update system libraries
RUN apt-get update && \
  apt-get upgrade -y && \
  apt-get clean

# basic R packages
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')" # main libs
RUN R -e "install.packages(c('XML', 'devtools', 'bit64', 'xtable'))" # dev libs
RUN R -e "install.packages(c('readr', 'rio'))" # load libs
RUN R -e "install.packages(c('dplyr', 'plyr', 'janitor'))" # Treatment libs
RUN R -e "install.packages(c('ggplot2'))" # viz libs

RUN R -e "install.packages(c('shinydashboard'))"
RUN R -e "install.packages(c('config'))"
RUN R -e "install.packages(c('shinyWidgets'))"
RUN R -e "install.packages(c('sf'))"
RUN R -e "install.packages(c('ggiraph'))"
RUN R -e "install.packages(c('mongolite'))"
RUN R -e "install.packages(c('shinycssloaders'))"
RUN R -e "install.packages(c('DT'))"

# copy necessary files
## app folder
COPY ./R ./R
COPY ./conf ./conf
COPY ./data/options ./data/options
COPY ./data/shp ./data/shp

# RUN mkdir -p ~/R
# COPY ./R ./app/R/
## renv.lock file
# COPY /example-app/renv.lock ./renv.lock

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('./R', host = '0.0.0.0', port = 3838)"]
FROM rocker/shiny-verse:latest

## Install any Linux system dependencies
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev

## Install R libraries
RUN R -e "install.packages(c('shiny', 'remotes', 'colourpicker', 'dplyr', 'ggplot2', 'this.path'))"

## Copy Shiny application files
COPY app.r /srv/shiny-server/
COPY .RData /srv/shiny/.RData
COPY ONFARM-soil-data.csv /srv/shiny-server/ONFARM-soil-data.csv

RUN sudo chown -R shiny:shiny /srv/shiny-server

CMD ["/usr/bin/shiny-server"]
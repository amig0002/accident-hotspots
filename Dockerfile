FROM rocker/shiny:4.1.1
RUN install2.r rsconnect tidyverse shinydashboard leaflet DT MASS ggtern
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R

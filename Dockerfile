FROM rocker/shiny:4.1.1
RUN install2.r rsconnect
WORKDIR /home/shinyusr
COPY app.R app.R 
COPY deploy.R deploy.R
CMD Rscript deploy.R

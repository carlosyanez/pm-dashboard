# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev  \
    nginx \
    apache2-utils 
 

#Generate certificate
   
RUN openssl req -batch -x509 -nodes -days 365 -newkey rsa:2048 \
        -keyout /etc/ssl/private/server.key \
        -out /etc/ssl/private/server.crt    
  

# install R packages required 
# (change it dependeing on the packages you need)
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('semantic.dashboard')"
RUN R -e "install.packages('tidyverse')"
RUN R -e "install.packages('knitr')"
RUN R -e "install.packages('kableExtra')"
RUN R -e "install.packages('vistime')"
RUN R -e "install.packages('lubridate')"
RUN R -e "install.packages('plotly')"
RUN R -e "install.packages('trelloR')"
RUN R -e "install.packages('tempR')"
RUN R -e "install.packages('shinyWidgets')"
RUN R -e "install.packages('plumber')"
RUN R -e "install.packages('leaflet')"
RUN R -e "install.packages('flexdashboard')"
RUN R -e "install.packages('pins')"
RUN R -e "install.packages('tinytex')"
RUN R -e "tinytex::install_tinytex()"

# select port
EXPOSE 80 443

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server
RUN sudo chmod -R 777 /tmp

COPY shiny-server.sh /usr/bin/shiny-server.sh

# run app
CMD ["/usr/bin/shiny-server.sh"]
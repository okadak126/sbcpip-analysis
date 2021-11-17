FROM rocker/verse:4.0.5

MAINTAINER Kai Okada <okadak12@gmail.com>

WORKDIR /home/app

COPY SBCpip /home/sbcpip
COPY pip /home/pip

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    pandoc \
    && rm -rf /var/lib/apt/lists/* \
    && R -e "install.packages('tidyverse', repos = 'http://cran.rstudio.com/');" \
    && R -e "install.packages('markdown', repos = 'http://cran.rstudio.com/');" \
    && R -e "install.packages('shinyWidgets', repos = 'http://cran.rstudio.com/');"
    

RUN R -e "devtools::install_dev_deps('/home/pip', dep = TRUE);"
RUN R -e "devtools::install_dev_deps('/home/sbcpip', dep = TRUE);" 

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/local/lib/R/etc/Rprofile.site

RUN addgroup --system app \
    && adduser --system --ingroup app app

RUN R -e "devtools::build('/home/pip');"
RUN R -e "devtools::install('/home/pip');"
#RUN R -e "devtools::check('/home/pip');"

RUN R -e "devtools::build('/home/sbcpip');"
RUN R -e "devtools::install('/home/sbcpip');"
#RUN R -e "devtools::check('/home/sbcpip');"
RUN R -e "r = getOption('repos');r['CRAN'] = 'http://cran.us.r-project.org';options(repos = r);install.packages('SBCpip');"

COPY SBCpip/inst/webapps/dashboard .
#COPY SBCpip/inst/extdata/platelet_data_sample /home/app
#RUN mkdir /home/app/platelet_logs

RUN chown app:app -R /home/app

USER app
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]

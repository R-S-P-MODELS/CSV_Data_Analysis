FROM openanalytics/r-base

MAINTAINER Rafael Pereira "r.s.p.models@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/',dependencies=TRUE)"

# install dependencies of the Analysis app
RUN R -e "install.packages(c('data.table','plotly','cleanerR','plyr'), repos='https://cloud.r-project.org/',dependencies=TRUE)"
RUN R -e "install.packages(c('parallel'), repos='https://cloud.r-project.org/',dependencies=TRUE)"

# copy the app to the image
#RUN mkdir /root/Exploration
#COPY APPLastVersion.R /root/Exploration
COPY Encontrar_candidatos_dataset_v1.R /root/Exploration
#COPY Rprofile.site /usr/lib/R/etc/
COPY AppLastVersion.R  AppLastVersion.R
COPY Encontrar_candidatos_dataset_v1.R Encontrar_candidatos_dataset_v1.R
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('AppLastVersion.R',port=3838,host='0.0.0.0',launch.browser=FALSE)"]
#CMD ["R", "-e", "shiny::runApp('/home/rafael/Downloads/APPs/DataExploration/APPLastVersion.R')"]

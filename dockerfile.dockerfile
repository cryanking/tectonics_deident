FROM ghcr.io/washu-it-ris/rstudio:4.3.0@sha256:5eb5fc499f43661f962386e117d85cae761df795867a75d45d4f1a5e0f0e29f6
## this base image dervies from "Ubuntu" VERSION="20.04.5 LTS (Focal Fossa)"
## tag: cryanking/tectonics_results:1.1

ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
ARG DEBIAN_FRONTEND=noninteractive
RUN apt-get update --fix-missing && \
apt-get install -y build-essential \
libcurl4-gnutls-dev libxml2-dev  \
apt-utils pandoc-citeproc lmodern libgit2-dev libgit2-28 \
libssh2-1-dev libssl-dev libgit2-dev pkg-config zlib1g-dev && \
apt-get clean


RUN useradd docker \
	&& mkdir /home/docker \
	&& chown docker:docker /home/docker \
	&& addgroup docker staff

RUN mkdir --parents /root/R/x86_64-pc-linux-gnu-library/4.3

## this dependcy is weird and won't install by itself
## RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('git2r'), dependencies=TRUE)"

RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); install.packages(c('groundhog'), dependencies=TRUE); "
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); library(groundhog); set.groundhog.folder('/root/R_groundhog/'); meta.groundhog('2023-12-14')"  ; exit 0

RUN R -e  "library(groundhog); groundhog.library(c('magrittr', 'data.table', 'forcats','readxl', 'sandwich', 'lmtest', 'DescTools' , 'dplyr', 'lubridate', 'tidyr', 'stringr', 'ggplot2', 'xgboost', 'boot', 'matrixStats', 'effectsize', 'randtoolbox', 'gridExtra', 'readr', 'openssl', 'scales' ,'bit64' ) , '2023-12-14') "  ; exit 0
RUN chmod 777 /root

ENTRYPOINT /bin/bash



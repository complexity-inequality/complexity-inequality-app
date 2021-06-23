#!/bin/sh

# Start logging into start.log
exec > start.log 2>&1

docker ps
ls

docker build -t ic-shiny-app:v1 .
docker run -d -p 3838:3838 ic-shiny-app:v1

docker tag ic-shiny-app:v1 guigo13/ic-shiny-app:v1
docker push guigo13/ic-shiny-app:v1

# docker commit container_id guigo13/ic-shiny-app:v1

# in gcloud
docker pull guigo13/ic-shiny-app:v1
docker tag guigo13/ic-shiny-app:v1 gcr.io/icgvdhv1/ic-shiny-app:v1
docker push gcr.io/icgvdhv1/ic-shiny-app:v1


exec > /dev/tty 2>&1 #redirects out to controlling terminal



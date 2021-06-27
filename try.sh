#!/bin/sh
# -------------------------------------------------------------------------
# Start logging into start.log
exec > start.log 2>&1
echo ls ----------------------------------------------------------------------
cd /home/guigo/Documents/projects_dir/08-complexity-inequality/complexity-inequality-app
pwd
ls
echo docker ps ---------------------------------------------------------------
docker ps
echo Setting Credentials -----------------------------------------------------
DockerUser=$(grep 'DockerUser' conf/credentials.yml | cut -f2 -d':' |tr -d "'")
DockerPasswd=$(grep 'DockerPasswd' conf/credentials.yml | cut -f2 -d':' |tr -d "'")
echo docker build ------------------------------------------------------------
docker login -u $DockerUser -p $DockerPasswd
docker build -t ic-shiny-app .
docker stop ci-app
docker rm ci-app
docker run -d -p 3838:3838 --name ci-app ic-shiny-app
docker tag ic-shiny-app guigo13/ic-shiny-app
docker push guigo13/ic-shiny-app


exec > /dev/tty 2>&1 #redirects out to controlling terminal

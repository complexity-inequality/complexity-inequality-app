#!/bin/sh

echo "Running deploy.sh ..." # --------------------------------------------------
echo "Please check deploy.log" # ------------------------------------------------
exec > deploy.log 2>&1 # Start logging into deploy.log
echo "" > deploy.log # Restart deploy.log

echo `date`
echo "ls -a ------------------------------------------------------------------"
cd /home/guigo/Documents/projects_dir/08-complexity-inequality/complexity-inequality-app
pwd
ls -a

echo "docker ps --------------------------------------------------------------"
docker ps

echo "Setting Global variables -----------------------------------------------"
NM_CONTAINER="ci-app-ctn"
NM_IMAGE="ci-app-img"
NM_GCP_PROJ_ID="ci-gcp-proj"
NM_GCP_SERVICE="ci-app-run"
NM_GCP_REGION="us-central1"
QT_MAX_INSTANCES="3"
NM_DOCKER_USER="guigo13"
PORT="3838"

echo "Setting Credentials ----------------------------------------------------"
DockerUser=$(grep 'DockerUser' conf/credentials.yml | cut -f2 -d':' |tr -d "'") # find out a better way to do it
DockerPasswd=$(grep 'DockerPasswd' conf/credentials.yml | cut -f2 -d':' |tr -d "'")

echo "docker build -----------------------------------------------------------"
docker login -u $DockerUser -p $DockerPasswd
docker build -t $NM_IMAGE .

echo "docker stop & rm previous container ------------------------------------"
docker stop $NM_CONTAINER
docker rm $NM_CONTAINER

echo "docker run $NM_CONTAINER -----------------------------------------------"
docker run -d -p 3838:3838 --name $NM_CONTAINER $NM_IMAGE

echo "docker push to dockerhub and gcp ---------------------------------------"
docker tag $NM_IMAGE $NM_DOCKER_USER/$NM_IMAGE
docker push $NM_DOCKER_USER/$NM_IMAGE
echo "image pushed to dockerhub ----------------------------------------------"
# make sure to have (gcloud auth configure-docker)
docker tag $NM_DOCKER_USER/$NM_IMAGE gcr.io/$NM_GCP_PROJ_ID/$NM_IMAGE
docker push gcr.io/$NM_GCP_PROJ_ID/$NM_IMAGE
echo "image pushed to gcr ----------------------------------------------------"

echo "deploy to gcp ----------------------------------------------------------"
gcloud config set project $NM_GCP_PROJ_ID
gcloud run deploy $NM_GCP_SERVICE --image=gcr.io/$NM_GCP_PROJ_ID/$NM_IMAGE --region=$NM_GCP_REGION --max-instances=$QT_MAX_INSTANCES --port=$PORT

echo "Finished deploy.sh ------------------------------------------------------"
exec > /dev/tty 2>&1 #redirects out to controlling terminal

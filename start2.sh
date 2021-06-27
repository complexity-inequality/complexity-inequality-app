DockerUser=$(grep 'DockerUser' conf/credentials.yml | cut -f2 -d':' |tr -d "'")
DockerPasswd=$(grep 'DockerPasswd' conf/credentials.yml | cut -f2 -d':' |tr -d "'")

echo $DockerUser
echo $DockerPasswd

docker login -u $DockerUser -p $DockerPasswd
docker tag ic-shiny-app gcr.io/icgcdhv1/ic-shiny-app
docker push gcr.io/icgvdhv1/ic-shiny-app
docker push gcr.io/icgvdhv1/ic-shiny-app


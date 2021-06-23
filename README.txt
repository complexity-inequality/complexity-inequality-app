docker build -t ic-shiny-app .
docker run -d -p 3838:3838 ic-shiny-app

docker tag ic-shiny-app guigo13/ic-shiny-app
docker push guigo13/ic-shiny-app

# in gcloud
docker pull guigo13/ic-shiny-app
docker tag guigo13/ic-shiny-app gcr.io/icgvdhv1/ic-shiny-app
docker push gcr.io/icgvdhv1/ic-shiny-app

# -------------------------------------------------

# Verifying domain
https://www.youtube.com/watch?v=3PeD23VkwyA

# Google Search Console
https://search.google.com/search-console/welcome

# Map domain to the Google Cloud Run app

VPC Network


# -------------------------------------------------
TO DO

trocar cd_year por year ou period

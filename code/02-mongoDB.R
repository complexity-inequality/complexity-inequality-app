# R-script 02-mongoDB.R


# Reference ---------------------------------------------------------------

# https://jeroen.github.io/mongolite/
# https://rstudio-pubs-static.s3.amazonaws.com/544823_1115c38053934765a7a2aa2131dab5f5.html


# Setup -------------------------------------------------------------------

rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())
options(scipen = 666)


# Packages ----------------------------------------------------------------

if(!require(readr)){install.packages("readr")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(janitor)){install.packages("janitor")}
if(!require(mongolite)){install.packages("mongolite")}
if(!require(readxl)){install.packages("readxl")}
if(!require(reticulate)){install.packages("reticulate")}


# Code --------------------------------------------------------------------

# Set the url path
mongo_credentials <- config::get(file = "conf/globalresources.yml")

# Set db
mongo_set <- mongo(db = "db1", collection = "collection1", url = mongo_credentials$mongoURL, verbose = TRUE)

# Check all collections
mongo(url = mongo_credentials$mongoURL)$run('{"listCollections":1}')$cursor$firstBatch %>% as_tibble()

# Create df
df <- data.frame(a=c(1, 2), b=c(3, 4))

# Insert df
mongo_set$insert(data = df)

# Find df
mongo_set$find()

# Drop df
mongo_set$drop()





# ICDB --------------------------------------------------------------------


# mun
mongo_credentials <- config::get(file = "conf/globalresources.yml")
mongo_set <- mongo(db = "db1", collection = "colec_mun", url = mongo_credentials$mongoURL, verbose = TRUE)
mongo(url = mongo_credentials$mongoURL)$run('{"listCollections":1}')$cursor$firstBatch %>% as_tibble()
colec_mun <- mongo_set$find()
product_input="eci_ref"
df <- mongo_set$find(paste0('{"product" : ', paste0('"', product_input, '"'), '}'))

# uf
mongo_credentials <- config::get(file = "conf/globalresources.yml")
mongo_set <- mongo(db = "db1", collection = "colec_uf", url = mongo_credentials$mongoURL, verbose = TRUE)
mongo(url = mongo_credentials$mongoURL)$run('{"listCollections":1}')$cursor$firstBatch %>% as_tibble()
colec_mun <- mongo_set$find()
product_input="eci_ref"
df <- mongo_set$find(paste0('{"product" : ', paste0('"', product_input, '"'), '}'))
tail(df)
rio::export(df, "df_uf_eci.csv")
sapply(df, unique)

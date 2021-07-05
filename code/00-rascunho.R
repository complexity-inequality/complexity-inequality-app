rm(list = ls())
library(config)
library(mongolite)

mongo_credentials <- config::get(file = "conf/credentials.yml")
mongo_credentials
mongo_credentials$MongoDB$mongoURL


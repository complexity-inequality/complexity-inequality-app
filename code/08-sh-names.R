# R-script db_uf.R


# References --------------------------------------------------------------

# https://grapher.network/blog/
# https://pacha.dev/blog/
# https://pacha.dev/
# https://cran.r-project.org/web/packages/economiccomplexity/economiccomplexity.pdf

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
if(!require(vroom)){install.packages("vroom")}
if(!require(economiccomplexity)){install.packages("economiccomplexity")}


# Code --------------------------------------------------------------------

NCM_SH <- read_delim("data/NCM_SH.csv", ";", escape_double = FALSE, trim_ws = TRUE)
names(NCM_SH)
NCM_SH %>% select(CO_SH2, NO_SH2_POR) %>% distinct() %>% filter(CO_SH2%in%c("95", "96", "97", "98", "99"))
NCM_SH %>% select(CO_SH2, NO_SH2_POR) %>% distinct() %>% View()

mongo_credentials <- config::get(file = "conf/globalresources.yml")
mongo_set <- mongo(db = "db1", collection = "colec_uf_exp_eci", url = mongo_credentials$mongoURL, verbose = TRUE)
# mongo(url = mongo_credentials$mongoURL)$run('{"listCollections":1}')$cursor$firstBatch %>% as_tibble()
mongo_data <- mongo_set$find()

mongo_data %>% filter(nm_uf=="Santa Catarina") %>% arrange(desc(value))

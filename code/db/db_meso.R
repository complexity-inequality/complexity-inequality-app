# R-script db_meso.R


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

mongo_credentials <- config::get(file = "conf/globalresources.yml")
mongo_set <- mongo(db = "db1", collection = "colec_mun", url = mongo_credentials$mongoURL, verbose = TRUE)
colec_mun <- mongo_set$find()

colec_meso <- colec_mun %>% 
  dplyr::group_by(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

source(file = "code/functions/fct_add_eci.R")
df_meso <- fct_add_eci(df = colec_meso, cd_ref = "cd_meso")


# Insert ------------------------------------------------------------------

source(file = "code/functions/fct_insertmongodb.R")
fmongo_insert(df = df_meso, nm_db = "db1", nm_collec = "colec_meso")



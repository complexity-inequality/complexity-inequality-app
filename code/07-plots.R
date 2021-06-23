# R-script 07-plots.R


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

# Set the url path
mongo_credentials <- config::get(file = "conf/globalresources.yml")

# Set db
mongo_set <- mongo(db = "db1", collection = "colec_meso_exp_eci", url = mongo_credentials$mongoURL, verbose = TRUE)
mongo_set <- mongo(db = "db1", collection = "colec_uf", url = mongo_credentials$mongoURL, verbose = TRUE)
# Check all collections
mongo(url = mongo_credentials$mongoURL)$run('{"listCollections":1}')$cursor$firstBatch %>% as_tibble()

# Find df
mongo_data <- mongo_set$find()

df1 <- mongo_data %>% filter(product=="eci")

# shp
shp_ufs <- sf::st_read("data/shp/shp_meso/")

# Join
dff_shp <- dplyr::left_join(df1, shp_ufs) %>% sf::st_sf()
class(dff_shp)


ggplot2::ggplot(dff_shp, ggplot2::aes(fill=value))+
  ggplot2::geom_sf(color="black", size=.2)+
  ggplot2::scale_fill_gradient(low="white", high="blue")+
  ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
  ggplot2::theme_void()




ggplot2::ggplot(df1)+
  ggplot2::geom_boxplot(ggplot2::aes(x=sg_rg, y=value))
  ggplot2::labs(title = "", caption = "", y = "", x = "")+
  ggplot2::theme_void()

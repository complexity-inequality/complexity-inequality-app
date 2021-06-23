# R-script db_construction.R


# References --------------------------------------------------------------

# https://grapher.network/blog/
# https://pacha.dev/blog/
# https://pacha.dev/
# https://cran.r-project.org/web/packages/economiccomplexity/economiccomplexity.pdf

# Method of Reflections is the one used by Hidalgo and Hausmann

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())
options(scipen = 666)
mongo_credentials <- config::get(file = "conf/globalresources.yml")

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


# Functions ---------------------------------------------------------------

source(file = "code/functions/data_loc.R")
source(file = "code/functions/fct_insertmongodb.R")
source(file = "code/functions/fct_add_eci.R")

# Code --------------------------------------------------------------------

# Getting BR location info
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  dplyr::distinct()

# Loading exp data
exp <- vroom::vroom(file = "data/EXP_COMPLETA_MUN2/EXP_COMPLETA_MUN.csv") %>% 
  suppressMessages() %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(exp=dplyr::if_else(is.na(vl_fob), 0, vl_fob)) %>% 
  dplyr::mutate("cd_sh2" = substr(sh4, 1, 2)) %>%
  dplyr::rename(
    "sg_uf"="sg_uf_mun",
    "cd_mun"="co_mun",
    "cd_year"="co_ano",
    "cd_sh4"="sh4"
  ) %>% 
  dplyr::select(cd_mun, sg_uf, cd_sh2, cd_sh4, cd_year, exp)

# Fixing those dumb ass mistakes :::::::::::::::::::::: make it within dplyr pipe
exp[which(exp$sg_uf=="SP"), "cd_mun"] = exp[which(exp$sg_uf=="SP"), "cd_mun"]+100000 # SP
exp[which(exp$sg_uf=="GO"), "cd_mun"] = exp[which(exp$sg_uf=="GO"), "cd_mun"]-100000 # GO
exp[which(exp$sg_uf=="MS"), "cd_mun"] = exp[which(exp$sg_uf=="MS"), "cd_mun"]-200000 # MS
exp[which(exp$sg_uf=="DF"), "cd_mun"] = exp[which(exp$sg_uf=="DF"), "cd_mun"]-100000 # DF

exp1 <- exp %>% 
  dplyr::group_by(cd_mun, sg_uf, cd_year, cd_sh2) %>%
  dplyr::summarise(exp = sum(exp)) %>% 
  dplyr::ungroup(); rm(exp)

exp2 <- exp1 %>% 
  dplyr::mutate(
    cd_mun=as.character(cd_mun),
    cd_year=as.character(cd_year)
  ) %>% 
  dplyr::left_join(., br_loc, by = c("cd_mun", "sg_uf")) %>% 
  stats::na.omit() %>% 
  dplyr::select(cd_mun, nm_mun, cd_meso, nm_meso, cd_rgint, nm_rgint, cd_micro, nm_micro, cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, cd_sh2, exp)

exp_t <- exp2 %>% 
  dplyr::group_by(cd_mun, sg_uf, cd_sh2) %>% 
  dplyr::summarise(exp=sum(exp)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    cd_year="1997-2021"
  ) %>%
  dplyr::left_join(., br_loc, by = c("cd_mun", "sg_uf")) %>% 
  dplyr::select(cd_mun, nm_mun, cd_meso, nm_meso, cd_rgint, nm_rgint, cd_micro, nm_micro, cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, cd_sh2, exp)

exp_t0 <- exp2 %>% 
  dplyr::group_by(cd_mun, sg_uf) %>% 
  dplyr::summarise(exp=sum(exp)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    cd_sh2="00",
    cd_year="1997-2021"
  ) %>%
  dplyr::left_join(., br_loc, by = c("cd_mun", "sg_uf")) %>% 
  dplyr::select(cd_mun, nm_mun, cd_meso, nm_meso, cd_rgint, nm_rgint, cd_micro, nm_micro, cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, cd_sh2, exp)

colec_mun <- dplyr::bind_rows(exp2, exp_t, exp_t0) %>% 
  dplyr::mutate(cd_sh2=paste0("sh", cd_sh2)) %>% 
  dplyr::mutate(
    cd_mun=as.character(cd_mun),
    cd_meso=as.character(cd_meso),
    cd_rgint=as.character(cd_rgint),
    cd_micro=as.character(cd_micro),
    cd_rgime=as.character(cd_rgime),
    cd_uf=as.character(cd_uf),
    cd_rg=as.character(cd_rg),
    product=cd_sh2,
    value=exp
  ) %>%
  dplyr::select(cd_mun, nm_mun, cd_meso, nm_meso, cd_rgint, nm_rgint, cd_micro, nm_micro, cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, product, value)

# Insert mun info into MongoDB
fmongo_insert(df = colec_mun, nm_db = "db1", nm_collec = "colec_mun")

# Get mun info from MongoDB
# mongo_set <- mongolite::mongo(db = "db1", collection = "colec_mun", url = mongo_credentials$mongoURL, verbose = TRUE)
# (colec_mun <- mongo_set$find())


# db_uf -------------------------------------------------------------------

# Grouping by uf
df_uf <- colec_mun %>% 
  dplyr::group_by(cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
ll_uf <- list()
for(i in unique(df_uf$cd_year)){
  ll_uf[[i]] <- fct_add_eci(
    df = df_uf, 
    reg = "cd_uf", 
    ano=i
  )
}
colec_uf <- dplyr::bind_rows(df_uf, ll_uf)

# Insert uf info into MongoDB
fmongo_insert(df = colec_uf, nm_db = "db1", nm_collec = "colec_uf")

# Get uf info from MongoDB
# mongo_set <- mongolite::mongo(db = "db1", collection = "colec_uf", url = mongo_credentials$mongoURL, verbose = TRUE)
# (colec_uf <- mongo_set$find())

# db_meso -----------------------------------------------------------------

# Grouping by meso
df_meso <- colec_mun %>% 
  dplyr::group_by(cd_meso, nm_meso, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
ll_meso <- list()
for(i in unique(df_meso$cd_year)){
  ll_meso[[i]] <- fct_add_eci(
    df = df_meso, 
    reg = "cd_meso", 
    ano=i
  )
}
colec_meso <- dplyr::bind_rows(df_meso, ll_meso)

# Insert meso info into MongoDB
fmongo_insert(df = colec_meso, nm_db = "db1", nm_collec = "colec_meso")

# Get meso info from MongoDB
# mongo_set <- mongolite::mongo(db = "db1", collection = "colec_meso", url = mongo_credentials$mongoURL, verbose = TRUE)
# (colec_meso <- mongo_set$find())

# db_micro ----------------------------------------------------------------

# Grouping by micro
df_micro <- colec_mun %>% 
  dplyr::group_by(cd_micro, nm_micro, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
ll_micro <- list()
for(i in unique(df_micro$cd_year)){
  ll_micro[[i]] <- fct_add_eci(
    df = df_micro, 
    reg = "cd_micro", 
    ano=i
  )
}
colec_micro <- dplyr::bind_rows(df_micro, ll_micro)

# Insert micro info into MongoDB
fmongo_insert(df = colec_micro, nm_db = "db1", nm_collec = "colec_micro")

# Get micro info from MongoDB
# mongo_set <- mongolite::mongo(db = "db1", collection = "colec_micro", url = mongo_credentials$mongoURL, verbose = TRUE)
# (colec_micro <- mongo_set$find())

# db_rgint ----------------------------------------------------------------

# Grouping by rgint
df_rgint <- colec_mun %>% 
  dplyr::group_by(cd_rgint, nm_rgint, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
ll_rgint <- list()
for(i in unique(df_rgint$cd_year)){
  ll_rgint[[i]] <- fct_add_eci(
    df = df_rgint, 
    reg = "cd_rgint", 
    ano=i
  )
}
colec_rgint <- dplyr::bind_rows(df_rgint, ll_rgint)

# Insert rgint to MongoDB
fmongo_insert(df = colec_rgint, nm_db = "db1", nm_collec = "colec_rgint")

# Get rgint info from MongoDB
# mongo_set <- mongolite::mongo(db = "db1", collection = "colec_rgint", url = mongo_credentials$mongoURL, verbose = TRUE)
# (colec_rgint <- mongo_set$find())

# db_rgime ----------------------------------------------------------------

# Grouping by rgime
df_rgime <- colec_mun %>% 
  dplyr::group_by(cd_rgime, nm_rgime, cd_uf, nm_uf, sg_uf, cd_rg, nm_rg, sg_rg, cd_year, product) %>% 
  dplyr::summarise(value=sum(value)) %>% 
  dplyr::ungroup()

# Adding ECI to df
ll_rgime <- list()
for(i in unique(df_rgime$cd_year)){
  ll_rgime[[i]] <- fct_add_eci(
    df = df_rgime, 
    reg = "cd_rgime", 
    ano=i
  )
}
colec_rgime <- dplyr::bind_rows(df_rgime, ll_rgime)

# Insert rgime to df
fmongo_insert(df = colec_rgime, nm_db = "db1", nm_collec = "colec_rgime")

# Get rgime info from MongoDB
# mongo_set <- mongolite::mongo(db = "db1", collection = "colec_rgime", url = mongo_credentials$mongoURL, verbose = TRUE)
# (colec_rgime <- mongo_set$find())


# More --------------------------------------------------------------------


# options -----------------------------------------------------------------



# Check plot --------------------------------------------------------------

# reac_query
colec = paste0("colec_", "meso")
mongo_set <- mongolite::mongo(db = "db1", collection = colec, url = mongo_credentials$mongoURL, verbose = TRUE)
df <- mongo_set$find()
df <- mongo_set$find(paste0('{"product" : ', paste0('"', "eci_ref_norm", '"'), '}'))
df <- mongo_set$find(paste0('{"product" : ', paste0('"', "eci_ref_norm", '"'), ', "cd_year" : ', paste0('"', "2013", '"'), '}'))

# react_df
shp_df <- sf::st_read("data/shp/shp_uf/")
df_shp <- dplyr::full_join(
  df,
  shp_df
) %>% sf::st_sf()

# output$plot1
ggplot2::ggplot(df_shp)+
  # ggplot2::geom_sf(ggplot2::aes(0), color="black", size=.13)+
  ggplot2::geom_sf(ggplot2::aes(fill=value), color="black", size=.2)+
  ggplot2::scale_fill_gradient(low="white", high="blue")+
  ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
  ggplot2::theme_void()

# draft -------------------------------------------------------------------

colec_rgime

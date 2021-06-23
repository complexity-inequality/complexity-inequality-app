# R-script 06-shp_handling.R


# References --------------------------------------------------------------

# https://grapher.network/blog/
# https://pacha.dev/blog/
# https://pacha.dev/
# https://cran.r-project.org/web/packages/economiccomplexity/economiccomplexity.pdf


# Observations ------------------------------------------------------------

# SHP must have ONLY cd. Do not insert nm so the joins wont work properly

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
if(!require(economiccomplexity)){install.packages("economiccomplexity")}


# Code --------------------------------------------------------------------

# ufs ---------------------------------------------------------------------
shp_uf <- sf::st_read("data/shp/BR_UF_2020/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_uf = as.character(cd_uf),
    nm_rg = as.character(nm_regiao),
    sg_uf = as.character(sigla_uf)
  ) %>% 
  dplyr::mutate(
    sg_rg=if_else(
      nm_rg=="Norte", "N", 
      if_else(
        nm_rg=="Nordeste", "NE",
        if_else(
          nm_rg=="Sudeste", "SE", 
          if_else(
            nm_rg=="Sul", "S",
            if_else(
              nm_rg=="Centro-oeste", "CO", "Outro"
            )
          )
        )
      )
    )
  ) %>% 
  mutate(nm_rg=if_else(nm_rg=="Centro-oeste", "Centro-Oeste", nm_rg)) %>% 
  mutate(cd_uf=as.character(cd_uf), nm_uf=as.character(nm_uf), sg_uf=as.character(sg_uf), nm_rg=as.character(nm_rg), sg_rg=as.character(sg_rg))

shp_uf_f <- shp_uf[, c("cd_uf", "nm_uf", "sg_uf", "nm_rg", "sg_rg")] %>% sf::st_sf()

unlink("data/shp/shp_uf/", recursive = T)
dir.create("data/shp/shp_uf")
sf::st_write(shp_uf_f, "data/shp/shp_uf/shp_uf.shp")


# meso --------------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  select(cd_meso, cd_uf, sg_uf, cd_rg, sg_rg) %>% 
  mutate(cd_meso=as.character(cd_meso)) %>% 
  distinct()

shp_meso <- sf::st_read("data/shp/BR_Mesorregioes_2020/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_meso = as.character(cd_meso),
    sg_uf = as.character(sigla_uf)
  ) %>% dplyr::left_join(., br_loc)

shp_meso_f <- shp_meso[, c("cd_meso", "nm_meso", "cd_uf", "sg_uf", "cd_rg", "sg_rg")] %>% 
  dplyr::mutate(cd_meso=as.character(cd_meso), cd_uf=as.character(cd_uf), cd_rg=as.character(cd_rg)) %>% 
  sf::st_sf()

unlink("data/shp/shp_meso/", recursive = T)
dir.create("data/shp/shp_meso")
sf::st_write(shp_meso_f, "data/shp/shp_meso/shp_meso.shp")

# shp_micro ---------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  select(cd_micro, cd_uf, sg_uf, cd_rg, sg_rg) %>% 
  mutate(cd_micro=as.character(cd_micro)) %>% 
  distinct()

shp_micro <- sf::st_read("data/shp/BR_Microrregioes_2020/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_micro = as.character(cd_micro),
    sg_uf = as.character(sigla_uf)
  ) %>% dplyr::left_join(., br_loc)

shp_micro_f <- shp_micro[, c("cd_micro", "nm_micro", "cd_uf", "sg_uf", "cd_rg", "sg_rg")] %>% 
  dplyr::mutate(cd_micro=as.character(cd_micro), cd_uf=as.character(cd_uf), cd_rg=as.character(cd_rg)) %>% 
  sf::st_sf()

unlink("data/shp/shp_micro/", recursive = T)
dir.create("data/shp/shp_micro")
sf::st_write(shp_micro_f, "data/shp/shp_micro/shp_micro.shp")


# shp_rgint -----------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  select(cd_rgint, cd_uf, sg_uf, cd_rg, sg_rg) %>% 
  mutate(cd_rgint=as.character(cd_rgint)) %>% 
  distinct()

shp_rgint <- sf::st_read("data/shp/BR_RG_Intermediarias_2020/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_rgint = as.character(cd_rgint),
    sg_uf = as.character(sigla_uf)
  ) %>% dplyr::left_join(., br_loc)

shp_rgint_f <- shp_rgint[, c("cd_rgint", "cd_uf", "sg_uf", "cd_rg", "sg_rg")] %>% 
  dplyr::mutate(cd_rgint=as.character(cd_rgint), cd_uf=as.character(cd_uf), cd_rg=as.character(cd_rg)) %>% 
  sf::st_sf()

unlink("data/shp/shp_rgint/", recursive = T)
dir.create("data/shp/shp_rgint")
sf::st_write(shp_rgint_f, "data/shp/shp_rgint/shp_rgint.shp")

# shp_rgime ---------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
br_loc <- data_loc(sg_uf_br) %>% 
  select(cd_rgime, cd_uf, sg_uf, cd_rg, sg_rg) %>% 
  mutate(cd_rgime=as.character(cd_rgime)) %>% 
  distinct()

shp_rgime <- sf::st_read("data/shp/BR_RG_Imediatas_2020/") %>%
  janitor::clean_names() %>%
  sf::st_set_crs(4326) %>%
  dplyr::mutate(
    cd_rgime = as.character(cd_rgi),
    sg_uf = as.character(sigla_uf)
  ) %>% dplyr::left_join(., br_loc)

shp_rgime_f <- shp_rgime[, c("cd_rgime", "cd_uf", "sg_uf", "cd_rg", "sg_rg")] %>% 
  dplyr::mutate(cd_rgime=as.character(cd_rgime), cd_uf=as.character(cd_uf), cd_rg=as.character(cd_rg)) %>% 
  sf::st_sf()

unlink("data/shp/shp_rgime/", recursive = T)
dir.create("data/shp/shp_rgime")
sf::st_write(shp_rgime_f, "data/shp/shp_rgime/shp_rgime.shp")


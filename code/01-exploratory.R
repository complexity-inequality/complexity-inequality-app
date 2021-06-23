# R-script 01-exploratory.R

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
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(janitor)){install.packages("janitor")}
if(!require(sf)){install.packages("sf")}
if(!require(sp)){install.packages("sp")}
if(!require(st)){install.packages("st")}
if(!require(leaflet)){install.packages("leaflet")}
if(!require(mongolite)){install.packages("mongolite")}
if(!require(readxl)){install.packages("readxl")}
if(!require(janitor)){install.packages("janitor")}
if(!require(spdep)){install.packages("spdep")}
if(!require(vroom)){install.packages("vroom")}
if(!require(jtools)){install.packages("jtools")}
if(!require(GGally)){install.packages("GGally")}
if(!require(ggfortify)){install.packages("ggfortify")}
if(!require(reticulate)){install.packages("reticulate")}



# Functions ---------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sul_loc <- data_loc(c("SC", "PR", "RS"))


# Data --------------------------------------------------------------------

tabela3543 <- read_csv("data/tabela3543.csv", skip = 5) %>% 
  janitor::clean_names() %>% 
  dplyr::rename("area"="areas_gerais_especificas_e_detalhadas_de_formacao_do_curso_de_nivel_mais_elevado_concluido") %>% 
  dplyr::mutate(
    "cd_mun" = as.character(cod),
    "qt_habitantes" = as.numeric(ifelse(total=="-", 0, total))
  ) %>% 
  dplyr::select("cd_mun", "area", "qt_habitantes")


# https://www.gov.br/produtividade-e-comercio-exterior/pt-br/assuntos/comercio-exterior/estatisticas/base-de-dados-bruta
exp <- vroom::vroom(file = "data/EXP_2019_MUN.csv") %>% 
  suppressMessages() %>% 
  janitor::clean_names() %>% 
  dplyr::filter(sg_uf_mun%in%c("SC", "PR", "RS")) %>%
  dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
  dplyr::mutate("cd_sh2" = substr(sh4, 1, 2)) %>%
  # dplyr::filter(cd_sh2==cd_sh2) %>%
  dplyr::group_by(co_mun, cd_sh2) %>%
  dplyr::summarise(exp_fob = sum(exp_fob)) %>% 
  dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(cd_mun, cd_sh2, exp_fob)
                
sh_table <- read_delim("data/sh_table.csv", 
                       "\"", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE) %>% 
  dplyr::rename("cd_sh2"="X1", "nm_sh2"="X3") %>% 
  dplyr::select("cd_sh2", "nm_sh2") %>% 
  dplyr::mutate("cd_sh2"=as.character(cd_sh2))

shp_micro <- sf::st_read("data/BR_Microrregioes_2020/") %>%
  janitor::clean_names() %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(cd_micro = as.integer(cd_micro))
# plot(shp_micro['cd_micro'])

# Joins -------------------------------------------------------------------

df1 <- dplyr::left_join(sul_loc, tabela3543, by = "cd_mun") %>% 
  dplyr::group_by(cd_micro, nm_micro, area) %>% 
  dplyr::summarise(qt_habitantes = sum(qt_habitantes)) %>% 
  dplyr::ungroup()

df2 <- dplyr::left_join(sul_loc, exp, by = "cd_mun") %>% 
  dplyr::group_by(cd_micro, nm_micro, cd_sh2) %>% 
  dplyr::summarise(exp = sum(exp_fob)) %>% 
  dplyr::ungroup()


df3 <- dplyr::full_join(df1, df2, by = c("cd_micro", "nm_micro")) %>% 
  dplyr::mutate(
    log_qt_habitantes = log(qt_habitantes),
    log_exp = log(exp),
  )

df4 <- df3 %>% 
  dplyr::group_by(cd_micro, nm_micro, area) %>% 
  dplyr::summarise(
    qt_habitantes = sum(qt_habitantes),
    cd_sh2 = "00",
    exp = sum(exp, na.rm = T)
  ) %>% 
  dplyr::mutate(
    log_qt_habitantes = log(qt_habitantes),
    log_exp = log(exp)
  )

df5 <- bind_rows(df4, df3) %>% 
  dplyr::arrange(cd_micro) %>% 
  dplyr::filter(
    log_exp>0,
    log_qt_habitantes>0
  ) %>% 
  na.omit()

df6 <- df5 %>% 
  dplyr::filter(
    area=="Total",
    cd_sh2=="00",
  )

source(file = "code/functions/fct_model.R")
m0 <- fct_model(df6)

# lembrar de limpar area vector with janitor
lista_df = list()
for(i in unique(df5$area)){
  for(j in unique(df5$cd_sh2)){
    dd <- df5 %>% 
      dplyr::filter(
        area==i, 
        cd_sh2==j
      )
    if(nrow(dd)>10){
      lista_df[[paste0(i, "_", j)]] <- fct_model(dd)
    }
  }
}
# lista_df
length(lista_df)

res_df <- tibble::tibble("area"=vector(), "cd_sh2"=vector(), "b1_est"=vector())
for(i in 1:length(names(lista_df))){
  res_df[i, "area"] = lista_df[[i]]["area"][[1]][1]
  res_df[i, "cd_sh2"] = lista_df[[i]]["cd_sh2"][[1]][1]
  res_df[i, "b1_est"] = lista_df[[i]]["b1_est"][[1]][1]
}

res_df2 <- res_df %>% tidyr::spread(area, b1_est)
# range(res_df2$Total)
# View(res_df2)

library(esquisse)
# esquisse::esquisser()
library(reshape2)
library(ggplot2)

res_df3 <- dplyr::inner_join(sh_table, res_df2, by = "cd_sh2") %>% 
  janitor::clean_names() %>% 
  dplyr::arrange(desc(total)) %>% 
  dplyr::select(nm_sh2, everything())
# rio::export(res_df3, "data/matrix_elasticidade_educacao_superior_exportacoes.csv", )


ggplot(melt(res_df3), aes(x=factor(cd_sh2, levels = res_df3$cd_sh2), y=variable, fill=value)) + 
  geom_tile() +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7")+
  labs(
    title = "Matriz das elasticidades educação superior sobre exportações", 
    y = "Tipo de educação superior",
    x = "SH2"
  )

ggplot(melt(res_df3), aes(x=variable, y=factor(cd_sh2, levels = res_df3$cd_sh2), fill=value)) + 
  geom_tile() +
  scale_fill_gradient(low = "#132B43", high = "#56B1F7")+
  labs(
    title = "Matriz das elasticidades educação superior sobre exportações", 
    x = "Tipo de educação superior",
    y = "SH2"
  )

df_shp <- dplyr::left_join(lista_df[[names(lista_df)[1]]], shp_micro, by = c("cd_micro", "nm_micro")) %>% st_sf()
# plot(df_shp["residuals"])
ggplot(df_shp)+
  geom_sf(aes(fill=residuals), color="black", size=.2)+
  scale_fill_gradient(low = "#132B43", high = "#56B1F7")


res_df3[which(res_df3$cd_sh2==26), "nm_sh2"]

# limpar area vector string to remove accent and spaces
# plot maps
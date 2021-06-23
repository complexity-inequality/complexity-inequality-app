# R-script 03-economiccomplexity.R


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
if(!require(economiccomplexity)){install.packages("economiccomplexity")}


# Code --------------------------------------------------------------------


# exploratory
df <- economiccomplexity::world_trade_avg_1998_to_2000
names(df) <- c("cc", "pp", "vv")
df <- economiccomplexity::balassa_index(data = df, discrete = F)
df <- economiccomplexity::balassa_index(data = economiccomplexity::world_trade_avg_1998_to_2000, discrete = F)
dim(df)
df["bra", ]

class(economiccomplexity_output$balassa_index)
co <- complexity_measures(economiccomplexity_output$balassa_index)

complexity_measures(matrix(1, 2, 3, 4))



# try_for_real ------------------------------------------------------------

source(file = "code/functions/data_loc.R")
sul_loc <- data_loc(c("SC", "PR", "RS"))

shp_micro <- sf::st_read("data/BR_Microrregioes_2020/") %>%
  janitor::clean_names() %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(cd_micro = as.integer(cd_micro))

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
  dplyr::select(cd_mun, cd_sh2, exp_fob)# %>% 
  # dplyr::rename("country"="cd_mun", "product"="cd_sh2", "value"="exp_fob")

bain <- economiccomplexity::balassa_index(data = exp)
cmbain <- complexity_measures(bain)
cmbain$complexity_index_country

df <- exp %>%
  dplyr::left_join(., sul_loc, by="cd_mun") %>% 
  dplyr::group_by(cd_micro, nm_micro, cd_sh2) %>%
  dplyr::summarise(exp=sum(exp_fob)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(log_exp=log(exp)) %>% 
  dplyr::arrange(desc(exp)) %>% 
  dplyr::rename("country"="cd_micro", "product"="cd_sh2", "value"="log_exp")

bain <- economiccomplexity::balassa_index(data = df)
cmbain <- complexity_measures(bain)
cmbain$complexity_index_country

dff <- df %>% 
  dplyr::rename("cd_micro"="country", "cd_sh2"="product", "log_exp"="value") %>% 
  dplyr::group_by(cd_micro, nm_micro) %>% 
  dplyr::summarise(exp=sum(exp)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    eci = as.numeric(cmbain$complexity_index_country),
    log_eci = log(as.numeric(cmbain$complexity_index_country))
  ) %>% 
  dplyr::arrange(dplyr::desc(eci))

dff

dff_shp <- dplyr::left_join(dff, shp_micro, by = c("cd_micro", "nm_micro")) %>% sf::st_sf()
class(dff_shp)
plot(dff_shp["eci"])
plot(dff_shp["log_eci"])

ggplot(dff_shp)+
  geom_sf(aes(fill=eci), color="black", size=.2)+
  scale_fill_gradient(low="blue", high="red")


nb <- spdep::poly2nb(dff_shp, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
spdep::moran.test(dff_shp$eci, lw)
spdep::moran.test(dff_shp$log_eci, lw)

# ECI_BR ------------------------------------------------------------------

sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

source(file = "code/functions/data_loc.R")
br_loc <- data_loc(sg_uf_br)

shp_micro <- sf::st_read("data/BR_Microrregioes_2020/") %>%
  janitor::clean_names() %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(cd_micro = as.integer(cd_micro))

exp <- vroom::vroom(file = "data/EXP_2018_MUN.csv") %>% 
  suppressMessages() %>% 
  janitor::clean_names() %>% 
  # dplyr::filter(sg_uf_mun%in%sg_uf_br) %>%
  dplyr::mutate(exp_fob=if_else(is.na(vl_fob), 0, vl_fob)) %>% 
  dplyr::mutate("cd_sh2" = substr(sh4, 1, 2)) %>%
  dplyr::group_by(co_mun, cd_sh2) %>%
  dplyr::summarise(exp_fob = sum(exp_fob)) %>% 
  dplyr::mutate(cd_mun=as.character(co_mun)) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(cd_mun, cd_sh2, exp_fob)

df <- exp %>%
  dplyr::left_join(., br_loc, by="cd_mun") %>% 
  dplyr::group_by(cd_micro, nm_micro, cd_sh2) %>%
  dplyr::summarise(exp=sum(exp_fob)) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(log_exp=log(exp)) %>% 
  dplyr::arrange(desc(exp)) %>% 
  dplyr::rename("country"="cd_micro", "product"="cd_sh2", "value"="log_exp") %>% 
  na.omit()

bain <- economiccomplexity::balassa_index(data = df)
cmbain <- complexity_measures(bain)
cmbain$complexity_index_country

dff <- df %>% 
  dplyr::rename("cd_micro"="country", "cd_sh2"="product", "log_exp"="value") %>% 
  dplyr::group_by(cd_micro, nm_micro) %>% 
  dplyr::summarise(exp=sum(exp)) %>% 
  dplyr::ungroup()# %>%

ecidf <- data.frame(
  cd_micro=as.integer(names(cmbain$complexity_index_country)),
  eci=cmbain$complexity_index_country
)
df_eci <- left_join(dff, ecidf, by = "cd_micro") %>% 
  dplyr::arrange(dplyr::desc(eci))


dff_shp <- dplyr::left_join(df_eci, shp_micro, by = c("cd_micro", "nm_micro")) %>% sf::st_sf()
class(dff_shp)
plot(dff_shp["eci"])

# plot(df_shp["residuals"])
ggplot(dff_shp)+
  geom_sf(aes(fill=eci), color="black", size=.2)+
  scale_fill_gradient(low="blue", high="red")


nb <- spdep::poly2nb(dff_shp, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
spdep::moran.test(dff_shp$eci, lw)


# ECI_UF_BR ---------------------------------------------------------------

sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

source(file = "code/functions/data_loc.R")
br_loc <- data_loc(sg_uf_br) %>% 
  group_by(nm_uf, cd_uf) %>% 
  summarise()

shp_ufs <- sf::st_read("data/BR_UF_2020/") %>%
  janitor::clean_names() %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(cd_uf = as.integer(cd_micro))

exp <- readxl::read_excel("data/EXP_2000_2021_20210527.xlsx") %>% 
  mutate(exp = purrr::reduce(select(., ends_with("(US$)")), `+`)) %>% 
  janitor::clean_names() %>% 
  dplyr::rename("nm_uf"="uf_do_produto", "cd_sh2"="codigo_sh2", "nm_sh2"="descricao_sh2") %>% 
  dplyr::select(nm_uf, cd_sh2, nm_sh2, exp) %>% 
  dplyr::arrange(desc(exp))

shp_ufs <- sf::st_read("data/BR_UF_2020/") %>%
  janitor::clean_names() %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(cd_uf = as.integer(cd_uf))

df <- exp %>%
  dplyr::left_join(., br_loc, by = "nm_uf") %>% 
  dplyr::mutate(log_exp=log(exp)) %>% 
  dplyr::arrange(desc(exp)) %>% 
  dplyr::rename("country"="nm_uf", "product"="cd_sh2", "value"="log_exp") %>% 
  na.omit()

bain <- economiccomplexity::balassa_index(data = df)
cmbain <- complexity_measures(bain)
cmbain$complexity_index_country

dff <- df %>%
  dplyr::rename("nm_uf"="country", "cd_sh2"="product", "log_exp"="value")

ecidf <- data.frame(
  nm_uf=names(cmbain$complexity_index_country),
  eci=cmbain$complexity_index_country
)
df_eci <- left_join(ecidf, dff, by="nm_uf") %>%
  dplyr::arrange(dplyr::desc(eci))


dff_shp <- dplyr::left_join(df_eci, shp_ufs) %>% sf::st_sf()
class(dff_shp)
plot(dff_shp["eci"])
plot(dff_shp["eci"])


# plot(df_shp["residuals"])
ggplot(dff_shp)+
  geom_sf(aes(fill=eci), color="black", size=.2)+
  scale_fill_gradient(low="blue", high="red")


nb <- spdep::poly2nb(dff_shp, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
spdep::moran.test(dff_shp$eci, lw)


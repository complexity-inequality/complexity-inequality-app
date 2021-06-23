# R-script 05-eci_br.R


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

sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
# sg_uf_br <- c("SC", "RS", "PR")

source(file = "code/functions/data_loc.R")
br_loc <- br_loc %>% select(cd_uf, sg_uf, nm_uf, cd_rg, sg_rg, nm_rg) %>% distinct()
br_loc <- data_loc(sg_uf_br) %>% 
  group_by(nm_uf, cd_uf) %>% 
  summarise() %>% 
  ungroup()

exp <- readxl::read_excel("data/EXP_2000_2021_20210527.xlsx") %>% 
  dplyr::mutate(exp = purrr::reduce(select(., ends_with("(US$)")), `+`)) %>% 
  janitor::clean_names() %>% 
  dplyr::rename("nm_uf"="uf_do_produto", "cd_sh2"="codigo_sh2", "nm_sh2"="descricao_sh2") %>% 
  dplyr::select(nm_uf, cd_sh2, nm_sh2, exp)

shp_ufs <- sf::st_read("data/shp/shp_ufs/")

df <- exp %>%
  dplyr::left_join(., br_loc, by = "nm_uf") %>% 
  dplyr::mutate(log_exp=log(exp)) %>%
  dplyr::rename("country"="nm_uf", "product"="cd_sh2", "value"="log_exp") %>% 
  na.omit()

bain <- economiccomplexity::balassa_index(data = df)
cmbain <- economiccomplexity::complexity_measures(balassa_index = bain)
# cmbain$complexity_index_country

dff <- df %>%
  dplyr::rename(
    "nm_uf"="country", 
    "cd_sh2"="product", 
    "log_exp"="value"
  )

ecidf <- data.frame(
  nm_uf=names(cmbain$complexity_index_country),
  eci=cmbain$complexity_index_country
)
# df_eci <- left_join(ecidf, dff, by="nm_uf") %>%
#   dplyr::arrange(dplyr::desc(eci))


dff_shp <- dplyr::left_join(ecidf, shp_ufs) %>% sf::st_sf()
class(dff_shp)
plot(dff_shp["eci"])

nb <- spdep::poly2nb(dff_shp, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
m1 <- round(spdep::moran.test(dff_shp$eci, lw)$estimate[[1]], 2)

# colnames(dff_shp)
rownames(dff_shp) <- dff_shp$cd_uf

ggplot(dff_shp, aes(fill=eci, tooltip = paste0("<b>Nome: </b>", nm_uf, "<br/>", "<b>Valor: </b>", round(eci, 3)), data_id = cd_uf))+
  ggiraph::geom_sf_interactive(color="black", size=.2)+
  # geom_sf(aes(fill=eci), color="black", size=.2)+
  scale_fill_gradient(low="white", high="blue")#+
  # annotate(x = -44.878609, y = -28.916854, geom = "text", label=paste0("M1=", "0.666"))+
  # labs(title = "Gráfico teste", caption = "eci.app.br", y = "Latitude", x = "Longitude")
ggiraph::girafe(ggobj = gce)

rownames(dff_shp) <- dff_shp$cd_uf
gg2 <- ggplot2::ggplot(dff_shp, ggplot2::aes(fill=eci, tooltip=cd_uf, data_id = cd_uf))+
  ggiraph::geom_sf_interactive(color="black", size=.2)+
  # ggplot2::geom_sf(color="black", size=.2)+
  ggplot2::scale_fill_gradient(low="white", high="blue")+
  ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
  ggplot2::theme_void()
ggiraph::girafe(ggobj = gg2)



# source(file = "code/functions/fct_insertmongodb.R")
# fmongo_insert(df = ecidf, nm_db = "db1", nm_collec = "br_uf_raweci")

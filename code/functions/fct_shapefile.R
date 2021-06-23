# .shp Shapefile dos municípios de SC (IBGE-Geociencias-divisao_do_territorio)
# https://www.ibge.gov.br/geociencias/downloads-geociencias.html
# organizacao_do_territorio > malhas_territoriais > municipio_2019 > UFs > UF > uf_municipios.zip
get_sf <- function(){
  sc_shp <- sf::st_read("data/sc_municipios/") %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326)
  # pr_shp <- sf::st_read("data/raw/pr_municipios/") %>%
  #   janitor::clean_names() %>% 
  #   sf::st_set_crs(4326)
  # rs_shp <- sf::st_read("data/raw/rs_municipios/") %>%
  #   janitor::clean_names() %>% 
  #   sf::st_set_crs(4326)
  # sul <- list(sc_shp, pr_shp, rs_shp)
  # sul_sf <- sf::st_as_sf(data.table::rbindlist(sul))
  return(sc_shp)
}


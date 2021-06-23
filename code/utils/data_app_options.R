library(dplyr)

# source(file = "code/functions/data_loc.R")
# br_loc <- data_loc(sg_uf_br)


loc_options <- data.frame(
  "cd_rg"=c("0", "1", "2", "3", "4", "5"), 
  "sg_rg"=c("BR", "N", "NE", "SE", "S", "CO"), 
  "nm_rg"=c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
)

rio::export(loc_options, "data/option_loc.csv")



# option_stats ------------------------------------------------------------

ncm_sh <- readr::read_delim("data/NCM_SH.csv", ";") %>% 
  janitor::clean_names() %>% 
  dplyr::select(co_sh2, no_sh2_por, no_sh2_esp, no_sh2_ing, co_ncm_secrom, no_sec_por, no_sec_esp, no_sec_ing)

ncm_sh %>% 
  filter(co_sh2=="94") %>% 
  select(no_sh2_ing) %>% 
  unique()

df <- data.frame(
  "cd_tema"=c("eci", "eci", "eci", "eci", "edu", "exp", "exp", "trab"),
  "nm_tema"=c("ECI", "ECI", "ECI", "ECI", "Educação", "Exportações", "Exportações", "Trabalho"),
  "cd_stat"=c("eci", "log_eci", "log_eci_log", "eci_log", "edu", "85", "02", "trab"),
  "nm_stat"=c("ECI", "Log ECI", "Log ECI Log", "ECI Log", "edu", "85 - Maquinários elétricos", "02 - Carne", "trab")
)
rio::export(df, "data/option_stats.csv")



# Dados das localizações e divisões regionais do Brasil através da API de serviços IBGE

data_loc <- function(ufs){
  req <- httr::GET(paste0('https://servicodados.ibge.gov.br/api/v1/localidades/estados/', paste0(ufs, collapse = '|'), '/municipios'))
  if(req$status_code == 200){
    res <- httr::content(x = req, type = 'text', encoding = 'UTF-8') %>% 
      jsonlite::fromJSON(txt = ., flatten = TRUE) %>% 
      as.data.frame(x = .) %>% 
      janitor::clean_names(dat = .) %>% 
      dplyr::rename(
        'cd_mun'='id',
        'nm_mun'='nome',
        'cd_micro'='microrregiao_id',
        'nm_micro'='microrregiao_nome',
        'cd_meso'='microrregiao_mesorregiao_id',
        'nm_meso'='microrregiao_mesorregiao_nome',
        # 'cd_uf'='microrregiao_mesorregiao_uf_id',
        # 'sg_uf'='microrregiao_mesorregiao_uf_sigla',
        # 'nm_uf'='microrregiao_mesorregiao_uf_nome',
        # 'cd_rg'='microrregiao_mesorregiao_uf_regiao_id',
        # 'sg_rg'='microrregiao_mesorregiao_uf_regiao_sigla',
        # 'nm_rg'='microrregiao_mesorregiao_uf_regiao_nome',
        'cd_rgime'='regiao_imediata_id',
        'nm_rgime'='regiao_imediata_nome',
        'cd_rgint'='regiao_imediata_regiao_intermediaria_id',
        'nm_rgint'='regiao_imediata_regiao_intermediaria_nome',
        'cd_uf'='regiao_imediata_regiao_intermediaria_uf_id',
        'sg_uf'='regiao_imediata_regiao_intermediaria_uf_sigla',
        'nm_uf'='regiao_imediata_regiao_intermediaria_uf_nome',
        'cd_rg'='regiao_imediata_regiao_intermediaria_uf_regiao_id',
        'sg_rg'='regiao_imediata_regiao_intermediaria_uf_regiao_sigla',
        'nm_rg'='regiao_imediata_regiao_intermediaria_uf_regiao_nome'
      ) %>% 
      dplyr::mutate('cd_mun'=as.character(cd_mun)) %>% 
      dplyr::select('cd_mun', 'nm_mun', 'cd_micro', 'nm_micro', 'cd_meso', 'nm_meso', 'cd_rgint', 'nm_rgint', 'cd_rgime', 'nm_rgime', 'cd_uf', 'sg_uf', 'nm_uf', 'cd_rg', 'sg_rg', 'nm_rg')
  } else{
    warning('Error: Bad request')
  }
  return(res)
}

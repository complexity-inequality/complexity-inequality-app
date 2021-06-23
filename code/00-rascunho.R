# Module UI

#' @title   mod_leafletmod_ui and mod_leafletmod_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_leafletmod
#'
#' @keywords internal
#' @export
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import leaflet
#' @import leaflet.extras
#' @import shinydashboard
#' @import shinycssloaders
#' @import sf
mod_leafletmod_ui <- function(id) {
  ns <- NS(id)
  tagList(fluidPage(
    fluidRow(
      leaflet::leafletOutput(ns("map"), height = "942") %>% shinycssloaders::withSpinner(color = "#f15c22"),
      absolutePanel(
        top = 80,
        fixed = T,
        right = 20,
        shinydashboard::box(
          style="width:750px; min-height:40px",
          collapsible = T,
          collapsed = F,
          width = "1000px",
          fluidPage(
            fluidRow(
              column(6,
                     uiOutput(outputId = ns("select_city")),
                     uiOutput(outputId = ns("nalunos_ui"))
              ),
              column(6,
                     shinyWidgets::pickerInput(
                       inputId = ns("filter_tipo_ui"),
                       label = "Tipo de entidade",
                       choices = list(
                         "Federal" = "Federal",
                         "Estadual" = "Estadual",
                         "Municipal" = "Municipal",
                         "Particular normal" = "Privada_Particular_Normal",
                         "Particular filantrópica" = "Privada_Filantropica_Normal",
                         "Privada comunitária normal" = "Privada_Comunitaria_Normal",
                         "Privada particular sistema S" = "Privada_Particular_Sistema_S",
                         "Privada confessional normal" = "Privada_Confessional_Normal",
                         # "Privada comunitária sistema S" = "Privada_Comunitaria_Sistema_S",
                         "Privada filantrópica sistema S" = "Privada_Filantropica_Sistema_S"
                       ),
                       selected = c("Privada_Particular_Normal", "Privada_Filantropica_Normal", "Privada_Comunitaria_Normal", "Privada_Particular_Sistema_S", "Privada_Confessional_Normal", "Privada_Comunitaria_Sistema_S", "Privada_Filantropica_Sistema_S"
                       ),
                       multiple = T,
                       options = shinyWidgets::pickerOptions(
                         actionsBox = TRUE,
                         selectAllText = "Selecionar todos",
                         deselectAllText = "Selecionar nenhum" 
                       )
                     ),
                     br(),
                     uiOutput(outputId = ns("segfilter_ui"))
              )
            )
          ),
          fluidPage(
            hr(),
            fluidRow(
              column(6,
                     br(),
                     checkboxInput(inputId = ns("inputsf"), label = "Camada de contorno", value = T)
              ),
              column(6,
                     shinyWidgets::pickerInput(
                       inputId = ns("ployvar_serv"), 
                       label = "Comparar bairros por", 
                       choices = c(
                         "Similaridade Neo Transformacional" = "similarity_neo_transf",
                         "Similaridade Neo Excelência" = "similarity_neo_excel",
                         # "Similaridade" = "similarity_media_bairro_customizada",
                         "Similaridade Nova Transformacional" = "similarity_new_transf",
                         "Similaridade Nova Excelência" = "similarity_new_excel",
                         "Similaridade Transformacional" = "similarity_transf",
                         "Similaridade Excelência" = "similarity_excel",
                         "similaridade Transformacional Teste" = "similarity_transf_teste",
                         "similaridade Execelência Teste" = "similarity_excel_teste",
                         "similaridade Execelência Teste 2" = "similarity_excel_teste2",
                         "MarketShare" = "marketshare_no_municipio_total_bairro",
                         "Média de salários mínimos per capta" = "mean_rend_mensal_per_capita_sal_min_media_bairro",
                         "Média da idade" = "media_idade_media_bairro",
                         "Média score Excelência" = "score_relevancia_excelencia_media_bairro",
                         "Média score Transformacional" = "score_relevancia_transformacional_media_bairro",
                         "Quantidade média de alunos" = "qt_alunos_total_bairro_mean",
                         "Quantidade média de docentes" = "qt_docentes_total_bairro_mean",
                         "Quantidade média de funcionários" = "qt_funcionarios_total_bairro_mean",
                         "Quantidade total de alunos" = "qt_alunos_total_bairro_sum",
                         "Quantidade total de docentes" = "qt_docentes_total_bairro",
                         "Quantidade total de funcionários" = "qt_funcionarios_total_bairro",
                         "Taxa de alunos de outros municípios" = "tx_alunos_de_outros_municipios_media_bairro",
                         "Pop. faixa etária até 9" = "pop_faixa_etaria_ate_9",
                         "Pop. faixa etária 10-14" = "pop_faixa_etaria_10_a_14",
                         "Pop. faixa etária 15-19" = "pop_faixa_etaria_15_a_19",
                         "Fluxo de alunos" = "fluxo_total"
                         # "dom_faixa_renda_a" = "dom_faixa_renda_a"               # mt falha estrutural
                       ), 
                       selected = "marketshare_no_municipio_total_bairro",
                       multiple = F
                     ),
                     conditionalPanel(
                       condition = paste0("input['", ns("ployvar_serv"), "'] == 'similarity_media_bairro_customizada'"),
                       shinyWidgets::pickerInput(
                         inputId = ns("simvar"),
                         label = "Variaveis para customização",
                         choices = list(
                           "Score Excelência" = "score_relevancia_excelencia_media_bairro",
                           "Score Transformacional" = "score_relevancia_transformacional_media_bairro",
                           "Renda média mensal per capita" = "mean_rend_mensal_per_capita_sal_min_media_bairro",
                           "Média da etapa de ensino" = "media_etapa_ensino",
                           "Média de idade dos alunos" = "media_idade",
                           "Quantidade média de alunos" = "qt_alunos_total_bairro_mean",
                           "Quantidade média de funcionários" = "qt_funcionarios_total_bairro_mean",
                           "Quantidade média de docentes" = "qt_docentes_total_bairro_mean",
                           "Quantidade de alunos por funcionário" = "qt_alunos_funcionarios",
                           "Quantidade de alunos por docente" = "qt_alunos_docentes",
                           "Uso de transporte público" = "media_uso_transp_pub"
                         ),
                         selected = c(
                           "media_etapa_ensino",
                           "qt_alunos_total_bairro_mean",
                           "qt_docentes_total_bairro_mean",
                           "mean_rend_mensal_per_capita_sal_min_media_bairro"
                         ),
                         multiple = T,
                         options = shinyWidgets::pickerOptions(
                           actionsBox = TRUE,
                           selectAllText = "Selecionar todos",
                           deselectAllText = "Selecionar nenhum" 
                         )
                       ),
                       uiOutput(outputId = ns("unidades_eleva_similaridade_ui"))
                     )
              )
            )
          ),
          fluidPage(
            hr(),
            fluidRow(
              column(6,
                     br(),
                     checkboxInput(inputId = ns("inputhm"), label = "Camada de calor", value = F)
              ),
              column(6,
                     uiOutput(outputId = ns("dropdown_var"))
              )
            )
          ),
          fluidPage(
            hr(),
            fluidRow(
              column(6,
                     br(),
                     checkboxInput(inputId = ns("inputpt"), label = "Camada de pontos", value = T)
              ),
              column(6,
                     shinyWidgets::pickerInput(
                       inputId = ns("points"),
                       label = "Mostrar pontos por",
                       choices = list(
                         "Federal" = "Federal",
                         "Estadual" = "Estadual",
                         "Municipal" = "Municipal",
                         "Particular normal" = "Privada_Particular_Normal",
                         "Particular filantrópica" = "Privada_Filantropica_Normal",
                         "Privada comunitária normal" = "Privada_Comunitaria_Normal",
                         "Privada particular sistema S" = "Privada_Particular_Sistema_S",
                         "Privada confessional normal" = "Privada_Confessional_Normal",
                         # "Privada comunitária sistema S" = "Privada_Comunitaria_Sistema_S",
                         "Privada filantrópica sistema S" = "Privada_Filantropica_Sistema_S"
                       ),
                       selected = c("Federal", "Estadual", "Municipal", "Privada_Particular_Normal", "Privada_Filantropica_Normal",
                                    "Privada_Comunitaria_Normal",
                                    "Privada_Particular_Sistema_S", "Privada_Confessional_Normal", "Privada_Comunitaria_Sistema_S", "Privada_Filantropica_Sistema_S"
                       ),
                       multiple = T,
                       options = shinyWidgets::pickerOptions(
                         actionsBox = TRUE,
                         selectAllText = "Selecionar todos",
                         deselectAllText = "Selecionar nenhum" 
                       )
                     ))
            )
          ),
          fluidPage(
            hr(),
            column(2, br(), downloadLink(ns("downloadData"), actionButton(inputId = ns("act"), label = strong("Exportar")))),
            column(4, br(), actionButton(ns("goButton"), width = "100%", strong("Processar"), #width = "320px", 
                                         style="display: inline-block; align-items: center;justify-content: center; float:center;")),
            column(6, uiOutput(outputId = ns("type_graph")))
          )
        )
      )
    )
  ))
}

# Module Server

#' @rdname mod_leafletmod
#' @export
#' @keywords internal

mod_leafletmod_server <- function(input, output, session) {
  ns <- session$ns
  
  # Arquivos default --------------------------------------------------------
  mongo_credentials <- config::get(file = "conf/globalresources.yml")$mongoURL
  mongo_collection <- "tabela_final_v35"
  
  
  # Filtros -----------------------------------------------------------------
  output$type_graph <- renderUI({
    shinyWidgets::pickerInput(
      ns("pick_type_map"),
      "Tipo do mapa",
      c(
        "Estrada aberta" = "OpenStreetMap",
        "Satélite" = "Esri.WorldImagery",
        "Aquarela" = "Stamen.Watercolor"
      ),
      # c(do.call("rbind", leaflet::providers)),
      multiple = F,
      selected = "OpenStreetMap"
    )
  })
  
  output$select_city <- renderUI({
    shinyWidgets::pickerInput(
      ns("cidade_atual"),
      label = "Município da análise",
      choices = sort(getCidadeChoices(mongoCredentials = mongo_credentials, collection = "geolocalizacao_cidades_expansao")),
      multiple = FALSE,
      selected = "Florianópolis - SC"
    )
  })
  
  output$dropdown_var <- renderUI({
    shinyWidgets::pickerInput(
      inputId = ns("heat_var"), 
      label = "Intensidade do calor por", 
      choices = c(
        "Peso atribuído" = "peso_tp_dependencia",
        "Score Excelência" = "score_relevancia_excelencia",
        "Score Transformacional" = "score_relevancia_transformacional",
        "MarketShare" = "marketshare_no_municipio"
      ), 
      selected = "peso_tp_dependencia",
      multiple = F)
  })
  
  output$segfilter_ui <- renderUI({
    shinyWidgets::pickerInput(
      inputId = ns("segfilter"), 
      label = "Segmento das entidades", 
      choices = c("EI"="ei", "EF1"="ef1", "EF2"="ef2", "EM"="em", "EMI"="emi", "EJA"="eja", "Outros"="outros"), 
      selected = c("ei", "ef1", "ef2", "em"),
      multiple = T,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Selecionar todos",
        deselectAllText = "Selecionar nenhum" 
      )
    )
  })
  
  output$nalunos_ui <- renderUI({
    sliderInput(
      inputId = ns("nalunos_serv"), 
      label = "Intervalo de número de alunos", 
      min = 0, max = 5000,ticks = F, 
      value = c(0,5000), step = 10)
  })
  
  output$unidades_eleva_similaridade_ui <- renderUI({
    uni_eleva <- mongolite::mongo(url = mongo_credentials,collection = mongo_collection)$find('{"escola_eleva":1}')$no_entidade
    shinyWidgets::pickerInput(
      inputId = ns("unidades_eleva_similaridade_serv"), label = "Unidades modelo Eleva", 
      choices = sort(uni_eleva), 
      selected = sort(uni_eleva)[1:3], multiple = T,
      options = shinyWidgets::pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Selecionar todos",
        deselectAllText = "Selecionar nenhum" 
      )
    )
  })
  
  # Tabelas reativas --------------------------------------------------------
  fronteira_cidade <- reactive({
    print(paste0("Cidade Selecionada: ", input$cidade_atual))
    getLatLongInicial(mongoCredential = mongo_credentials,
                      inputCidest = input$cidade_atual,
                      mongoCollection = "geolocalizacao_cidades_expansao")
  })
  
  reac1 <- reactive({
    # Codigo do municipio para filtrar a cidade nas outras funcoes
    cod_municipio <-
      getCodMunicipio(mongoCredentials = mongo_credentials,
                      inputCidade = input$cidade_atual)
  })
  
  reac2 <- reactive({
    cod_municipio <- reac1()
    if(input$nalunos_serv[2]>=5000){    # Regra pra que o filtro nalunos considero todo valor acima de 5000 como 100000
      nalmax <- 100000
    }else{nalmax <- input$nalunos_serv[2]}
    
    # tabela com os dados dos markers e heatmap
    inep_filtrado_cities <-
      getDadosEntidades(
        mongoCredentials = mongo_credentials,
        codMunicipio = cod_municipio,
        collection = mongo_collection
      ) %>% 
      filter(no_tipo_entidade %in% input$filter_tipo_ui) %>%
      filter_at(vars(!!dplyr::quo(input$segfilter)), any_vars(.==1)) %>%
      filter(qt_alunos >= !!dplyr::quo(input$nalunos_serv[1])) %>% 
      filter(qt_alunos <= !!dplyr::quo(nalmax))
  })
  
  reac3 <- reactive({
    cod_municipio <- reac1()
    inep_filtrado_cities <- reac2()
    # Tabela com os scores por bairro
    shape_files_plot <- computeDistance(
      mongoCredential = mongo_credentials,
      codMunicipio = cod_municipio,
      collection = mongo_collection,
      inputSegmento = input$segfilter,
      nalunos = input$nalunos_serv,
      vsim = input$simvar,
      unel = input$unidades_eleva_similaridade_serv,
      tipoe = input$filter_tipo_ui
    ) %>% glimpse()
  })
  
  reac4 <- reactive({
    dt <- reac3()
    st_geometry(dt) <- NULL
    dt
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {paste0("dados_eleva_expansao_", Sys.Date(), ".xlsx")},
    content = function(file) {writexl::write_xlsx(reac4() %>% 
                                                    dplyr::select(-qt_alunos_total_bairro_mean, -qt_alunos_total_bairro_mean, -qt_docentes_total_bairro_mean, -qt_funcionarios_total_bairro_mean, -qt_alunos_docentes, -qt_alunos_funcionarios, -score_relevancia_excelencia_media_bairro, -score_relevancia_transformacional_media_bairro, -similarity_media_bairro_customizada) %>% 
                                                    as_tibble(), path = file)}
  )
  
  # Renderizar mapa ---------------------------------------------------------
  event <- eventReactive(input$goButton, {
    
    cod_municipio <- reac1()
    inep_filtrado_cities <- reac2()
    shape_files_plot <- reac3()
    
    normalize <- function(x) {
      x <- x[!is.na(x)]
      return ((x - min(x)) / (max(x) - min(x)))
    }
    
    pal <- colorNumeric(c("aquamarine", "blue4"), 0:1) # Função cria codigos de cores para intensidade.
    
    
    labs <- paste0("<b>Bairro: </b>", as.character(shape_files_plot$NM_BAIRRO), "<br/>", 
                   "<b>População: </b>", round(as.numeric(shape_files_plot$populacao_bairro), digits = 0), " habitantes", "<br/>",  #  (2008)
                   "<b>Quantidade total de alunos: </b>", round(as.numeric(shape_files_plot$qt_alunos_total_bairro_sum), digits = 0), "<br/>",
                   "<b>Alunos em escolas privadas: </b>", round(as.numeric(shape_files_plot$aep_bairro), digits = 0), "<br/>",
                   "<b>MarketShare absoluto entre privadas: </b>", round(as.numeric(shape_files_plot$sum_bairro_mktshare_privada), digits = 2), "%", "<br/>",
                   "<b>Fluxo de alunos: </b>", round(as.numeric(shape_files_plot$fluxo_total), digits = 2), "<br/>",
                   "<b>Média das notas ENEM do bairro: </b>", round(as.numeric(shape_files_plot$media_geral_enem_cr_bairro), digits = 2), "<br/>",
                   # "<b>Domicílios c/ faixa de renda A: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_a_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda B1: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_b1_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda B2: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_b2_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda C1: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_c1_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda C2: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_c2_bairro), digits = 0), "<br/>",
                   "<b>% domicílios na faixa A + B: </b>", round(as.numeric(shape_files_plot$dom_pc_ab), digits = 2), " %", "<br/>",
                   "<b>% domicílios na faixa B + C: </b>", round(as.numeric(shape_files_plot$dom_pc_bc), digits = 2), " %", "<br/>",
                   "<b>Domicílios na faixa total: </b>", round(as.numeric(shape_files_plot$dom_total), digits = 2), "<br/>",
                   "<b>Similaridade Nova Excelência: </b>", round(as.numeric(shape_files_plot$similarity_new_excel), digits = 2), "<br/>",
                   "<b>Similaridade Nova Transformacional: </b>", round(as.numeric(shape_files_plot$similarity_new_transf), digits = 2), "<br/>",
                   "<b>Média de salários mínimos per capta: </b>", round(as.numeric(shape_files_plot$renda_mediana), digits = 2)
                   
                   # "<b>Número de escolas: </b>", round(as.numeric(shape_files_plot$numero_de_escolas), digits = 0), "<br/>",
                   # "<b>Média do score excelência: </b>", round(as.numeric(shape_files_plot$score_relevancia_excelencia_media_bairro_vis), digits = 2), "<br/>",
                   # "<b>Média do score transformacional: </b>", round(as.numeric(shape_files_plot$score_relevancia_transformacional_media_bairro_vis), digits = 2), "<br/>",
                   # "<b>MarketShare relativo: </b>", as.character(round(shape_files_plot$mktshare_relativo, 2)), "%", "<br/>",
                   # "<b>MarketShare absoluto: </b>", round(as.numeric(shape_files_plot$mktshare_absoluto), digits = 2), "%", "<br/>",
                   # "<b>MarketShare absoluto entre públicas: </b>", round(as.numeric(shape_files_plot$sum_bairro_mktshare_publica), digits = 2), "%", "<br/>",
                   # "<b>Média da idade dos alunos: </b>", round(as.numeric(shape_files_plot$media_idade), digits = 2), " anos", "<br/>",
                   # "<b>Quantidade de alunos p/ km2: </b>", round(as.numeric(shape_files_plot$qt_alunos_pkq), digits = 0), "<br/>",
                   # "<b>Quantidade média de alunos p/ escola: </b>", round(as.numeric(shape_files_plot$qt_alunos_total_bairro_mean), digits = 2), "<br/>",
                   # "<b>Quantidade média de docentes p/ escola: </b>", round(as.numeric(shape_files_plot$qt_docentes_total_bairro_mean), digits = 2), "<br/>",
                   # "<b>Quantidade média de funcionários p/ escola: </b>", round(as.numeric(shape_files_plot$qt_funcionarios_total_bairro_mean), digits = 2), "<br/>",
                   # "<b>Domicílios (2018): </b>", round(as.numeric(shape_files_plot$domicilios_bairro), digits = 0), " residências", "<br/>",
                   # "<b>Área do bairro: </b>", round(as.numeric(shape_files_plot$area_bairro), digits = 2), " km2", "<br/>",
                   # "<b>Densidade demográfica: </b>", round(as.numeric(shape_files_plot$densidade_demografica_bairro), digits = 0), " habitantes p/ km2", "<br/>",
                   # "<b>Potencial de consumo p/ cursos regulares: </b>", round(as.numeric(shape_files_plot$pot_consumo_estimado_por_categoria_2019_cursos_regulares_bairro), digits = 0), "<br/>",
                   # "<b>População faixa etária até 9 anos: </b>", round(as.numeric(shape_files_plot$pop_faixa_etaria_ate_9_bairro), digits = 0), "<br/>",
                   # "<b>População faixa etária de 10 a 14 anos: </b>", round(as.numeric(shape_files_plot$pop_faixa_etaria_10_a_14_bairro), digits = 0), "<br/>",
                   # "<b>População faixa etária de 15 a 19 anos: </b>", round(as.numeric(shape_files_plot$pop_faixa_etaria_15_a_19_bairro), digits = 0), "<br/>",
                   # "<b>População faixa etária de 0 a 19 anos: </b>", round(as.numeric(shape_files_plot$pop_fgaixa_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda A: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_a_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda B1: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_b1_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda B2: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_b2_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda C1: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_c1_bairro), digits = 0), "<br/>",
                   # "<b>Domicílios c/ faixa de renda C2: </b>", round(as.numeric(shape_files_plot$dom_faixa_renda_c2_bairro), digits = 0), "<br/>",
                   # "<b>População Econômicamente Ativa - PEA: </b>", round(as.numeric(shape_files_plot$pea_dia_2017_bairro), digits = 0), "<br/>",
                   # "<b>Taxa de alunos de outros municípios: </b>", round(as.numeric(shape_files_plot$tx_alunos_de_outros_municipios_media_bairro_vis), digits = 2), "<br/>",
    )
    
    popup <- paste(
      "<b>Nome:</b>",
      inep_filtrado_cities[["no_entidade"]],
      "<br><b>Categoria:</b>",
      gsub(pattern = "_", replacement = " ", x = inep_filtrado_cities[["no_tipo_entidade"]]),
      "<br><b>Código INEP:</b>",
      inep_filtrado_cities[["co_entidade"]],
      "<br><b>Quantidade de alunos:</b>",
      inep_filtrado_cities[["qt_alunos"]],
      "<br><b>Quantidade de docentes:</b>",
      inep_filtrado_cities[["qt_docentes"]],
      "<br><b>Quantidade de funcionários:</b>",
      inep_filtrado_cities[["qt_funcionarios"]],
      "<br><b>Média da idade dos alunos:</b>",
      paste0(round(as.numeric(inep_filtrado_cities[["media_idade"]]), digits = 2), " anos"),
      "<br><b>Taxa de alunos de outro municípios:</b>",
      paste0(round(as.numeric(inep_filtrado_cities[["tx_alunos_de_outros_municipios"]])*100, digits = 2), "%"),
      "<br><b>Marketshare no município:</b>",
      paste0(round(as.numeric(inep_filtrado_cities[["marketshare_no_municipio"]]), digits = 2), "%"),
      "<br><b>Marketshare na categoria:</b>",
      paste0(round(as.numeric(inep_filtrado_cities[["mkt_share_publicas"]])+as.numeric(inep_filtrado_cities[["mkt_share_privadas"]]), digits = 2), "%"),
      "<br><b>Score Excelência:</b>",
      round(as.numeric(inep_filtrado_cities[["score_relevancia_excelencia"]]), digits = 2),
      "<br><b>Score Transformacional:</b>",
      round(as.numeric(inep_filtrado_cities[["score_relevancia_transformacional"]]), digits = 2),
      "<br><b>Nota no ENEM:</b>",
      round(as.numeric(inep_filtrado_cities[["media_geral_enem_cr"]]), digits = 2)
    )
    
    defineCircleColor <- function(inep_filtrado_cities){
      lapply(inep_filtrado_cities$no_tipo_entidade, function(x){
        if(x == "Municipal"){
          return ("lightblue")
        } else if (x == "Estadual"){
          return ("blue")
        } else if(x == "Federal"){
          return ("darkblue")
        } else if(x == "Privada_Filantropica_Normal"){
          return("lightred")
        } else if(x == "Privada_Particular_Normal"){
          return ("darkred")
        } else if (x == "Privada_Comunitaria_Normal"){
          return ("red")
        } else if(x == "Privada_Particular_Sistema_S"){
          return ("darkgreen")
        } else if(x == "Privada_Confessional_Normal"){
          return("red")
        } else if(x == "Privada_Comunitaria_Sistema_S"){
          return ("green")
        } else if (x == "Privada_Filantropica_Sistema_S"){
          return ("lightgreen")
        } else {
          return ("aqua")
        }
      })
    }
    
    mapa_all <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(input$pick_type_map) %>%
      leaflet::fitBounds(
        fronteira_cidade()[1],
        fronteira_cidade()[2],
        fronteira_cidade()[3],
        fronteira_cidade()[4]
      ) %>%
      leaflet::addPolygons(
        data = shape_files_plot,
        fill = TRUE,
        stroke = TRUE,
        # https://rstudio.github.io/leaflet/colors.html
        fillOpacity = 0.60,
        color = ~pal(normalize(shape_files_plot[[as.character(input$ployvar_serv)]])),
        weight = 1,
        label = lapply(labs, htmltools::HTML)
      ) %>%
      leaflet.extras::addHeatmap(
        data = inep_filtrado_cities,
        lng = ~ lng,
        lat = ~ lat,
        intensity = ~ input$heat_var,
        blur = 20,
        max = 1,
        radius = 15
      ) %>%
      leaflet::addAwesomeMarkers(
        data = inep_filtrado_cities[which(inep_filtrado_cities$no_tipo_entidade %in% c(input$points)),],
        lat = ~ lat,
        lng = ~ lng,
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 100,
          chunkedLoading = "true",
          disableClusteringAtZoom = 15,
          chunkInterval = "50ms",
          chunkDelay = "10ms"
        ),
        icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion', 
          defineCircleColor(inep_filtrado_cities[which(inep_filtrado_cities$no_tipo_entidade %in% c(input$points)),])
        ),
        group = "Concorrentes",
        popup = popup
      )
    
    #
    mapa_no_points <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(input$pick_type_map) %>%
      leaflet::fitBounds(
        fronteira_cidade()[1],
        fronteira_cidade()[2],
        fronteira_cidade()[3],
        fronteira_cidade()[4]
      ) %>%
      leaflet::addPolygons(
        data = shape_files_plot,
        fill = TRUE,
        stroke = TRUE,
        # https://rstudio.github.io/leaflet/colors.html
        fillOpacity = 0.60,
        color = ~pal(normalize(shape_files_plot[[as.character(input$ployvar_serv)]])),
        weight = 1,
        label = lapply(labs, htmltools::HTML)
      ) %>% 
      leaflet.extras::addHeatmap(
        data = inep_filtrado_cities,
        lng = ~ lng,
        lat = ~ lat,
        intensity = ~ input$heat_var,
        blur = 20,
        max = 1,
        radius = 15
      )
    
    #
    mapa_shm <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(input$pick_type_map) %>%
      leaflet::fitBounds(
        fronteira_cidade()[1],
        fronteira_cidade()[2],
        fronteira_cidade()[3],
        fronteira_cidade()[4]
      ) %>%
      leaflet::addPolygons(
        data = shape_files_plot,
        fill = TRUE,
        stroke = TRUE,
        # https://rstudio.github.io/leaflet/colors.html
        fillOpacity = 0.60,
        color = ~pal(normalize(shape_files_plot[[as.character(input$ployvar_serv)]])),
        weight = 1,
        label = lapply(labs, htmltools::HTML)
      ) %>% 
      leaflet::addAwesomeMarkers(
        data = inep_filtrado_cities[which(inep_filtrado_cities$no_tipo_entidade %in% c(input$points)),],
        lat = ~ lat,
        lng = ~ lng,
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 100,
          chunkedLoading = "true",
          disableClusteringAtZoom = 15,
          chunkInterval = "50ms",
          chunkDelay = "10ms"
        ),
        icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion', 
          defineCircleColor(inep_filtrado_cities[which(inep_filtrado_cities$no_tipo_entidade %in% c(input$points)),])
        ),
        group = "Concorrentes",
        popup = popup
      )
    
    #
    mapa_spoly <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(input$pick_type_map) %>%
      leaflet::fitBounds(
        fronteira_cidade()[1],
        fronteira_cidade()[2],
        fronteira_cidade()[3],
        fronteira_cidade()[4]
      ) %>% 
      leaflet.extras::addHeatmap(
        data = inep_filtrado_cities,
        lng = ~ lng,
        lat = ~ lat,
        intensity = ~ input$heat_var,
        blur = 20,
        max = 1,
        radius = 15
      ) %>% 
      leaflet::addAwesomeMarkers(
        data = inep_filtrado_cities[which(inep_filtrado_cities$no_tipo_entidade %in% c(input$points)),],
        lat = ~ lat,
        lng = ~ lng,
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 100,
          chunkedLoading = "true",
          disableClusteringAtZoom = 15,
          chunkInterval = "50ms",
          chunkDelay = "10ms"
        ),
        icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion', 
          defineCircleColor(inep_filtrado_cities[which(inep_filtrado_cities$no_tipo_entidade %in% c(input$points)),])
        ),
        group = "Concorrentes",
        popup = popup
      )
    
    #
    mapa_pontos <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(input$pick_type_map) %>%
      leaflet::fitBounds(
        fronteira_cidade()[1],
        fronteira_cidade()[2],
        fronteira_cidade()[3],
        fronteira_cidade()[4]
      ) %>% 
      leaflet::addAwesomeMarkers(
        data = inep_filtrado_cities[which(inep_filtrado_cities$no_tipo_entidade %in% c(input$points)),],
        lat = ~ lat,
        lng = ~ lng,
        clusterOptions = markerClusterOptions(
          maxClusterRadius = 100,
          chunkedLoading = "true",
          disableClusteringAtZoom = 15,
          chunkInterval = "50ms",
          chunkDelay = "10ms"
        ),
        icon = awesomeIcons(
          icon = 'ios-close',
          iconColor = 'black',
          library = 'ion', 
          defineCircleColor(inep_filtrado_cities[which(inep_filtrado_cities$no_tipo_entidade %in% c(input$points)),])
        ),
        group = "Concorrentes",
        popup = popup
      )
    
    #
    mapa_hm <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(input$pick_type_map) %>%
      leaflet::fitBounds(
        fronteira_cidade()[1],
        fronteira_cidade()[2],
        fronteira_cidade()[3],
        fronteira_cidade()[4]
      ) %>% 
      leaflet.extras::addHeatmap(
        data = inep_filtrado_cities,
        lng = ~ lng,
        lat = ~ lat,
        intensity = ~ input$heat_var,
        blur = 20,
        max = 1,
        radius = 15
      )
    
    #
    mapa_poly <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(input$pick_type_map) %>%
      leaflet::fitBounds(
        fronteira_cidade()[1],
        fronteira_cidade()[2],
        fronteira_cidade()[3],
        fronteira_cidade()[4]
      ) %>%
      leaflet::addPolygons(
        data = shape_files_plot,
        fill = TRUE,
        stroke = TRUE,
        # https://rstudio.github.io/leaflet/colors.html
        fillOpacity = 0.60,
        color = ~pal(normalize(shape_files_plot[[as.character(input$ployvar_serv)]])),
        weight = 1,
        label = lapply(labs, htmltools::HTML)
      )
    
    #
    mapa_solo <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addProviderTiles(input$pick_type_map) %>%
      leaflet::fitBounds(
        fronteira_cidade()[1],
        fronteira_cidade()[2],
        fronteira_cidade()[3],
        fronteira_cidade()[4]
      )
    
    #
    input_polygon <- input$inputsf; input_hm <- input$inputhm; input_point <- input$inputpt
    
    if(input_polygon == TRUE && input_hm == TRUE && input_point == TRUE){
      return(mapa_all)
    }else if(input_polygon == TRUE && input_hm == TRUE && input_point == FALSE){
      return(mapa_no_points)
    }else if(input_polygon == TRUE && input_hm == FALSE && input_point == TRUE){
      return(mapa_shm)
    }else if(input_polygon == FALSE && input_hm == TRUE && input_point == TRUE){
      return(mapa_spoly)
    }else if(input_polygon == FALSE && input_hm == FALSE && input_point == TRUE){
      return(mapa_pontos)
    }else if(input_polygon == FALSE && input_hm == TRUE && input_point == FALSE){
      return(mapa_hm)
    }else if(input_polygon == TRUE && input_hm == FALSE && input_point == FALSE){
      return(mapa_poly)
    }else return(mapa_solo)
    
  })
  
  output$map <- leaflet::renderLeaflet({
    event()
  })
  
}
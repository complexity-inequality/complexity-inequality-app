rm(list = ls())
library(shiny)
library(dplyr)

# https://davidgohel.github.io/ggiraph/articles/offcran/shiny.html
# https://shiny.rstudio.com/gallery/brazil-voter-profile.html
# https://jeroen.github.io/mongolite/query-data.html#filter-fields

# Insert logs

source(file = "./tab_home.R")
source(file = "./tab_sobre.R")
source(file = "./tab_more.R")

ui <- shiny::shinyUI(
  shiny::fluidPage(
    shiny::navbarPage(
      title = "Complexity-Inequality v:0.72", 
      id = "page_id", 
      selected = "app",
      
      home,
      
      shiny::tabPanel(
        title = "Membros",
        value = "members",
        shiny::fluidPage(
          shiny::column(
            width = 10, offset = 1,
            shiny::uiOutput(outputId = "members") %>% shinycssloaders::withSpinner()
          ),
        )
      ),
      
      
      shiny::tabPanel(
        title = "App", 
        value = "app",
        shiny::fluidPage(
          shiny::column(
            width = 3,
            shiny::fluidRow(
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_1"),
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_2"),
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_3"),
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_4"),
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_5"),
              shiny::br(),
              shiny::actionButton(
                inputId = "goButton", width = "100%", strong("Processar"), #width = "320px",
                style="display: inline-block; align-items: center;justify-content: center;float:center;padding-bottom:13px;border-radius: 30px;"
              )
            )
          ),
          shiny::column(width = 1),
          shiny::column(
            width = 7,
            shiny::fluidRow(
              shiny::tabsetPanel(
                shiny::tabPanel(title = "Distribuição espacial", shiny::plotOutput(outputId = "plot1") %>% shinycssloaders::withSpinner()),
                shiny::tabPanel(title = "geom_sf_interactive", ggiraph::girafeOutput(outputId = "plot2") %>% shinycssloaders::withSpinner()),
                shiny::tabPanel(title = "Gráfico de densidade", shiny::plotOutput(outputId = "plot3") %>% shinycssloaders::withSpinner()),
                shiny::tabPanel(title = "Tabela", shiny::fluidPage(br(), DT::dataTableOutput(outputId = "table1") %>% shinycssloaders::withSpinner()))
              )
            ),
            br(),
            shinydashboard::box(
              title = "Informações adicionais", 
              background = "light-blue", 
              solidHeader = T, 
              shiny::textOutput("info1")
            )
          ),
          shiny::column(width = 1)
        )
      ),

      shiny::tabPanel(
        title = "Publicações", 
        value = "publications",
        shiny::fluidPage(
          shiny::column(
            width = 10, offset = 1,
            shiny::uiOutput(outputId = "publications_ui") %>% shinycssloaders::withSpinner()
          ),
        )
      ),
      
      shiny::tabPanel(
        title = "Metodologia", 
        value = "met",
        shiny::br(),
        shiny::h4("Here we are gonna explain the methodology, methods and tools used in the app and by the group")
      ),
      
      more,
      sobre
    )
  )
)




server <- function(input, output){
  
  options(stringsAsFactors = F)
  
  mongo_credentials <- config::get(file = "../conf/globalresources.yml")
  
  # Info dfs stored local or in mongoDB?????????????????? make one of unique options after db_mun and store in mongoDB
  # option_estados <- readr::read_csv("./data/options/option_estados.csv")
  option_loc <- readr::read_csv("../data/options/option_loc.csv")
  option_stats <- readr::read_csv("../data/options/option_stats.csv")
  
  # Global variables
  ##### Dowload series button with zero value to empty regions
  shp_df <- list(
    "uf" = sf::st_read("../data/shp/shp_uf/"),
    "meso" = sf::st_read("../data/shp/shp_meso/"),
    "rgint" = sf::st_read("../data/shp/shp_rgint/"),
    "micro" = sf::st_read("../data/shp/shp_micro/"),
    "rgime" = sf::st_read("../data/shp/shp_rgime/")
  )
  
  output$input_ui_1 <- shiny::renderUI({
    choices1 <- as.list(option_loc %>% dplyr::select(sg_rg) %>% unique() %>% dplyr::pull())
    names(choices1) <- option_loc %>% dplyr::select(nm_rg) %>% unique() %>% dplyr::pull()
    shinyWidgets::pickerInput(
      inputId = "input_server_1",
      label = "Selecione a região",
      choices = choices1,
      multiple = F,
      selected = choices1[1]
    )
  })
  
  output$input_ui_2 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_2",
      label = "Selecione a divisão territorial",
      choices = list(
        "Estadual" = "uf",
        "Mesorregional" = "meso", 
        "Microrregional" = "micro", 
        "Intermediária" = "rgint", 
        "Imediata" = "rgime"
      ),
      multiple = FALSE,
      selected = "rgime"
    )
  })
  
  output$input_ui_3 <- shiny::renderUI({
    choices3 <- as.list(option_stats %>% dplyr::select(cd_tema) %>% unique() %>% dplyr::pull())
    names(choices3) <- option_stats %>% dplyr::select(nm_tema) %>% unique() %>% dplyr::pull()
    shinyWidgets::pickerInput(
      inputId = "input_server_3",
      label = "Selecione o tema",
      choices = choices3,
      multiple = FALSE,
      selected = "eci"
    )
  })
  
  output$input_ui_4 <- shiny::renderUI({
    choices4 <- as.list(option_stats %>% dplyr::filter(cd_tema==input$input_server_3) %>% dplyr::select(cd_stat) %>% dplyr::pull())
    names(choices4) <- option_stats %>% dplyr::filter(cd_tema==input$input_server_3) %>% dplyr::select(nm_stat) %>% dplyr::pull()
    shinyWidgets::pickerInput(
      inputId = "input_server_4",
      label = "Selecione a estatística",
      choices = choices4,
      multiple = FALSE,
      selected = choices4[2]
    )
  })
  
  output$input_ui_5 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_5",
      label = "Selecione o período",
      choices = c(as.character(1997:2021), "1997-2021"),
      multiple = FALSE,
      selected = "1997-2021"
    )
  })
  
  reac_shp <- shiny::eventReactive(input$goButton, {
    shp_df <- shp_df[[input$input_server_2]]
    if(input$input_server_1!="BR"){
      shp_df <- shp_df %>%
        dplyr::filter(sg_rg==input$input_server_1)
    } else {shp_df}
    shp_df
  })

  reac_query <- shiny::eventReactive(input$goButton, {
    colec = paste0("colec_", input$input_server_2)
    mongo_set <- mongolite::mongo(db = "db1", collection = colec, url = mongo_credentials$mongoURL, verbose = TRUE)
    df <- mongo_set$find(paste0('{"product" : ', paste0('"', input$input_server_4, '"'), ', "cd_year" : ', paste0('"', input$input_server_5, '"'), '}'))
    if(input$input_server_1!="BR"){ # melhorar com vars() depois
      df <- df %>%
        dplyr::filter(sg_rg==input$input_server_1)
    } else {df}
    df
  })

  react_df <- shiny::eventReactive(input$goButton, {
    df_shp <- dplyr::full_join(
      reac_query(),
      reac_shp()
    ) %>% sf::st_sf()
  })

  output$plot1 <- shiny::renderPlot({
    ggplot2::ggplot(react_df())+
      # ggplot2::geom_sf(ggplot2::aes(0), color="black", size=.13)+
      ggplot2::geom_sf(ggplot2::aes(fill=value), color="black", size=.2)+
      ggplot2::scale_fill_gradient(low="white", high="blue")+
      ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
      ggplot2::theme_void()
  })

  output$plot2 <- ggiraph::renderGirafe({
    dfr1 <- react_df()
    rownames(dfr1) <- dfr1$cd_meso
    gg2 <- ggplot2::ggplot(dfr1,
      ggplot2::aes(
        fill=dfr1$value,
        tooltip=dfr1$cd_meso,
        data_id=dfr1$cd_meso
      )
    )+
      ggiraph::geom_sf_interactive(color="black", size=.2)+
      ggplot2::scale_fill_gradient(low="white", high="blue")+
      ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
      ggplot2::theme_void()
    ggiraph::girafe(ggobj = gg2)
  })

  output$plot3 <- shiny::renderPlot({
    dfr1 <- react_df()
    ggplot2::ggplot(dfr1)+
      ggplot2::geom_density(ggplot2::aes(value))+
      ggplot2::theme_void()
  })

  output$info1 <- shiny::renderText({
    vl <- reac_query()$value
    paste0(
      "Média: ", round(mean(vl), 3), "; ",
      "Mediana: ", round(median(vl), 3), "; ",
      "Desvio Padrão: ", round(sd(vl), 3), "; ",
      "Variância: ", round(var(vl), 3), "; ",
      "Máximo: ", round(max(vl), 3), "; ",
      "Mínimo: ", round(min(vl), 3)
    )
  })

  output$table1 <- DT::renderDataTable({
    reac_query()
  })
  
  output$members <- shiny::renderUI({
    bios <- readr::read_csv("../data/options/bios.csv")
    lapply(1:nrow(bios), function(i) {
      shiny::div(
        shiny::h3(bios[i, "nm"]), 
        shiny::h5(bios[i, "en_title"]),
        shiny::h6(bios[i, "en_desc"]),
        shiny::br()
      )
    })
  })
  
  output$publications_ui <- shiny::renderUI({
    publications <- readr::read_csv("../data/options/publications.csv")
    lapply(1:nrow(publications), function(i) {
      shiny::div(
        shiny::h3(publications[i, "title"]), 
        shiny::h5(publications[i, "year"]), 
        shiny::h5(publications[i, "authors"]),
        shiny::h6(publications[i, "citation"]),
        shiny::br()
      )
    })
  })
  
  
}

shiny::shinyApp(ui = ui, server = server)



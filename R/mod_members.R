


mod_members_ui <- function(id){
  ns <- NS(id)
  shiny::tabPanel(
    title = "Membros",
    value = "members",
    shiny::fluidPage(
      shiny::column(
        width = 10, offset = 1,
        shiny::h3("O grupo é constituído dos seguintes integrantes:"),
        shiny::br(),
        # shiny::uiOutput(outputId = "authors_test") %>% shinycssloaders::withSpinner(),
        br()
      ),
    )
  )
}

mod_mapa_server <- function(input, output, session){
  ns <- session$ns
  
}
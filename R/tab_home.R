tab_home <- shiny::tabPanel(
  title = "Home", 
  value = "home",
  shiny::column(width = 10, offset = 1,
                shiny::br(),
                shiny::h3("The Center for Research on Complexity, Development, and Inequality (CDI) at the Federal University of Santa Catarina makes use of new methods from network science, econometrics and economic complexity to understand the challenges of smart diversification, inequality and inclusive growth in Brazil and across the world."),
                shiny::br(),
                shiny::h3("Our team views economies as complex evolving systems and explores the boundaries of economics with other disciplines, such as data science, geography, and innovation studies to get a better understanding on dynamic socioeconomic development processes and challenges in the digital age."),
                shiny::br(),
                shiny::h3("We collaborate with leading researchers from various countries and regions of Brazil on topics such as innovation, structural change and labor market dynamics, inequality and poverty dynamics, and economic catch-up processes."),
                shiny::br(),
                shiny::h3("The complexity explorer application allows to map and download data on education, complexity, inequality and social efficiency values across Brazil and the world."),
                shiny::br(),
                shiny::h3("Welcome to CDI’s webpage, projects and applications.")
  )
)
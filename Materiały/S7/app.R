library(shiny)
library(SmarterPoland)
library(dplyr)

ui <- fluidPage(
  
  titlePanel("Simple Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(inputId = "chosen_continent",
                         label = "Select continent names:",
                         choices = unique(countries[["continent"]]),
                         selected = unique(countries[["continent"]]))
    ),
    
    mainPanel(
      h2("Scatterplot"),
      plotOutput("countries_plot", height = 600, brush = "country_brush"),
      textOutput("cointries_counter"),
      h2("Table"),
      tableOutput("countries_table")
    )
  )
)

server <- function(input, output) {
  
  continent_colors <- c(Asia = "red", Europe = "green", Africa = "orange", Americas = "black", 
                        Oceania = "blue")
  
  countries_r <- reactive({
    
    filter(countries, continent %in% input[["chosen_continent"]]) 
  })
  
  filtered_countries <- reactive({
    
    validate(
      need(input[["country_brush"]], "Select at least one country")
    )
    
    filter(countries_r(), birth.rate > input[["country_brush"]][["xmin"]],
           birth.rate < input[["country_brush"]][["xmax"]], 
           death.rate > input[["country_brush"]][["ymin"]],
           death.rate < input[["country_brush"]][["ymax"]]) 
  })
  
  output[["countries_plot"]] <- renderPlot({
    p <- ggplot(countries_r(), aes(x = birth.rate, y = death.rate, color = continent)) +
      geom_point() +
      scale_color_manual(values = continent_colors[input[["chosen_continent"]]]) +
      theme_bw()
    
    p
  })
  
  output[["countries_table"]] <- renderTable({
    filtered_countries()
  })
  
  output[["cointries_counter"]] <- renderText({
    paste("Selected", nrow(filtered_countries()), " countries")
  })
}

shinyApp(ui = ui, server = server)

library(tidyverse)
library(shiny)
library(shinythemes)
library(janitor)

ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  
  selectInput("Y",
              "select sleep variable", 
              choices = c("sleep_total",
                          "sleep_rem",
                          "sleep_cycle",
                          "awake"),
              selected = "sleep_total"),
  plotOutput("plot", width="600px", height = "500px")
)

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    
    msleep %>% 
      filter(vore != "NA") %>% 
      ggplot(aes(x=vore,
                 y= .data[[input$Y]],
                 fill = vore))+
      geom_boxplot(alpha=0.75)+
      labs(title="sleep Variables by Vore Type",
           x="Vore")+
      theme_minimal()
  })
}

shinyApp(ui, server)
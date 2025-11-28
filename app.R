library(dplyr)
library(shiny)
library(ggplot2)
library(DT)
library(usethis)

ui <- fluidPage(
    titlePanel("Star Wars"),
    h1("Star Wars Characters"),

    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "taille",
                        label = "Height of characters :",
                        min = 0,
                        max = 250,
                        value = 30),
            selectInput(inputId = "gender",
                        label = "Gender of characters",
                        choices = c("feminine", "masculine")
                        )
        ),
      
        
        
        mainPanel(
          textOutput(outputId = "nbcharacters"),
          plotOutput("StarWarsPlot"),
          DT::DTOutput(outputId = "tab_nb_charact")
        )
    )
)

server <- function(input, output) {
    
    output$nbcharacters <- renderText({
      nb_lignes=starwars |> 
        filter(height > input$taille & gender == input$gender)|>
        nrow()
      paste("Number of row(s):", nb_lignes)
    })  
    
    output$StarWarsPlot <- renderPlot({
      starwars |> 
        filter(height > input$taille & gender == input$gender) |>
        ggplot(aes(x = height)) +
        geom_histogram(
          binwidth = 10, 
          fill = "darkgray", 
          color = "white"
        ) +
      labs(title = paste("Gender choice : ", input$gender))
    })
    
    output$tab_nb_charact <- renderDT({
      starwars |> 
        filter(height > input$taille & gender == input$gender) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

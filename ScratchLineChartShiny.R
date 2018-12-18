library(shiny)
library(ggplot2)



# DEFINE UI FUNCTION #######################################


ui <- fluidPage(
  titlePanel("Charting Scratch Data"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting variable from sracth data.
      selectInput("var",
                  label = "Select X Axis Variable",
                  choices = c("viewers_website" = 4,
                              "lovers_website"  = 5,
                              "downloaders_website" = 6,
                              "sprites_website"  = 7,
                              "scripts_website"  = 8),
                  selected = 6),  # Default selection
      selectInput("var2",
                  label = "Select Y Axis Variable",
                  choices = c("viewers_website" = 4,
                              "lovers_website"  = 5,
                              "downloaders_website" = 6,
                              "sprites_website"  = 7,
                              "scripts_website"  = 8),
                  selected = 4)  # Default selection
    ),
    mainPanel(
      plotOutput("ScratchLinePlot")  
    )
  )
)
# DEFINE SERVER FUNCTION ###################################

server <- function(input, output) {
  # Define histogram output
  
  output$ScratchLinePlot <- renderPlot({  
    iX   <- as.numeric(input$var)  
    iY   <- as.numeric(input$var2) #
    x    <- Top50Projects[, iX]
    y    <- Top50Projects[, iY]
 #  p <- plot_ly(Top50Projects,x=~xV,y=~yV,type ="scatter",mode='lines+markers')  %>%
#   layout(title = 'Line Chart',
#          xaxis = list(title = paste("Total of", names(Top50Projects[iX]))),
#          yaxis = list (title = paste("Total of", names(Top50Projects[iY]))))
#    p
    # Top50Projects[, Top50Projects$downloaders_website]
  #   Top50Projects[, Top50Projects$lovers_website]
    require(graphics)
    ggplot(Top50Projects, 
           aes(x,y,
               color = Top50Projects$lovers_website)) +
    #  geom_point(size = 1) +
      xlab(paste("Total of", names(Top50Projects[iX]))) +
      ylab(paste("Total of", names(Top50Projects[iY]))) +
      ggtitle("Line Chart") +
      geom_line() 
   ##   geom_smooth(method = lm)  
  })
}

# CALL THE SHINY APP #######################################

shinyApp(ui = ui, server = server)
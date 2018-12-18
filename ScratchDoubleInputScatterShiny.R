library(shiny)



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
      plotOutput("ScratchPlot")  
    )
  )
)
# DEFINE SERVER FUNCTION ###################################

server <- function(input, output) {
  # Define histogram output
  
  output$ScratchPlot <- renderPlot({  
    iX   <- as.numeric(input$var)  
    iY   <- as.numeric(input$var2) #
    x    <- Top50Projects[, iX]
    y   <- Top50Projects[, iY]
    require(graphics)
    ggplot(Top50Projects, 
           aes(x,y ,
               color = Top50Projects$lovers_website)) +
      geom_point(size = 3) +
      xlab(paste("Total of", names(Top50Projects[iX]))) +
      ylab(paste("Total of", names(Top50Projects[iY]))) +
      ggtitle("Scatter Plot") +
      geom_smooth(method = lm) 
    
  })
}

# CALL THE SHINY APP #######################################

shinyApp(ui = ui, server = server)
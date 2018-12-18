# LOAD SHINY ###############################################

library(shiny)



# DEFINE UI FUNCTION #######################################

ui <- fluidPage(
  titlePanel("Charting Scratch Data"),
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting variable from sracth data.
      selectInput("var",
                  label = "Select variable",
                  choices = c("viewers_website" = 4,
                              "lovers_website"  = 5,
                              "downloaders_website" = 6,
                              "sprites_website"  = 7,
                              "scripts_website"  = 8),
                  selected = 4),  # Default selection
      # Slider to set bin width in histogram
      sliderInput("bins",
                  "Number of bins",
                  min   = 1,
                  max   = 4000,
                  value = 50)
    ),
    mainPanel(
      plotOutput("ScratchHist")  # "ScratchHist" in server also
    )
  )
)
# DEFINE SERVER FUNCTION ###################################

server <- function(input, output) {
  # Define histogram output
  output$ScratchHist <- renderPlot({  # "ScratchHist" in UI also
    ic   <- as.numeric(input$var)
    X   <- Top50Projects[, ic]
    bins <- seq(min(X), 
                max(X), 
                length.out = input$bins + 50)
    hist(X, 
         breaks = bins, 
         col    = '#8DC13D',  # Green 
         border = 'white',
         xlab   = paste("Total of", names(Top50Projects[ic])),
         main   = paste("Histogram of", names(Top50Projects[ic]))
    )
  })
}
# CALL THE SHINY APP #######################################

shinyApp(ui = ui, server = server)


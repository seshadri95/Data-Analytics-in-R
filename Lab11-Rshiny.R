library(shiny)
library(plotly)
library(ggplot2)
ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "subset_min",
                  label = "index to start subset:",
                  min = 1,
                  max = 50,
                  value = 30),
      sliderInput(inputId = "subset_max",
                  label = "index to end subset:",
                  min = 51,
                  max = 150,
                  value = 50)	
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      verbatimTextOutput(outputId = "summary_op"),
      plotlyOutput(outputId = "lm_model"),
      
    )
  )
)

server <- function(input, output) {
  
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram 
  # is wrapped in a call to renderPlot to indicate 
  # that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    faithful = faithful[input$subset_min:input$subset_max,]
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
  output$summary_op <- renderPrint({
    summary(faithful[input$subset_min:input$subset_max,])
  })
  
  output$lm_model = renderPlotly({
    
    ggplotly(ggplot(faithful[input$subset_min:input$subset_max,],aes(x=waiting,y=eruptions)) + geom_point() + geom_smooth(method='lm'))
    
  })
}

shinyApp(ui = ui, server = server)
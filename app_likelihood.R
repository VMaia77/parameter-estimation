library(shiny)

x <- seq(0, 200)
y <- (7 * x) + rnorm(201, 0, 300)

ui <- fluidPage(
  titlePanel("Likelihood"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "b1",
        label = "Slope",
        min = 0,
        max = 10,
        value = 7
      ), 
      sliderInput(
        inputId = "b0",
        label = "Intercept",
        min = -500,
        max = 1700,
        value = 23
      )
    ),
    
    mainPanel(
      plotOutput(outputId = "distPlot")
    )
  )
) 

server <- function(input, output) {
  output$distPlot <- renderPlot({
    b0 <- input$b0
    b1 <- input$b1
    
    plot(
      y ~ x,
      main = "Likelihood",
      xlab = "x",
      pch = 21,
      col = "deepskyblue2",
      cex = 1.3,
      ylab = "Y",
      cex.lab = 1.3,
      cex.axis = 1.2
    )
    
    f1 <- function(b0, b1) {  
      curve(b0 + b1 * x, add = TRUE, col = "red")
    } 
    f1(b0, b1)
  })
}

shinyApp(ui = ui, server = server)

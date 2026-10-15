library(shiny)


ui <- fluidPage(
  titlePanel("Creating a Scatter Plot on An Interactive Plot"),
  sidebarLayout(
    sidebarPanel(
      h1("Click on the plot to add data points"),
      verbatimTextOutput("dataPoints")
    ),
    mainPanel(
      plotOutput("scatter", click = "pclick")
    )
  )
)

server <- function(input, output, session) {
  points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
  observeEvent(input$pclick, {
    new_point <- data.frame(x = input$pclick$x, y = input$pclick$y)
    points(rbind(points(), new_point))
  })
  output$scatter <- renderPlot({
    data <- points()
    plot(data$x, data$y, 
         xlim = c(-4, 4), ylim = c(-4, 4), 
         main = "Click to Add Points",
         xlab = "X", ylab = "Y",
         pch = 20, col = "purple")
  })

  output$dataPoints <- renderPrint({
    points()
  })
}

shinyApp(ui, server)
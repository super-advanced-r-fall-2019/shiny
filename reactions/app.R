library(shiny)

ui <- fluidPage(
  numericInput("n", "n", 10),
  p("Fibonacci value:", textOutput("nthValue", inline = TRUE)),
  p("Inverse Fibonacci value:", textOutput("nthValueInv", inline = TRUE)),
  p("It took", textOutput("fibTime", inline = TRUE), "seconds to compute"),
  helpText("Hit this button to show how long it takes to compute fib(n) for various n"),
  actionButton("go", "Go"),
  plotOutput("reaction_plot")
)

server <- function(input, output) {
  # use reactive conductor to save computation time
  currentFib         <- reactive({ fib(as.numeric(input$n)) })
  # print values
  output$nthValue    <- renderText({ currentFib()$fibVal })
  output$nthValueInv <- renderText({ 1 / currentFib()$fibVal })
  # print computation times
  output$fibTime <- renderText({currentFib()$runTime})
  # only compute run times once user hits go button
  fibRunTimes <- eventReactive(input$go, {
    computeTimes(input$n)
  })
  # plot result of fibRunTimes()
  output$reaction_plot <- renderPlot({
    plot(fibRunTimes(), xlab = "n", ylab = "Run Time (seconds)", main = "Run times of fib() function")
  })
}

# define helper function to compute fibonacci sequence
# also returns computation time
fib <- function(n) {
  startTime <- Sys.time()
  if (n < 3) {
    fibVal <- 1
  } else {
    fibVal <- fib(n-1)$fibVal + fib(n-2)$fibVal
  }
  endTime <- Sys.time()
  runTime <- endTime - startTime
  return(list(fibVal = fibVal, runTime = runTime))
}

# another helper function
# given an n, returns computation time of fib() for 1:n
computeTimes <- function(n) {
  times <- numeric(n)
  for (i in 1:length(times)) {
    times[i] <- fib(i)$runTime
  }
  return(times)
}

# run the app
shinyApp(ui, server)


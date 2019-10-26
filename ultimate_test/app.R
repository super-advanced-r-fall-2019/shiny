library(shiny)

source("helper.R")

# Potential exercises
# make this look nicer!
# more functionality

# define ui
ui <- fluidPage(
  fluidRow(
    # Washington inputs
    column(4,
           h3("Washington"),
           numericInput("N0_WA", label = "Initial Population Size", 
                        value = 1500, min = 1000, max = 2000, step = 1),
           helpText("Takes values between 1000 and 2000 in increments of 1"),
           numericInput("K_WA", label = "Carrying Capacity", 
                        value = 3500, min = 1000, max = 4000, step = 1),
           helpText("Takes values between 1000 and 4000 in increments of 1"),
           sliderInput("r_WA", label = "Growth Rate", 
                       min = 0, max = 1, value = 0.05, step = 0.01),
           checkboxInput("harvested_WA", label = "Harvested?", value = FALSE),
           # if harvested, what is catch size?
           uiOutput("show_catch_WA"),
           # show projection here
           actionButton("go_WA", "Go"), 
           plotOutput("project_WA", click = "WA_plot_click"),
           verbatimTextOutput("WA_info")
           
    ),
    # Oregon inputs
    column(4, 
           h3("Oregon"),
           numericInput("N0_OR", label = "Initial Population Size", 
                        value = 1500, min = 1000, max = 2000, step = 1),
           helpText("Takes values between 1000 and 2000 in increments of 1"),
           numericInput("K_OR", label = "Carrying Capacity", 
                        value = 1500, min = 1000, max = 4000, step = 1),
           helpText("Takes values between 1000 and 4000 in increments of 1"),
           sliderInput("r_OR", label = "Growth Rate", 
                       min = 0, max = 1, value = 0.05, step = 0.01),
           checkboxInput("harvested_OR", label = "Harvested?", value = FALSE),
           # if harvested, what is catch size?
           uiOutput("show_catch_OR"),
           # show projection here
           actionButton("go_OR", "Go"), 
           plotOutput("project_OR", click = "OR_plot_click"),
           verbatimTextOutput("OR_info")
    ),
    # California inputs
    column(4, 
           h3("California"),
           numericInput("N0_CA", label = "Initial Population Size", 
                        value = 1500, min = 1000, max = 2000, step = 1),
           helpText("Takes values between 1000 and 2000 in increments of 1"),
           numericInput("K_CA", label = "Carrying Capacity", 
                        value = 1500, min = 1000, max = 4000, step = 1),
           helpText("Takes values between 1000 and 4000 in increments of 1"),
           sliderInput("r_CA", label = "Growth Rate", 
                       min = 0, max = 1, value = 0.05, step = 0.01),
           checkboxInput("harvested_CA", label = "Harvested?", value = FALSE),
           # if harvested, what is catch size?
           uiOutput("show_catch_CA"),
           # show projection here
           actionButton("go_CA", "Go"), 
           plotOutput("project_CA", click = "CA_plot_click"),
           verbatimTextOutput("CA_info")
    )
  ), 
  hr(),
  sliderInput("projection_length", label = "Number of years ahead to forecast", 
              min = 1, max = 50, value = 25, step = 1, width = "100%")
)


# define server
server <- function(input, output) {
  
  projected_WA <- eventReactive(input$go_WA, {
    logistic_growth(input$N0_WA, input$r_WA, input$K_WA, input$projection_length, input$harvested_WA, input$catch_size_WA)
  })
  
  output$project_WA <- renderPlot({
    maxN <- max(projected_WA())
    plot(projected_WA(), col = "blue", lwd = 2, 
         xlim = c(0, input$projection_length), ylim = c(0, 4000), type = "p", 
         xlab = "", ylab = "Population Size", main = "")
  })
  
  projected_OR <- eventReactive(input$go_OR, {
    logistic_growth(input$N0_OR, input$r_OR, input$K_OR, input$projection_length, input$harvested_OR, input$catch_size_OR)
  })
  
  output$project_OR <- renderPlot({
    maxN <- max(projected_OR())
    plot(projected_OR(), col = "blue", lwd = 2, 
         xlim = c(0, input$projection_length), ylim = c(0, 4000), type = "p", 
         xlab = "", ylab = "Population Size", main = "")
  })
  
  projected_CA <- eventReactive(input$go_CA, {
    logistic_growth(input$N0_CA, input$r_CA, input$K_CA, input$projection_length, input$harvested_CA, input$catch_size_CA)
  })
  
  output$project_CA <- renderPlot({
    maxN <- max(projected_CA())
    plot(projected_CA(), col = "blue", lwd = 2, 
         xlim = c(0, input$projection_length), ylim = c(0, 4000), type = "p", 
         xlab = "", ylab = "Population Size", main = "")
  })
  
  
  output$show_catch_WA <- renderUI({
    if (input$harvested_WA) {
      numericInput("catch_size_WA", label = "Catch Size", value = 300, 
                   min = 0, max = 2000)
    }
  })
  output$show_catch_OR <- renderUI({
    if (input$harvested_OR) {
      numericInput("catch_size_OR", label = "Catch Size", value = 300, 
                   min = 0, max = 2000)
    }
  })
  output$show_catch_CA <- renderUI({
    if (input$harvested_CA) {
      numericInput("catch_size_CA", label = "Catch Size", value = 300, 
                   min = 0, max = 2000)
    }
  })
  
  # the following works, but is kind of inefficient. How could you improve it?
  # Hint: we are rerunning the logistic growth function again! How could you use a reactive conductor?
  output$WA_info <- renderPrint({
    projected_WA2 <- as.data.frame(cbind(year = 1:(input$projection_length+1), 
                                        N = logistic_growth(input$N0_WA, input$r_WA, input$K_WA, 
                                                            input$projection_length, input$harvested_WA, input$catch_size_WA)))
    nearPoints(projected_WA2, input$WA_plot_click, xvar = "year", yvar = "N")
  })
  
  output$OR_info <- renderPrint({
    projected_OR2 <- as.data.frame(cbind(year = 1:(input$projection_length+1), 
                                         N = logistic_growth(input$N0_OR, input$r_OR, input$K_OR, 
                                                             input$projection_length, input$harvested_OR, input$catch_size_OR)))
    nearPoints(projected_OR2, input$OR_plot_click, xvar = "year", yvar = "N")
  })
  
  output$CA_info <- renderPrint({
    projected_CA2 <- as.data.frame(cbind(year = 1:(input$projection_length+1), 
                                         N = logistic_growth(input$N0_CA, input$r_CA, input$K_CA, 
                                                             input$projection_length, input$harvested_CA, input$catch_size_CA)))
    nearPoints(projected_CA2, input$CA_plot_click, xvar = "year", yvar = "N")
  })
  
}

# run app
shinyApp(ui = ui, server = server)


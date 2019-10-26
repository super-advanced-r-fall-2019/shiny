library(shiny)

# define UI
ui <- fluidPage(
   # Application title
   titlePanel("Some widgets"),
   # Copy the line below to make a checkbox
   checkboxInput("checkbox", label = "I am a checkbox", value = TRUE),
   p("My value is:", textOutput("checkbox_value", inline = TRUE)),
   # Copy the chunk below to make a group of checkboxes
   checkboxGroupInput("checkGroup", label = "I am a bunch of checkboxes", 
                      choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                      selected = 1),
   p("My values are:", textOutput("checkbox_group_value", inline = TRUE)),
   # Copy the line below to make a slider bar 
   sliderInput("slider1", label = "I slide", min = 0, max = 100, value = 50),
   p("My range is 0 to:", textOutput("slider_value", inline = TRUE)),
   # Copy the line below to make a slider range 
   sliderInput("slider2", label = "I slide, but fancier", min = 0, 
               max = 100, value = c(40, 60)),
   p("My range is:", textOutput("slider_range", inline = TRUE)),
   # Copy the line below to make a select box 
   selectInput("select", label = "I am a select box", 
               choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
               selected = 1),
   p("My value is:", textOutput("select_value", inline = TRUE)), 
   # Copy the line below to make a set of radio buttons
   radioButtons("radio", label = "I am radio buttons",
                choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                selected = 1),
   p("My value is:", textOutput("radio_value", inline = TRUE)),
   # Copy the line below to make a numeric input
   numericInput("num", label = "I am a numeric input", value = 1),
   p("My value is:", textOutput("numeric_value", inline = TRUE)),
   # Copy the line below to make a date selector 
   dateInput("date", label = "I am a date", value = "2014-01-01"),
   p("My value is:", textOutput("date_value", inline = TRUE)), 
   # Copy the line below to make a date range selector
   dateRangeInput("dates", label = "I am a date range"),
   p("My value is:", textOutput("date_range", inline = TRUE)),
   # Copy the line below to make an action button
   actionButton("action", label = "Action"),
   p("My value is:", textOutput("action_value", inline = TRUE))
)

# define server
server <- function(input, output) {
  output$checkbox_value <- renderPrint({ input$checkbox })
  output$checkbox_group_value <- renderPrint({ input$checkGroup })
  output$slider_value <- renderPrint({ input$slider1 })
  output$slider_range <- renderPrint({ input$slider2 })
  output$select_value <- renderPrint({ input$select })
  output$radio_value <- renderPrint({ input$radio })
  output$numeric_value <- renderPrint({ input$num })
  output$date_value <- renderPrint({ input$date })
  output$date_range <- renderPrint({ input$dates })
  output$action_value <- renderPrint({ input$action })
}

# run the application 
shinyApp(ui = ui, server = server)


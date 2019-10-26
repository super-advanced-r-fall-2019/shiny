library(shiny)
library(markdown)

shinyApp(
  ui = tagList(
    shinythemes::themeSelector(), # lets you explore appearance of many themes
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "This is a complicated layout",
      tabPanel("very",
               mainPanel(
                 tabsetPanel(
                   tabPanel("look!",
                            br(), # just here to create vertical space
                            sidebarPanel(
                              titlePanel("I am a sidebar"),
                              helpText("I only appear in this tab"),
                              textInput("txt", "Text input:", "hello world"), 
                              h4("Table"),
                              tableOutput("table"),
                              h4("Verbatim text output"),
                              verbatimTextOutput("txtout")
                            ),
                            mainPanel(
                              # demonstrate some common html tags
                              h1("Header 1"),
                              h2("Header 2"),
                              h3("Header 3"),
                              h4("Header 4"),
                              h5("Header 5"), 
                              p("There is about to be some vertical space"),
                              br(),
                              p("Now there is going to be vertial space with a line"), 
                              hr(),
                              p(strong("bold")), 
                              p(em("italic")), 
                              p(code("code")),
                              a(href = "", "link (to nowhere)"), 
                              p("You can add a lot more specialized HTML elements. Use", 
                                code("tags$element_name(...)"), ". Google is your friend here")
                            )
                   ),
                   tabPanel("tabs!", 
                            # demonstrate how to include text files 
                            # demonstrate how to delineate column widths in fluid rows
                            titlePanel("includeText, includeHTML, and includeMarkdown"),
                            fluidRow(
                              column(4,
                                     includeText("include.txt"),
                                     br(),
                                     h2("Preformatted text here below:"),
                                     pre(includeText("include.txt"))
                              ),
                              column(4,
                                     includeHTML("include.html")
                              ),
                              column(4,
                                     includeMarkdown("include.md")
                              )
                            )
                   ),
                   tabPanel("more tabs!", 
                            fluidPage(
                              title = "Geyser Wait Times to Next Eruption",
                              plotOutput("distPlot"),
                              hr(),
                              p("Here is an example of a fluid row"),
                              fluidRow(
                                column(4, 
                                       sliderInput(inputId = "bins",
                                                   label = "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 30)
                                ),
                                column(4, 
                                       radioButtons("color", "Plot color:",
                                                    choices = list("Red" = "red", 
                                                                   "Orange " = "orange", 
                                                                   "Yellow " = "yellow", 
                                                                   "Green " = "green", 
                                                                   "Blue " = "blue", 
                                                                   "Indigo " = "darkblue", 
                                                                   "Violet " = "violet"
                                                    ), 
                                                    selected = "blue"
                                       )
                                ),
                                column(4, 
                                       textInput("xlabel", "x label:", "Waiting time to next eruption (in mins)"),
                                       textInput("main", "Plot title:", "Histogram of waiting times")
                                )
                              ) # ending row
                            ) # ending page
                   ) # ending tab panel
                 ) # ending tabset panel
               ) # ending main panel
      ), # ending tabset panel
      tabPanel("many", "This panel is intentionally left blank"),
      tabPanel("pages", "This panel is intentionally left blank")
    ) # ending navbar page
  ), # ending ui
  
  server = function(input, output) {
    # render text input
    output$txtout <- renderText({
      paste("The text input says: ", input$txt, sep = "")
    })
    # render table
    output$table <- renderTable({
      head(cars, 4)
    })
    # plot histogram of waiting times to next eruption 
    output$distPlot <- renderPlot({
      x    <- faithful$waiting
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = input$color, border = "white",
           xlab = input$xlabel,
           main = input$main)
    })
  }
)


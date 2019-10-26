# load libraries
library(shiny)
library(shinythemes)
library(dplyr)
library(leaflet)
library(lubridate)

# load helper functions
source("helper.R")

####### ui ########
  ui <- tagList(
    navbarPage(
      theme = shinytheme("yeti"),
      title = "RShiny Tutorial",
      #### MOTIVATION ####
      tabPanel("Motivation", 
               tabsetPanel(
                 tabPanel("Motivation",
                             includeHTML("pages/Motivation.html")
                 ),
                 #### example ####
                 tabPanel("Example",
                          br(),
                          helpText("This app makes population projections for a three different populations of a species, 
                                   using a logistic growth curve with the entered parameters and constant catch rate that 
                                   can be turned on or off."),
                          fluidRow(
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
                                   plotOutput("project_WA")
                                   
                            ),
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
                                   plotOutput("project_OR")
                            ),
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
                                   plotOutput("project_CA")
                            )
                          ), 
                          hr(),
                          sliderInput("projection_length", label = "Number of years ahead to forecast", 
                                      min = 1, max = 50, value = 25, step = 1, width = "100%")
                          )
               )
      ),
      #### FUNDAMENTALS ####
      tabPanel("Fundamentals", 
               tabsetPanel(
                 tabPanel("Fundamentals",
                   includeHTML("pages/Fundamentals.html")
                 ),
                 #### example ####
                 tabPanel("Example",
                   headerPanel('Plot selected time-series'),
                   sidebarPanel(
                     selectInput('dat', 
                                 'Dataset:',
                                 c("AirPassengers", "uspop", "mdeaths", "lynx", "USAccDeaths")
                     ),
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
                   mainPanel(
                     plotOutput('fund_ts_plot'),
                     textOutput("fund_ts_txt")
                   )
                 )
               )

      ),
      #### LAYOUTS ####
      tabPanel("Layouts", 
               tabsetPanel(
                 tabPanel("Layouts",
                          includeHTML("pages/Layouts.html")
                 ),
                 #### example ####
                 tabPanel("Example",
                          br(),
                          navbarPage(
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
                                                    p("You can add a lot more specialized HTML elements. Use", code("tags$element_name(...)"), ". Google is your friend here")
                                                  )
                                         ),
                                         tabPanel("tabs!", 
                                                  titlePanel("includeText, includeHTML, and includeMarkdown"),
                                                  
                                                  fluidRow(
                                                    column(4,
                                                           includeText("pages/Layouts/include.txt"),
                                                           br(),
                                                           h2("Preformatted text here below:"),
                                                           pre(includeText("pages/Layouts/include.txt"))
                                                    ),
                                                    column(4,
                                                           includeHTML("pages/Layouts/include.html")
                                                    ),
                                                    column(4,
                                                           includeMarkdown("pages/Layouts/include.md")
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
                                                             sliderInput(inputId = "layout_bins",
                                                                         label = "Number of bins:",
                                                                         min = 1,
                                                                         max = 50,
                                                                         value = 30)
                                                      ),
                                                      column(4, 
                                                             radioButtons("layout_color", "Plot color:",
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
                                                             textInput("layout_xlabel", "x label:", "Waiting time to next eruption (in mins)"),
                                                             textInput("layout_main", "Plot title:", "Histogram of waiting times")
                                                      )
                                                    )
                                                  )
                                         )
                                       )
                                     )
                            ),
                            tabPanel("many", "This panel is intentionally left blank"),
                            tabPanel("pages", "This panel is intentionally left blank")
                          )
                 ),
                 #### exercises ####
                 tabPanel("Exercises",
                          br(),
                          p("1. Create the layout for the logistic growth example on the Motivation page. Everywhere there is an element you can just put some placeholder text for now (how?)."),
                          p("2. Play around with themes. Pick one you like and apply it to the logistic growth app."))
               )
      ),
      #### WIDGETS ####
      tabPanel("Widgets", 
                 tabsetPanel(
                   tabPanel("Widgets",
                              includeHTML("pages/Widgets.html")
                            ),
                            #### example ####
                            tabPanel("Example",
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
                            ),
                   #### exercises ####
                   tabPanel("Exercises", 
                            br(),
                            p("1. Add numeric inputs for initial popuation size, carrying capacity, and harvest for each region."), 
                            p("2. Add slider inputs for growth rate for each region, and number of years to forcast all regions."), 
                            p("3. Add checkbox input to indicate whether or not each region is harvested."), 
                            p("4. Plot result of logistic function with regional inputs."))
                 )
      ),
      #### REACTIONS ####
      tabPanel("Reactions", 
               tabsetPanel(
                 tabPanel("Reactions", 
                   includeHTML("pages/Reactions.html")
                 ),
                 #### example ####
                 tabPanel("Example", 
                          mainPanel(
                            br(),
                            numericInput("n", "n", 10),
                            p("Fibonacci value:", textOutput("nthValue", inline = TRUE)),
                            p("Inverse Fibonacci value:", textOutput("nthValueInv", inline = TRUE)),
                            p("It took", textOutput("fibTime", inline = TRUE), "seconds to compute"),
                            helpText("Hit this button to show how long it takes to compute fib(n) for various n"),
                            actionButton("go", "Go"),
                            plotOutput("reaction_plot")
                          )
                 ),
                 #### exercises ####
                 tabPanel("Exercises",
                    br(),
                    p("1. Add an action button for each region that delays updating the plot until you hit \"Go\".")
                 )
               )
      ),
      #### PLOTTING ####
      tabPanel("Plotting", 
               tabsetPanel(
                 tabPanel("Plotting", 
                    includeHTML("pages/Plotting.html")
                 ),
                 #### example ####
                 tabPanel("Example", 
                          mainPanel(
                            helpText("Try clicking on a point!"),
                            plotOutput("plotting_plot1", click = "plot_click"),
                            verbatimTextOutput("plotting_info")
                          )
                 ), 
                 #### exercises ####
                 tabPanel("Exercises", 
                    br(),
                    p("1. Change plot type to points instead of lines."), 
                    p("2. Add ability to click a point and print out the population size in that year."))
               )
               ),
      #### MAPPING ####
      tabPanel( "Mapping with leaflet",
                tabsetPanel(
                  tabPanel("Mapping",
                    includeHTML("pages/Mapping.html")
                  ),
                  #### example ####
                  tabPanel("Example", #"blank"
                           mainPanel(
                             br(),
                             p("there is supposed to be a map here but for some reason it breaks the app :("),
                             p("you can run the plotting example from GitHub to see how it is supposed to work")
                             #,
                             #leafletOutput("mapping_map") # AGHHHHHHHHH WHY DOES THIS BREAK IT
                           ),
                           sidebarPanel(
                            radioButtons("decade", label = h3("Decade planted:"),
                                         choices = list("1950s" = 1950,
                                                        "1960s" = 1960,
                                                        "1970s" = 1970,
                                                        "1980s" = 1980,
                                                        "1990s" = 1990,
                                                        "2000s" = 2000,
                                                        "2010s" = 2010),
                                         selected = 1950)
                           )
                  )
                )

      ),
      #### PUBLISHING ####
      tabPanel("Publishing", 
               includeHTML("pages/Publishing.html")
               ),
      #### RESOURCES ####
      tabPanel("Resources", 
               includeHTML("pages/Resources.html")
               )
    )
  )
  ######## server #######
  server <- function(input, output) {
    
    #### MOTIVATION OUTPUTS ####
    output$project_WA <- renderPlot({
      projected_WA <- logistic_growth(input$N0_WA, input$r_WA, input$K_WA, input$projection_length, input$harvested_WA, input$catch_size_WA)
      maxN <- max(projected_WA)
      plot(projected_WA, col = "blue", lwd = 2, 
           xlim = c(0, input$projection_length), ylim = c(0, 4000), type = "l", 
           xlab = "", ylab = "Population Size", main = "")
    })
    
    output$project_OR <- renderPlot({
      projected_OR <- logistic_growth(input$N0_OR, input$r_OR, input$K_OR, input$projection_length, input$harvested_OR, input$catch_size_OR)
      maxN <- max(projected_OR)
      plot(projected_OR, col = "blue", lwd = 2, 
           xlim = c(0, input$projection_length), ylim = c(0, 4000), type = "l", 
           xlab = "Years Ahead", ylab = "", main = "")
    })
    
    output$project_CA <- renderPlot({
      projected_CA <- logistic_growth(input$N0_CA, input$r_CA, input$K_CA, input$projection_length, input$harvested_CA, input$catch_size_CA)
      maxN <- max(projected_CA)
      plot(projected_CA, col = "blue", lwd = 2, 
           xlim = c(0, input$projection_length), ylim = c(0, 4000), type = "l", 
           xlab = "", ylab = "", main = "")
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
    
    #### FUNDAMENTALS OUTPUTS ####
    output$fund_ts_plot <- renderPlot({
      selected_data <- selectData(input$dat)
      plot(selected_data$series,
           col = input$color, ylab = input$dat, lwd = 2)
    })
    
    output$fund_ts_plot2 <- renderPlot({
      selected_data <- selectData(input$dat)
      plot(selected_data$series,
           col = input$color, ylab = input$dat, lwd = 2)
    })
    
    output$fund_ts_text <- renderText({
      selected_data <- selectData(input$dat)
      selected_data$description})
    
    #### LAYOUTS OUTPUTS ####
    output$txtout <- renderText({
      paste(input$txt, input$slider, format(input$date), sep = ", ")
    })
    output$txtout2 <- renderText({
      paste(input$txt2, sep = "")
    })
    output$table <- renderTable({
      head(cars, 4)
    })
    output$distPlot <- renderPlot({
      
      x    <- faithful$waiting
      bins <- seq(min(x), max(x), length.out = input$layout_bins + 1)
      
      hist(x, breaks = bins, col = input$layout_color, border = "white",
           xlab = input$layout_xlabel,
           main = input$layout_main)
    })
    
    #### WIDGETS OUTPUTS ####
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
    
    #### REACTIONS OUTPUTS ####
    currentFib         <- reactive({ fib(as.numeric(input$n)) })
    
    output$nthValue    <- renderText({ currentFib()$fibVal })
    output$nthValueInv <- renderText({ 1 / currentFib()$fibVal })
    
    output$fibTime <- renderText({currentFib()$runTime})
    
    fibRunTimes <- eventReactive(input$go, {
      computeTimes(input$n)
    })
    
    output$reaction_plot <- renderPlot({
      plot(fibRunTimes(), xlab = "n", ylab = "Run Time (seconds)", main = "Run times of fib() function")
    })
    
    #### PLOTTING OUTPUTS ####
    output$plotting_plot1 <- renderPlot({
      plot(mtcars$wt, mtcars$mpg)
    })
    
    output$plotting_info <- renderPrint({
      # With base graphics, need to tell it what the x and y variables are.
      nearPoints(mtcars, input$plot_click, xvar = "wt", yvar = "mpg")
      # nearPoints() also works with hover and dblclick events
    })
    
    #### MAPPING OUTPUTS ####
    # THIS DOES NOT WORK FOR SOME REASON
    output$mapping_map <- renderLeaflet({})
    outputOptions(output, "mapping_map", suspendWhenHidden = FALSE)
    output$mapping_map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addMarkers(lng = -122.2821, lat = 47.63521) %>%
        addProviderTiles(providers$OpenStreetMap)
    })
    
    #### PUBLISHING OUTPUTS ####
    
    #### RESOURCES OUTPUTS ####

  }

# run app
shinyApp(ui = ui, server = server)


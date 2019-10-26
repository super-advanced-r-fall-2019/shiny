library(shiny)

# create page with sidebar
ui <- pageWithSidebar( 
  # place title
  headerPanel('Plot selected time-series'),
  # sidebar
  sidebarPanel(
    # choose data set
    selectInput('dat', 
                'Dataset:',
                c("AirPassengers", "uspop", "mdeaths", "lynx", "USAccDeaths")
                ),
    # choose color of plot, default to blue
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
  # main panel
  mainPanel(
    # render plot
    plotOutput('plot1'),
    # render description of plot
    textOutput("txtout")
  )
)

# Define server function
server <- function(input, output) {
  # render plot using selected data and selected color
  output$plot1 <- renderPlot({
    selected_data <- selectData(input$dat)
    plot(selected_data$series,
         col = input$color, ylab = input$dat, lwd = 2)
  })
  # render text using description of selected data
  output$txtout <- renderText({
    selected_data <- selectData(input$dat)
    selected_data$description})
  
}

# helper function
# exercise: what happens if you try to put this in the server portion??
selectData <- function(sent_var) {
  if (sent_var == "AirPassengers") {
    series <- datasets::AirPassengers
    description <- "The classic Box & Jenkins airline data. Monthly totals of international airline passengers, 1949 to 1960."
    return(list(series = series, description = description))
  } else if (sent_var == "uspop") {
    series <- datasets::uspop
    description <- "This data set gives the population of the United States (in millions) as recorded by the decennial census for the period 1790–1970."
    return(list(series = series, description = description))
  } else if (sent_var == "mdeaths") {
    series <- datasets::mdeaths
    description <- "Time series giving the monthly deaths from bronchitis, emphysema and asthma in the UK, 1974–1979, males only."
    return(list(series = series, description = description))
  } else if (sent_var == "lynx") {
    series <- datasets::lynx
    description <- "Annual numbers of lynx trappings for 1821–1934 in Canada. Taken from Brockwell & Davis (1991), this appears to be the series considered by Campbell & Walker (1977)."
    return(list(series = series, description = description))
  } else if (sent_var == "USAccDeaths") {
    series <- datasets::USAccDeaths
    description <- "A time series giving the monthly totals of accidental deaths in the USA, 1973 - 1978."
    return(list(series = series, description = description))
  }
}

# Run the application 
shinyApp(ui = ui, server = server)


# helper functions for shiny lecture act

# computes logistic growth for Motivation example
logistic_growth <- function(N_0, r, K, projection_interval, harvested, catch_size) {
  N <- numeric(projection_interval)
  N[1] <- N_0
  for(t in 1:projection_interval) {
    if(harvested) {
      N[t+1] <- max(N[t] + r*N[t]*(1 - N[t]/K) - catch_size, 0)
    } else {
      N[t+1] <- max(N[t] + r*N[t]*(1 - N[t]/K), 0)
    }
  }
  return(N)
}

# selects data for Fundamentals example
selectData <- function(sent_var) {
  #nonchar <- input
  #iris[, c(input$xcol, input$ycol)]
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

# computes fibonacci sequence for Reactions example
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

# computes fibunacci sequence runtimes for Reactions example
computeTimes <- function(n) {
  times <- numeric(n)
  for (i in 1:length(times)) {
    times[i] <- fib(i)$runTime
  }
  return(times)
}
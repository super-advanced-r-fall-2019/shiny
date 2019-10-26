# compute logistic growth with optional constant harvest
# takes initial popuation size, growth rate, carrying capacity, number of years to project
# also whether or not population is harvested and at what catch size
# returns time series of projected population size
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

# what type of animal
# where?
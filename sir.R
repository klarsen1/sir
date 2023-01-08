library("deSolve")
library(dplyr)
library(ggplot2)

### Helper functions copied from here: https://rpubs.com/choisy/sir

sir_equations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -beta * I * S
    dI <-  beta * I * S - gamma * I
    dR <-  gamma * I
    return(list(c(dS, dI, dR)))
  })
}

run_sir <- function(time_values, tam,  cohort_size, beta, gamma, lim){
  parameters_values <- c(
    beta  = beta,
    gamma = gamma  
  )
  initial_values <- c(
    S = tam,  
    I = cohort_size,  
    R = 0   
  )
  v <- data.frame(ode(
    y = initial_values,
    times = time_values,
    func = sir_equations,
    parms = parameters_values
  ))
  g <- ggplot(data=v, aes(x=time, y=I)) + 
    geom_line(size=1.2) +  
    scale_y_continuous(labels=scales::comma, limits=lim) +
    xlab("Time") + ylab("Active Customers (originating from the 1k cohort)")
  return(list(v, g))
}

########### Run some scenarios

### R0=.6, churn=10%
churn_rate=.1
b <- .6*churn_rate/tam
run_sir(time_values=seq(0, 36), tam=1000000, cohort_size=1000, beta=b, gamma=churn_rate, lim=c(0,1000))[[2]] 

### R0=1.4, churn=10%
churn_rate=.1
b <- 1.4*churn_rate/tam
run_sir(time_values=seq(0, 36), tam=1000000, cohort_size=1000, beta=b, gamma=churn_rate, lim=c(0,4500))[[2]] 

### Same beta as above, but churn=15%
churn_rate <- .14
run_sir(time_values=seq(0, 36), tam=1000000, cohort_size=1000, beta=b, gamma=churn_rate, lim=c(0,1500))[[2]] 

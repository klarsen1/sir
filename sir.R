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

run_sir <- function(time_values, tam,  cohort_size, r0, churn_rate, lim){
  parameters_values <- c(
    beta  = r0*churn_rate/tam,
    gamma = churn_rate  
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
    xlab("Time") + ylab("Active Customers")
  return(list(v, g))
}

########### Run some scenarios

### R0=.6, churn=10%
run_sir(time_values=seq(0, 36), tam=1000000, cohort_size=1000, r0=.6, churn_rate=.1, lim=c(0,1000))[[2]] 

### R0=1.4, churn=10%
run_sir(time_values=seq(0, 36), tam=1000000, cohort_size=1000, r0=1.4, churn_rate=.1, lim=c(0,4500))[[2]] 

### R0=.6, churn=7%
run_sir(time_values=seq(0, 36), tam=1000000, cohort_size=1000, r0=.6, churn_rate=.07, lim=c(0,1000))[[2]] 

### R0=1, churn=10%
run_sir(time_values=seq(0, 36), tam=1000000, cohort_size=1000, r0=1, churn_rate=.1, lim=c(0,1000))[[2]] 

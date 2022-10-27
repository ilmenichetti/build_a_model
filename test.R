library(deSolve)

exp_diff<-function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    C    <- C*-k
    return(list(c(C)))
  })
}

Time <- seq(0, 20, by = 1)
State <- c(C=80)
Pars <- c(k=0.15)

output   <- ode(State, Time, exp_diff, Pars)

  
plot(output[,1], output[,2], type="l", xlab="time", ylab=expression(paste(C[t])))


#reading the data we want to calibrate on
target_data<-read.csv("Ultuna_LTBF_data.csv")


#putting the data in the right place
Time <- target_data$BF_duration
State <- c(C=target_data$Mean_C.stock_t.ha[1])

library(Metrics) #to include the function to calculate the RMSE


exp_optim<-function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    
    output   <- ode(State, Time, exp_diff, Pars) #here you are using the solver and the ODE function we wrote before 
    #to calculate a simulation with a certain paramter setup
    fitness  <- rmse(output, target_data$Mean_C.stock_t.ha) # here you are calculating the fitness of your model on the data
    
    return(fitness) #this function must return one single value as object
  })
}



exp_optim_wrapper<-function(x){exp_optim(Time, State, Pars=c(k=x))}

opt_results<-optimize(exp_optim_wrapper,  lower = 0.01, upper = 1)


Pars_opt<-c(k=opt_results$minimum)

simulation<-as.data.frame(ode(State, Time, exp_diff, Pars_opt))


plot(target_data$BF_duration, target_data$Mean_C.stock_t.ha, ylim=c(0,50), ylab=expression(paste("C stocks t ",ha^-1)), xlab="Time", pch=16)
lines(simulation$time, simulation$C, lty=2, lwd=2, col="red")
legend("topright", c("Measured data", "Model simulation"), bty="n", pch=c(16, NA), lty=c(NA, 2), lwd=c(NA, 2), col=c("black", "red"))


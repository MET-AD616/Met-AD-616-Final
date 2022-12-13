

library(tidyverse)


###############################################################
#Simulation accounting for bus capacity

#Import and pivot data
Peak <- read.csv("PeakData.csv", header = T)[-1,]
NonPeak <- read.csv("NonPeakData.csv", header = T)[-1,]
Peak_p <- pivot_longer(Peak, cols = c(-Bus.Stop.Name),
                       names_to = "Destination",
                       values_to = "Mean_Passengers") %>%
  mutate(num_origin = rep(1:21,each=21),
         num_dest = rep(1:21,times=21))
NonPeak_p <- pivot_longer(NonPeak, cols = c(-Bus.Stop.Name),
                       names_to = "Destination",
                       values_to = "Mean_Passengers") %>%
  mutate(num_origin = rep(1:21,each=21),
         num_dest = rep(1:21,times=21))

#North route for Peak Hours
Peak_n <- filter(Peak_p, num_origin < num_dest)
#South route for Peak Hours
Peak_s <- filter(Peak_p, num_origin > num_dest) %>%
  mutate(num_origin = -1*(num_origin-22), num_dest = -1*(num_dest-22))
#North route for NonPeak Hours
NonPeak_n <- filter(NonPeak_p, num_origin < num_dest)
#South route for NonPeak Hours
NonPeak_s <- filter(NonPeak_p, num_origin > num_dest) %>%
  mutate(num_origin = -1*(num_origin-22), num_dest = -1*(num_dest-22))


#Function that determines who gets left off the bus if the limit is met
#*Only used within one_run function
limit_adjust <- function(d, num_change,stop){
  vals <- filter(d,num_origin == stop)$Mean_Passengers
  dest_start <- stop + 1
  vals_long <- rep(dest_start:21,times=vals)
  ran_choice <- sample(vals_long, size=num_change, replace = F)
  
  for (num in ran_choice) {
    d$Mean_Passengers[d$num_origin==stop & d$num_dest == num] <- d$Mean_Passengers[d$num_origin==stop & d$num_dest == num]-1
  }
  return(d)
  
}

#Runs one simulation for the bus routes
one_run <- function(d, limit, buses){
  d$Mean_Passengers <- rnorm(n=length(d$Mean_Passengers), mean = d$Mean_Passengers, sd = d$Mean_Passengers/5) %>% round()
  
  d <- mutate(d, Mean_Passengers = Mean_Passengers%/%buses + ifelse(runif(1,0,1) > Mean_Passengers%%buses/buses, 0,1))
  
  result <- data.frame(passengers = rep(0,times=21),total=rep(0,times=21))
  
  for (stop in 1:21){
    get_on <- sum(filter(d,num_origin == stop)$Mean_Passengers)
    get_off <- sum(filter(d,num_dest == stop)$Mean_Passengers)
    
    result[stop,1] <- get_on-get_off
    result$total <- cumsum(result$passengers)
    
    if (result[stop,2] > limit) {
      num_change <- result[stop,2] - limit
      d <- limit_adjust(d,num_change,stop)
      
      get_on <- sum(filter(d,num_origin == stop)$Mean_Passengers)
      get_off <- sum(filter(d,num_dest == stop)$Mean_Passengers)
      
      result[stop,1] <- get_on-get_off
      result$total <- cumsum(result$passengers)
    }
  }
  return(result$total)
}


#Runs a simulation for multiple trials of the bus route
#d : dataset (either Peak_n, Peak_s, NonPeak_n, or NonPeak_s)
#trials: Number of trials for simulation
#limit: The maximum amount of people that can fit on the bus
#buses: The amount of buses available
#north: boolean, True for north route, False for south route
run_sim <- function(d, trials, limit, buses, north){
  sim <- one_run(d, limit,buses)
  for (run in 2:trials) {
    sim <- rbind(sim,one_run(d,limit,buses))
  }
  sim <- data.frame(sim)
  if (north) {
    colnames(sim) <- make.names(Peak$Bus.Stop.Name)
  }
  else{
    colnames(sim) <- make.names(rev(Peak$Bus.Stop.Name))
  }
    rownames(sim) <- 1:nrow(sim)
  return(sim)
  
}

#run_sim only function that should be run
#Warning: Simulation slow... takes about 30 seconds for 200 trials
y <- run_sim(d = Peak_n,
             trials = 200,
             limit = 200,
             buses = 20,
             north = T)


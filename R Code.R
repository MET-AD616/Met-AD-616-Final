
library(ggridges)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)

#Boyu & Jack data manipulation
## import data
Peak <- read.csv("PeakData.csv", header = T)
NonPeak <- read.csv("NonPeakData.csv", header = T)
Peak_1 <- Peak[-1,-1]
NonPeak_1 <- NonPeak[-1,-1]
Peak_RG <- Peak_1 
Peak_RG[lower.tri(Peak_1)] <- 0 



### Simulate Matrix
## Peak_RG SIM
## lower.tri
Peak_RG_SIM <- array(dim=c(21,21,200))
for (i in 1:21){
  for(j in 1:21){
    Peak_RG_SIM [j,i,] <- rnorm(n=200, mean=Peak_RG[j,i,], sd=Peak_RG[j,i,]/5) %>% round()
  }
}


Peak_RG_SIM2 <- array(dim=c(21,3,200))
x1<-array(dim=c(1,21,200))
y1<-array(dim=c(1,21,200))
z1<-array(dim=c(1,21,200))
for (i in 1:200){
  x1[,,i]<-cumsum(rowSums(Peak_RG_SIM[,,i],na.rm = TRUE))
  y1[,,i]<-cumsum(colSums(Peak_RG_SIM[,,i],na.rm = TRUE))  
  z1 <- x1-y1
  Peak_RG_SIM2[,,i]<- as.matrix(cbind(x1[,,i],y1[,,i],z1[,,i]))
}
z1
Peak_RG_SIM2
Peak_my_data <- as.data.frame.table(z1)
#add risk profile
library(tidyr)
d1 <- pivot_wider(data = Peak_my_data, names_from = "Var2", values_from = "Freq")
d1 <- d1[-c(1,2)]
d1_final <- t(d1)
d1_final <- d1_final / 20

hplot <- function(x){
  ggplot()+geom_histogram(aes(x=x,y=..count../sum(..count..)),
                          color="dark blue", fill="blue", binwidth = 0.8)+
    geom_vline(xintercept = 51, linetype="dotted", color = "gray", size=1.5)+
    theme_bw() + ylab("probability")+
    scale_x_continuous(labels = scales::comma)
}

library(cowplot)
p1<-hplot(d1_final[1,]) + xlab("Rajiv.Gandhi")
p2<-hplot(d1_final[2,]) + xlab("Mata.Gujri")
p5<-hplot(d1_final[5,]) + xlab("Holkar.Subway")
p6<-hplot(d1_final[6,]) + xlab("Navlakha.Square")
p7<-hplot(d1_final[7,]) + xlab("Indra.Pratima")
p9<-hplot(d1_final[9,]) + xlab("Shivaji.Vatika")

plot_grid(p1,p2,p5,p6,p7,p9)

## upper.tri
Peak_RG2 <- Peak_1 
Peak_RG2[upper.tri(Peak_1)] <- 0 
upPeak_RG_SIM <- array(dim=c(21,21,200))
for (i in 1:21){
  for(j in 1:21){
    upPeak_RG_SIM [j,i,] <- rnorm(n=200, mean=Peak_RG2[j,i,], sd=Peak_RG2[j,i,]/5) %>% round()
  }
}


upPeak_RG_SIM2 <- array(dim=c(21,3,200))
x2<-array(dim=c(1,21,200))
y2<-array(dim=c(1,21,200))
z2<-array(dim=c(1,21,200))
for (i in 1:200){
  x2[,,i]<-cumsum(rowSums(upPeak_RG_SIM[,,i],na.rm = TRUE))
  y2[,,i]<-cumsum(colSums(upPeak_RG_SIM[,,i],na.rm = TRUE))  
  z2 <- y2-x2
  upPeak_RG_SIM2[,,i]<- as.matrix(cbind(x2[,,i],y2[,,i],z2[,,i]))
}
z2
upPeak_RG_SIM2

## Non-Peak
## lower.tri
NonPeak <- read.csv("NonPeakData.csv", header = T)
NonPeak_1 <- NonPeak[-1,-1]
NonPeak_RG <- NonPeak_1 
NonPeak_RG[lower.tri(NonPeak_1)] <- 0

NonPeak_RG_SIM <- array(dim=c(21,21,200))
for (i in 1:21){
  for(j in 1:21){
    NonPeak_RG_SIM [j,i,] <- rnorm(n=200, mean=NonPeak_RG[j,i,], sd=NonPeak_RG[j,i,]/5) %>% round()
  }
}


NonPeak_RG_SIM2 <- array(dim=c(21,3,200))
x3<-array(dim=c(1,21,200))
y3<-array(dim=c(1,21,200))
z3<-array(dim=c(1,21,200))
for (i in 1:200){
  x3[,,i]<-cumsum(rowSums(NonPeak_RG_SIM[,,i],na.rm = TRUE))
  y3[,,i]<-cumsum(colSums(NonPeak_RG_SIM[,,i],na.rm = TRUE))  
  z3 <- x3-y3
  NonPeak_RG_SIM2[,,i]<- as.matrix(cbind(x3[,,i],y3[,,i],z3[,,i]))
}
z3
NonPeak_RG_SIM2


## upper.tri
upNonPeak_RG <- NonPeak_1 
upNonPeak_RG[upper.tri(NonPeak_1)] <- 0

upNonPeak_RG_SIM <- array(dim=c(21,21,200))
for (i in 1:21){
  for(j in 1:21){
    upNonPeak_RG_SIM [j,i,] <- rnorm(n=200, mean=upNonPeak_RG[j,i,], sd=upNonPeak_RG[j,i,]/5) %>% round()
  }
}


upNonPeak_RG_SIM2 <- array(dim=c(21,3,200))
x4<-array(dim=c(1,21,200))
y4<-array(dim=c(1,21,200))
z4<-array(dim=c(1,21,200))
for (i in 1:200){
  x4[,,i]<-cumsum(rowSums(upNonPeak_RG_SIM[,,i],na.rm = TRUE))
  y4[,,i]<-cumsum(colSums(upNonPeak_RG_SIM[,,i],na.rm = TRUE))  
  z4 <- y4-x4
  upNonPeak_RG_SIM2[,,i]<- as.matrix(cbind(x4[,,i],y4[,,i],z4[,,i]))
}
z4
upNonPeak_RG_SIM2

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



## Passing buses: # of buses passing each stop
## Active buses: # of buses in-operation 

## Risk Profile for crowdedness / different # Passing buses 

## define crowdedness 
crowd <- function(x, n){
  #each standing customers will increase 3.3 crowdedness level
  a1 <- pmin(100,(x/n-36)*3.3)
  S1 <- hist(a1, breaks = 20, plot = F)
  S1$counts = S1$counts/sum(S1$counts)
  plot(S1, xlab = "crowd level", 
       ylab = "Probability", main = NULL)
}

## AVG scenario: Peak Hours with avg demand
# Applying 60 passing buses to all stops
# View crowdedness from Rajiv Gandhi to Niranjanpur Sq.
par(mfrow=c(3,3))
crowd(df_z1[1,], 60) + title(main = "Stop 1")
crowd(df_z1[2,], 60) + title(main = "Stop 2")
crowd(df_z1[3,], 60) + title(main = "Stop 3")
crowd(df_z1[4,], 60) + title(main = "Stop 4")
crowd(df_z1[5,], 60) + title(main = "Stop 5")
crowd(df_z1[6,], 60) + title(main = "Stop 6")
crowd(df_z1[7,], 60) + title(main = "Stop 7")
crowd(df_z1[8,], 60) + title(main = "Stop 8")
crowd(df_z1[9,], 60) + title(main = "Stop 9")
mtext("Peak Hour 1st Trip", side = 3, line = -1.5, outer = T)

par(mfrow=c(3,3))
crowd(df_z1[10,], 60) + title(main = "Stop 10")
crowd(df_z1[11,], 60) + title(main = "Stop 11")
crowd(df_z1[12,], 60) + title(main = "Stop 12")
mtext("Peak Hour 1st Trip", side = 3, line = -1.5, outer = T)

par(mfrow=c(3,3))
crowd(df_z1[12,], 60) + title(main = "Stop 12")
crowd(df_z1[13,], 60) + title(main = "Stop 13")
crowd(df_z1[14,], 60) + title(main = "Stop 14")
crowd(df_z1[15,], 60) + title(main = "Stop 15")
crowd(df_z1[16,], 60) + title(main = "Stop 16")
crowd(df_z1[17,], 60) + title(main = "Stop 17")
crowd(df_z1[18,], 60) + title(main = "Stop 18")
crowd(df_z1[19,], 60) + title(main = "Stop 19")
crowd(df_z1[20,], 60) + title(main = "Stop 20")
mtext("Peak Hour 1st Trip", side = 3, line = -1.5, outer = T)


## Sensitivity Analysis 
# Number of active buses impacts on crowdedness for specific stop (Peak hours) 
# Crowdedness for stop 5, applying different passing buses to stop 5  
par(mfrow=c(3,3))
crowd(df_z1[5,], 48) + title(main = "Stop 5,48 buses")
crowd(df_z1[5,], 49) + title(main = "Stop 5,49 buses")
crowd(df_z1[5,], 50) + title(main = "Stop 5,50 buses")
crowd(df_z1[5,], 51) + title(main = "Stop 5,51 buses")
crowd(df_z1[5,], 52) + title(main = "Stop 5,52 buses")
crowd(df_z1[5,], 53) + title(main = "Stop 5,53 buses")
crowd(df_z1[5,], 54) + title(main = "Stop 5,54 buses")


## Sensitivity Analysis
# Number of active buses impacts on crowdedness for the entire trip (Peak hours) 

# Define under crowdedness level 50 as comfortable
crowdlevelunder50 <-function(ds,n){
  crowdlevel <- pmin(100,(ds/n-36)*3.3)
  return(
    mean(crowdlevel) < 50
  ) 
}

## Under crwodedness level 50 will return true
# Number of comfortable stops by total number passing buses

# 25-35 active buses in-operation and 3-5 minutes headway for Peak Hours (4 hours)
# result passing buses range (48:80)
# from Rajiv Gandhi to Niranjanpur Sq.
result <- list()
for (i in 48:80){
  result_nested <- list()
  for (k in 1:20){
    result_nested[[k]] <-crowdlevelunder50(df_z1[k],i) 
  }
  result[[i]] <-result_nested
}

# Convert list to dataframe 
df_Ph1 <- as.data.frame(matrix(unlist(result), ncol = max(lengths(result)), byrow = TRUE))
row.names(df_Ph1) <- 48:80
totalTrue = rowSums(df_Ph1)
df_Ph1$totalTrue <-totalTrue
df_Ph1$passingBuses <- as.numeric(row.names(df_Ph1))

# Number of comfortable stops by total number passing buses 
ggplot (aes(x=passingBuses, y=totalTrue,label=passingBuses),data=df_Ph1) +
  labs(y= "number of comfortable stops", x = "Total number passing buses")+
  geom_line()+
  geom_point()+
  geom_text(aes(label=ifelse(totalTrue == unique(totalTrue),as.numeric(passingBuses),'')),hjust=0,vjust=0)

##James and Tong's risk profile
##data transform
df_z1 <- t(data.frame(matrix(z1, nrow=200, byrow = T))) %>% round()

df_z2 <- t(data.frame(matrix(z2, nrow=200, byrow = T))) %>% round()

df_z3 <- t(data.frame(matrix(z3, nrow=200, byrow = T))) %>% round()

df_z4 <- t(data.frame(matrix(z4, nrow=200, byrow = T))) %>% round()

## Risk profile

##risk profile by James

#the great function of risk profile
RP_James <- function(x, n){
  a <- x/n
  S <- hist(a, breaks = 20, plot = F)
  S$counts = S$counts/sum(S$counts)
  if (min(a)<=51){p = min(a)}
  if (min(a)>=51){p = 51}
  if (max(a)<=51){q = 51}
  if (max(a)>=51){q = max(a)}
  plot(S, xlim = c(p, q), xlab = "# of Passenger", 
       ylab = "Probability", main = NULL)
  abline(v=51, col = "red", lty = 2)
}
#RP_James(df_z1[1,], 30)

RP_adjust_James <- function(x, n){
  a <- x/n
  hist_breaks <- hist(a, plot = F)$breaks
  color_list <- rep('red', length(hist_breaks))
  color_list[hist_breaks < 65] <- 'yellow'
  color_list[hist_breaks < 51] <- 'blue'
  color_list[hist_breaks < 36] <- 'green'
  S <- hist(a, plot = F)
  S$counts = S$counts/sum(S$counts)
  plot(S, col = color_list, xlim = c(min(a)-3, max(a)+3), xlab = "# of Passenger per Bus", 
       ylab = "Probability", main = NULL)
}

#RP_adjust_James(df_z1[1,], 30)
par(mfrow=c(1,3))
RP_adjust_James(df_z1[2,], 30) + title(main = "Stop 2: Mata Gujri / 30 Buses")
RP_adjust_James(df_z2[4,], 30) + title(main = "Stop 4: Bhanwarkuan Square / 30 Buses")
RP_adjust_James(df_z1[4,], 30) + title(main = "Stop 4")
mtext("Peak Hour 1st Trip", side = 3, line = -1.5, outer = T)


##Peak hours, RG final stop
Peak <- read.csv("PeakData.csv", header = T)
Peak_1 <- Peak[-1,-1]
#View(Peak_1)
#Calculate the difference between each stop
Peak_RG <- Peak_1
Peak_RG[lower.tri(Peak_1)] <- 0

x <- cumsum(rowSums(Peak_RG, na.rm = TRUE))
y <- cumsum(colSums(Peak_RG, na.rm = TRUE))
df <- data.frame(x, y) %>% mutate(z = x- y)
#view(df)
#difference of number of people between each stop
difference <- diff(as.matrix(df$z)) %>% as.data.frame()
colnames(difference) <- c('difference')

difference1 <- data.frame(difference = 0)

difference_final <- difference1 %>% 
  rows_insert(difference)

difference_final <- data.frame(val = difference_final, name = colnames(Peak_RG))

sum_difference <- ggplot(data = difference_final, aes(x = factor(name, level=
                                                                   c('Rajiv.Gandhi','Mata.Gujri','Vishnupuri','Bhanwarkuan.Square','Holkar.Subway','Navlakha.Square',
                                                                     'Indra.Pratima','GPO','Shivaji.Vatika','AICTSL','Geeta.Bhawan','Palasiya',
                                                                     'Industry.House','LIG','Press.Complex','MR.9','Vijay.Nagar','Satya.Sai',
                                                                     'Shalimar.Township','Scheme.No..78','Niranjanpur.Square')), y = difference)) + 
  geom_col(width = 0.01, color = "black") +
  geom_point(size = 3, color = "red", fill = alpha("orange", 0.3), alpha = 0.7, shape = 21, stroke = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(sum_difference + ggtitle("Difference of number of people between each stop") + 
        labs(y = "Number of Passenger", x = "Bus Stop"))

#risk profile
hplot <- function(x){
  ggplot()+geom_histogram(aes(x=x,y=..count../sum(..count..)),
                          color="dark blue", fill="blue", binwidth = 50)+
    theme_bw() + ylab("probability")+
    scale_x_continuous(labels = scales::comma)
}

hplot(difference_final$difference) + xlab("Difference between Stops")






##Jayden's Sensitivity Analysis
# Maximize comfortable stops
# stop 8 is the most crowded one
crowdlevelunder50(df_z1[8,],94 ) 
crowdlevelunder50(df_z1[8,],95 )
# 95 is the least passing buses number required for maintaining all stops under 50 crowdedness level

# From Niranjanpur Sq. to Rajiv Gandhi 
par(mfrow=c(3,3))
RP_James(df_z2[7,], 60) + title(main = "Stop 1")
RP_James(df_z2[8,], 60) + title(main = "Stop 8")
RP_James(df_z2[9,], 60) + title(main = "Stop 9")
RP_James(df_z2[10,], 60) + title(main = "Stop 10")
RP_James(df_z2[11,], 60) + title(main = "Stop 11")
RP_James(df_z2[12,], 60) + title(main = "Stop 12")
RP_James(df_z2[13,], 60) + title(main = "Stop 13")
mtext("Peak Hour 2nd Trip", side = 3, line = -1.5, outer = T)

#Check the highest demand
crowdlevelunder50(df_z2[11,],105 )
crowdlevelunder50(df_z2[8,],105 )
crowdlevelunder50(df_z2[9,],105 )
crowdlevelunder50(df_z2[10,],105 )

crowdlevelunder50(df_z2[9,],104 )
# 105 is the least passing buses number required for maintaining all stops under 50 crowdedness level

## Under crwodedness level 50 will return true
# Number of comfortable stops by total number passing buses 
result2 <- list()
for (i2 in 48:80){
  result_nested2 <- list()
  for (k2 in 1:20){
    result_nested2[[k2]] <-crowdlevelunder50(df_z2[k2],i2) 
  }
  result2[[i2]] <-result_nested2
}

# Convert list to dataframe 
df_Ph2 <- as.data.frame(matrix(unlist(result2), ncol = max(lengths(result2)), byrow = TRUE))
row.names(df_Ph2) <- 48:80
totalTrue2 = rowSums(df_Ph2)
df_Ph2$totalTrue2 <-totalTrue2
df_Ph2$passingBuses2 <- as.numeric(row.names(df_Ph2))

# Number of comfortable stops by total number passing buses 
ggplot (aes(x=passingBuses2, y=totalTrue2,label=passingBuses2),data=df_Ph2) +
  labs(y= "number of comfortable stops", x = "Total number passing buses")+
  geom_line()+
  geom_point()+
  geom_text(aes(label=ifelse(totalTrue2 == unique(totalTrue2),as.numeric(passingBuses2),'')),hjust=0,vjust=0)


#compute the demand of active buses for maintaining all stops (back trip) under 50 crowdedness 

#Assuming increasing the number of active buses (25-35) would 
#decrease the headway (3-5 during peak hours) between each bus, 
#each additional active bus would decrease 0.2 minutes for the headway, 
#lower bound at 3 minutes. we got the formula that 
#compute number of active buses (x) with input of total number of buses (A) passing each stop: x = (10 ?C 240/A) * 5 

PH_required_active_buses <- (10-240/105)*5
PH_required_active_buses
# 38-39 number of active buses is required for achieving the goal of allowing all stops to be comfortable (all trip)





## Number of active buses impacts on crowdedness for the entire trip (Non Peak hours)
# 25-35 active buses in-operation and 7-10 minutes headway for Non Peak Hours (3 hours)
# result passing buses range (18:26)
# From Rajiv Gandhi to Niranjanpur Sq.

par(mfrow=c(3,3))
RP_James(df_z3[3,], 60) + title(main = "Stop 1")
RP_James(df_z3[4,], 60) + title(main = "Stop 2")
RP_James(df_z3[5,], 20) + title(main = "Stop 3")
RP_James(df_z3[6,], 20) + title(main = "Stop 8")
RP_James(df_z3[7,], 20) + title(main = "Stop 9")
RP_James(df_z3[8,], 20) + title(main = "Stop 10")
RP_James(df_z3[9,], 20) + title(main = "Stop 17")
RP_James(df_z3[10,], 20) + title(main = "Stop 18")
RP_James(df_z3[11,], 20) + title(main = "Stop 19")
mtext("Non Peak Hour 1st Trip", side = 3, line = -1.5, outer = T)

## Under crwodedness level 50 will return true
# Number of comfortable stops by total number passing buses 
result3 <- list()
for (i3 in 18:26){
  result_nested3 <- list()
  for (k3 in 1:20){
    result_nested3[[k3]] <-crowdlevelunder50(df_z3[k3],i3) 
  }
  result3[[i3]] <-result_nested3
}

# Convert list to dataframe 
df_NPh1 <- as.data.frame(matrix(unlist(result3), ncol = max(lengths(result3)), byrow = TRUE))
row.names(df_NPh1) <- 18:26
totalTrue3 = rowSums(df_NPh1)
df_NPh1$totalTrue3 <-totalTrue3
df_NPh1$passingBuses3 <- as.numeric(row.names(df_NPh1))

# Number of comfortable stops by total number passing buses 
ggplot (aes(x=passingBuses3, y=totalTrue3,label=passingBuses3),data=df_NPh1) +
  labs(y= "number of comfortable stops", x = "Total number passing buses")+
  geom_line()+
  geom_point()+
  geom_text(aes(label=ifelse(totalTrue3 == unique(totalTrue3),as.numeric(passingBuses3),'')),hjust=0,vjust=0)


#Assuming increasing the number of active buses (25-35) would 
#decrease the headway (7-10 minutes during peak hours) between each bus, 
#each additional active bus would decrease 0.3 minutes for the headway, 
#lower bound at 7 minutes. we got the formula that 
#compute number of active buses (x) with input of total number of buses (A) passing each stop: x = (17.5 ?C 240/A) * (10/3) 
NPH_required_active_buses <- (17.5- 180/18 )* (10/3) 
NPH_required_active_buses


## From Niranjanpur Sq. to Rajiv Gandhi 
par(mfrow=c(3,3))
RP_James(df_z4[3,], 20) + title(main = "Stop 3")
RP_James(df_z4[4,], 20) + title(main = "Stop 4")
RP_James(df_z4[5,], 20) + title(main = "Stop 5")
RP_James(df_z4[6,], 20) + title(main = "Stop 6")
RP_James(df_z4[7,], 20) + title(main = "Stop 7")
RP_James(df_z4[8,], 20) + title(main = "Stop 8")
RP_James(df_z4[9,], 20) + title(main = "Stop 9")
RP_James(df_z4[10,], 20) + title(main = "Stop 10")
RP_James(df_z4[11,], 20) + title(main = "Stop 11")
mtext("Non Peak Hour 2nd Trip", side = 3, line = -1.5, outer = T)

#Check the highest demand
crowdlevelunder50(df_z4[9,],26 )
crowdlevelunder50(df_z4[10,],26 )
crowdlevelunder50(df_z4[11,],26 )

crowdlevelunder50(df_z4[10,],25 )


## Under crwodedness level 50 will return true
# Number of comfortable stops by total number passing buses 
result4 <- list()
for (i4 in 18:26){
  result_nested4 <- list()
  for (k4 in 1:20){
    result_nested4[[k4]] <-crowdlevelunder50(df_z4[k4],i4) 
  }
  result4[[i4]] <-result_nested4
}

# Convert list to dataframe 
df_NPh2<- as.data.frame(matrix(unlist(result4), ncol = max(lengths(result4)), byrow = TRUE))
row.names(df_NPh2) <- 18:26
totalTrue4 = rowSums(df_NPh2)
df_NPh2$totalTrue4 <-totalTrue4
df_NPh2$passingBuses4 <- as.numeric(row.names(df_NPh2))

# Number of comfortable stops by total number passing buses 
ggplot (aes(x=passingBuses4, y=totalTrue4,label=passingBuses4),data=df_NPh2) +
  labs(y= "number of comfortable stops", x = "Total number passing buses")+
  geom_line()+
  geom_point()+
  geom_text(aes(label=ifelse(totalTrue4 == unique(totalTrue4),as.numeric(passingBuses4),'')),hjust=0,vjust=0)

view(df_NPh2)
crowdlevelunder50(df_z4[10,],26 )





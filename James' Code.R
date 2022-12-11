## Initial place for coding by James
library(ggplot2)
library(ggridges)
library(tidyr)
library(dplyr)
library(tidyverse)

##Peak hours, RG final stop
Peak <- read.csv("PeakData.csv", header = T)
Peak_1 <- Peak[-1,-1]
#View(Peak_1)

Peak_RG <- Peak_1
Peak_RG[lower.tri(Peak_1)] <- 0
Peak_RG1 <- cbind(Peak_RG, c(rowSums(Peak_RG)))
Peak_RG2 <- rbind(Peak_RG1, c(colSums(Peak_RG1)))
#View(Peak_RG2)

#sum of people get off the bus at each stop
P_RG2 <- data.frame(val = Peak_RG2[-22,22], name = colnames(Peak_RG))
Sum_GetOff1 <- ggplot(data = P_RG2, aes(x = factor(name, level=
                                                     c('Rajiv.Gandhi','Mata.Gujri','Vishnupuri','Bhanwarkuan.Square','Holkar.Subway','Navlakha.Square',
                                                       'Indra.Pratima','GPO','Shivaji.Vatika','AICTSL','Geeta.Bhawan','Palasiya',
                                                       'Industry.House','LIG','Press.Complex','MR.9','Vijay.Nagar','Satya.Sai',
                                                       'Shalimar.Township','Scheme.No..78','Niranjanpur.Square')), y = val)) + 
  geom_col(width = 0.01, color = "black") +
  geom_point(size = 3, color = "red", fill = alpha("orange", 0.3), alpha = 0.7, shape = 21, stroke = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(Sum_GetOff1 + ggtitle("Sum of passengers get off the bus at each stop") + 
        labs(y = "Number of Passenger", x = "Bus Stop"))

#sum of people get on the bus at each stop
P_RG3 <- data.frame(val = unlist(Peak_RG2[22,-22]), name = colnames(Peak_RG))
Sum_GetOn1 <- ggplot(data = P_RG3, aes(x = factor(name, level=
                                                    c('Rajiv.Gandhi','Mata.Gujri','Vishnupuri','Bhanwarkuan.Square','Holkar.Subway','Navlakha.Square',
                                                      'Indra.Pratima','GPO','Shivaji.Vatika','AICTSL','Geeta.Bhawan','Palasiya',
                                                      'Industry.House','LIG','Press.Complex','MR.9','Vijay.Nagar','Satya.Sai',
                                                      'Shalimar.Township','Scheme.No..78','Niranjanpur.Square')), y = val)) + 
  geom_col(width = 0.01, color = "black") +
  geom_point(size = 3, color = "red", fill = alpha("orange", 0.3), alpha = 0.7, shape = 21, stroke = 2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(Sum_GetOn1 + ggtitle("Sum of passengers get on the bus at each stop") + 
        labs(y = "Number of Passenger", x = "Bus Stop"))

#Sum of on and off
P_RG4 <- data.frame(val = unlist(c(unlist(Peak_RG2[22,-22]), Peak_RG2[-22,22])), 
                    name = rep(colnames(Peak_RG),2), 
                    on_off = c(rep("off", 21), rep("on",21)))
Sum_1 <- ggplot(data = P_RG4, aes(x = factor(name, level=
                                               c('Rajiv.Gandhi','Mata.Gujri','Vishnupuri','Bhanwarkuan.Square','Holkar.Subway','Navlakha.Square',
                                                 'Indra.Pratima','GPO','Shivaji.Vatika','AICTSL','Geeta.Bhawan','Palasiya',
                                                 'Industry.House','LIG','Press.Complex','MR.9','Vijay.Nagar','Satya.Sai',
                                                 'Shalimar.Township','Scheme.No..78','Niranjanpur.Square')), 
                                  y = val, fill = on_off)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(Sum_1 + ggtitle("Sum of passengers get on and off the bus at each stop") + 
        labs(y = "Number of Passenger", x = "Bus Stop"))



##Peak hours, NS final stop
Peak_NS <- Peak_1
Peak_NS[upper.tri(Peak_1)] <- 0
#View(Peak_NS)


##For non-peak hours
NonPeak <- read.csv("NonPeakData.csv", header = T)
NonPeak_1 <- NonPeak[-1,]
View(NonPeak_1)


##data simulation

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
#View(z1)
#View(Peak_RG_SIM2)



##data transform
df_z1 <- t(data.frame(matrix(z1, nrow=200, byrow = T))/20) %>% round()
View(df_z1)

## Risk profile

PS_df_one <- data.frame(sim = 1:200, stop1 = t(PS_df[1,]/4))
rownames(PS_df_one) <- NULL
p_stop1 <- ggplot(data = PS_df_one, aes(x = sim, y = X1)) +
  geom_point()
p_stop1 + labs(y = "Number of Passenger / Hour", x = "Sim Trials") +
  ggtitle("Sum of passengers on the bus at 1st Stop (sim 200 times)")

#for the first stop
PS_df_one <- data.frame(sim = 1:200, stop1 = df_z1[1,])
stop_1 <- ggplot(data = PS_df_one, aes(x = sim)) +
  geom_density()

#for the second stop
PS_df_two <- data.frame(sim = 1:200, stop1 = df_z1[2,])
stop_2 <- ggplot(data = PS_df_two, aes(x = sim)) +
  geom_density()

#for the tenth stop
PS_df_ten <- data.frame(sim = 1:200, stop1 = df_z1[10,])
stop_10 <- ggplot(data = PS_df_ten, aes(x = sim)) +
  geom_histogram()

#for the nth stop
PS_df_17 <- data.frame(sim = 1:200, stop1 = df_z1[17,])
stop_17 <- ggplot(data = PS_df_17, aes(x = sim)) +
  geom_histogram()











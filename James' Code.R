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


##coding

p_1 <- unlist(as.list(t(Peak_1)))
p_1

PeakSim <- c(rep(0,441))
for (i in 1:441) {
  PeakSim[i] <- mean(rnorm(10000, mean = p_1[i], sd = p_1[i]/5))
}

PS_1 <- matrix(data = PeakSim, nrow = 21, ncol = 21)
PS_1[upper.tri(PS_1, diag = F)]


##data transform
x <- cumsum(rowSums(Peak_RG, na.rm = TRUE))
y <- cumsum(colSums(Peak_RG, na.rm = TRUE))
df <- data.frame(x, y) %>% mutate(z = x- y)

p_1 <- c(rep(df$z,200))
PeakSim <- c(rep(0,4200))

for (i in 1:4200) {
  PeakSim[i] <- rnorm(1, mean = p_1[i], sd = p_1[i]/5)
}

PS_df <- data.frame(matrix(data =PeakSim, ncol = 200, nrow = 21))
#View(PS_df)

PS_df_one <- data.frame(sim = 1:200, stop1 = t(PS_df[1,]/4))
rownames(PS_df_one) <- NULL
p_stop1 <- ggplot(data = PS_df_one, aes(x = sim, y = X1)) +
  geom_point()
p_stop1 + labs(y = "Number of Passenger / Hour", x = "Sim Trials") +
  ggtitle("Sum of passengers on the bus at 1st Stop (sim 200 times)")

PS_df_one <- data.frame(sim = 1:200, stop1 = t(PS_df[1,]/4))
rownames(PS_df_one) <- NULL
p_stop2 <- ggplot(data = PS_df_one, aes(x = sim)) +
  geom_histogram(binwidth = 20, alpha = 0.9)




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

Peak_RG2 <- Peak_1 
Peak_RG2[upper.tri(Peak_1)] <- 0 
#Reverses the column and row order of the matrix
Peak_RG2 <- Peak_RG2[, rev(seq_len(ncol(Peak_RG2)))]
Peak_RG2 <- Peak_RG2[rev(seq_len(nrow(Peak_RG2))), ]

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
  z2 <- x2-y2
  upPeak_RG_SIM2[,,i]<- as.matrix(cbind(x2[,,i],y2[,,i],z2[,,i]))
}
z2


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



## upper.tri
upNonPeak_RG <- NonPeak_1 
upNonPeak_RG[upper.tri(NonPeak_1)] <- 0
#Reverses the column and row order of the matrix
upNonPeak_RG <- upNonPeak_RG[, rev(seq_len(ncol(upNonPeak_RG)))]
upNonPeak_RG <- upNonPeak_RG[rev(seq_len(nrow(upNonPeak_RG))), ]

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


### simulation by James
#View(Peak_RG)

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

par(mfrow=c(2,3))
RP_James(df_z1[1,], 20) + title(main = "Stop 1")
RP_James(df_z1[2,], 20) + title(main = "Stop 2")
RP_James(df_z1[3,], 20) + title(main = "Stop 3")
RP_James(df_z1[4,], 20) + title(main = "Stop 4")
RP_James(df_z1[5,], 20) + title(main = "Stop 5")
RP_James(df_z1[6,], 20) + title(main = "Stop 6")

par(mfrow=c(2,3))
RP_James(df_z1[7,], 20) + title(main = "Stop 7")
RP_James(df_z1[8,], 20) + title(main = "Stop 8")
RP_James(df_z1[9,], 20) + title(main = "Stop 9")
RP_James(df_z1[10,], 20) + title(main = "Stop 10")
RP_James(df_z1[11,], 20) + title(main = "Stop 11")
RP_James(df_z1[12,], 20) + title(main = "Stop 12")

par(mfrow=c(2,3))
RP_James(df_z1[13,], 20) + title(main = "Stop 13")
RP_James(df_z1[14,], 20) + title(main = "Stop 14")
RP_James(df_z1[15,], 20) + title(main = "Stop 15")
RP_James(df_z1[16,], 20) + title(main = "Stop 16")
RP_James(df_z1[17,], 20) + title(main = "Stop 17")
RP_James(df_z1[18,], 20) + title(main = "Stop 18")

# for peak hour round trip
par(mfrow=c(3,3))
RP_James(df_z2[1,], 20) + title(main = "Stop 1")
RP_James(df_z2[2,], 20) + title(main = "Stop 2")
RP_James(df_z2[3,], 20) + title(main = "Stop 3")
RP_James(df_z2[8,], 20) + title(main = "Stop 8")
RP_James(df_z2[9,], 20) + title(main = "Stop 9")
RP_James(df_z2[10,], 20) + title(main = "Stop 10")
RP_James(df_z2[17,], 20) + title(main = "Stop 17")
RP_James(df_z2[18,], 20) + title(main = "Stop 18")
RP_James(df_z2[19,], 20) + title(main = "Stop 19")
mtext("Peak Hour Other Trip", side = 3, line = -1, outer = T)

# for non peak hour one-way trip
par(mfrow=c(3,3))
RP_James(df_z3[1,], 20) + title(main = "Stop 1")
RP_James(df_z3[2,], 20) + title(main = "Stop 2")
RP_James(df_z3[3,], 20) + title(main = "Stop 3")
RP_James(df_z3[8,], 20) + title(main = "Stop 8")
RP_James(df_z3[9,], 20) + title(main = "Stop 9")
RP_James(df_z3[10,], 20) + title(main = "Stop 10")
RP_James(df_z3[17,], 20) + title(main = "Stop 17")
RP_James(df_z3[18,], 20) + title(main = "Stop 18")
RP_James(df_z3[19,], 20) + title(main = "Stop 19")
mtext("Non Peak Hour 1st Trip", side = 3, line = -1.5, outer = T)

# for non peak hour round trip
par(mfrow=c(3,3))
RP_James(df_z4[1,], 20) + title(main = "Stop 1")
RP_James(df_z4[2,], 20) + title(main = "Stop 2")
RP_James(df_z4[3,], 20) + title(main = "Stop 3")
RP_James(df_z4[8,], 20) + title(main = "Stop 8")
RP_James(df_z4[9,], 20) + title(main = "Stop 9")
RP_James(df_z4[10,], 20) + title(main = "Stop 10")
RP_James(df_z4[17,], 20) + title(main = "Stop 17")
RP_James(df_z4[18,], 20) + title(main = "Stop 18")
RP_James(df_z4[19,], 20) + title(main = "Stop 19")
mtext("Non Peak Hour 2nd Trip", side = 3, line = -1.5, outer = T)




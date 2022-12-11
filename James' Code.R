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

### simulation by James
#View(Peak_RG)

##data transform
df_z1 <- t(data.frame(matrix(z1, nrow=200, byrow = T))) %>% round()
View(df_z1)

## Risk profile
max(df_z1)


#for the nth stop
PS_df_17 <- data.frame(sim = 1:200, stop1 = df_z1[17,])
stop_17 <- ggplot(data = PS_df_17, aes(x = stop1)) +
  geom_histogram()
#stop_17 + labs(y = "Count", x = "# of passenger at stop 17") +
  ggtitle("Sum of passengers on the bus at 17th Stop (sim 200 times)")

#probability histogram code from internet
probabilityplot<-function(x, ..., prob=T, ylab="Probability") {
  xx<-hist(x, yaxt="n", prob=prob, ylab=ylab , ...)
  bin.sizes<-diff(xx$breaks)
  if (any(bin.sizes != bin.sizes[1])) stop("bin sizes are not the same")
  marks<-axTicks(2)
  axis(2, at=marks, labels=marks*bin.sizes[1])
  xx$probabilities <- xx$density*bin.sizes[1]
  invisible(xx)
}

#probabilityplot(df_z1[14,])

#risk profile by James

S_14 <- hist(df_z1[14,], breaks = 20, plot = F)
S_14$counts = S_14$counts/sum(S_14$counts)
plot(S_14, xlim = c(40, 150), xlab = "# of Passenger", 
     ylab = "Probability", main = "Risk Profile for Stop 14")
abline(v=51, col = "red", lty = 2)

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












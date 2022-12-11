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

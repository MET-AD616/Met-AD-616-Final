library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyverse)
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


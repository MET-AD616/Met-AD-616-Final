# sensitivity analysis for 2nd stop with different # of buses
sensi_z1 <- df_z1[2,] %>% t()
df_final <- data.frame()
# correlations of people at 2nd stop with 10,20,30,40,50 buses
for (i in c(10,20,30,40,50)){
  model <- sensi_z1 / i
  df <- data.frame(model)
  df_final <- rbind(df_final, df)
}

df_final <- t(df_final)
colnames(df_final) <- c('10 buses','20 buses','30 buses','40 buses','50 buses')

Corrs <- cor(df_final)[1,][-1]
# bar chart
ggplot()+
  geom_bar(aes(x=reorder(names(Corrs), abs(Corrs)),
               y=Corrs, fill=(Corrs>0.5)),
           stat='identity', color='black')+
  coord_flip()+xlab('Random Variable') +
  ylab('Correlation')+
  scale_fill_manual(values = c('red','blue'), guide='none')+
  theme_classic()+
  ggtitle("Correlations between the # of people at the 10th stop and other stops")+
  theme(plot.title = element_text(hjust = 0.5))

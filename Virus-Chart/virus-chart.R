library(datasauRus)
library(ggplot2)
library(gganimate)

Gun <- c("11.03.20","12.03.20","13.03.20","14.03.20","15.03.20","16.03.20","17.03.20")
Vaka <- c(1,2,5,6,18,47,98)
virus <- data.frame(Gun,Vaka)

ggplot(virus, aes(x=Gun, y=Vaka,fill=Vaka))+
  geom_col()+
  scale_fill_distiller(palette(gray(seq(0,.7,len = 25))), direction = 2) +
  geom_text(aes(label = paste(Vaka,"")), vjust =-.2 ,col="#88398A")+
  shadow_mark()+
  enter_grow()+
  enter_fade()+
  theme_minimal() +
  theme(
    plot.title=element_text(color="#88398A",face="bold"),
    axis.title.x= element_text(color="#88398A",face="bold",vjust = -1),
    axis.title.y= element_text(color="#88398A",face="bold"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "Violet"),
    panel.ontop = TRUE,
    plot.margin = margin(2, 2, 2, 2, "cm"),
    legend.position="bottom",
    legend.title=element_blank()
  ) +
  labs(title= "                          Evinde otur Türkiye !!! ")+
  transition_states(Gun,wrap=FALSE)+
  ease_aes('cubic-in-out')

  anim_save("virus-chart.gif") 
  
  
  
  
  
  
  
  
  
  
  
  

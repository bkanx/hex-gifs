library(ggplot2)
library(gganimate)
Gun <- c("11.03.20","12.03.20","13.03.20","14.03.20","15.03.20",
         "16.03.20","17.03.20","18.03.20","19.03.20","20.03.20",
         "21.03.20","22.03.20","23.04.20")
Vaka <- c(1,2,5,6,18,47,98,191,359,670,947,1236,1579)
virus <- data.frame(Gun,Vaka)

library(gapminder)
library(lubridate)
library(tidyverse)
xmin <- ymd("2020/03/11")
xmax <- ymd("2020/03/22")
ymin <- 30
height <- round(as.numeric(xmax-xmin)/356, 0)
ymax <- ymin + height


library(magick)
library(here)
library(magrittr)

# Scale down the logo and give it a border and annotation
# This is the cool part because you can do a lot to the , *-+/=
# And bring in a logo

logo_raw <- image_read("R-LadiesGlobal-sticker.png") 
logo <- logo_raw %>%
  image_scale("100") %>% 
  image_background("white", flatten = TRUE) %>%
  image_border("white", "80x40") %>%
  image_annotate("by R Studio", color = "gray", size = 6, 
                 location = "+76+34", gravity = "northeast")
logo

anim <-  virus %>% mutate(Guns=ymd(Gun,truncated = 2L)) %>% 
  ggplot()+
  scale_y_continuous(name = "COVID-19 \n Vaka sayısı / Confirmed", limits = c(0, 1600))+
  scale_x_discrete(name = "Gün / Day", labels=Gun)+
  aes(x=Gun, y=Vaka,fill=Vaka)+
  annotation_raster(logo, ymin =1150, ymax = 1600, xmin = 5, xmax =10)+
  #ggplot(virus, aes(x=Gun, y=Vaka,fill=Vaka))+
   scale_fill_distiller(palette(gray(0:8 / 8)), direction = 1) +
  geom_text(aes(label = paste(Vaka,"")), vjust =-.2 ,col="#88398A")+
    shadow_mark()+  enter_grow()+  enter_fade()+  theme_minimal() +
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
  labs(title= "  Evinde otur Türkiye ! \n                Stay Home ! ")+
  annotation_raster(logo, ymin = ymin, ymax = ymax, xmin = xmin, xmax =xmax)+
  transition_states(Gun,wrap=FALSE)+
  ease_aes('cubic-in-out')
  animate(anim, height = 500, width =600)
  anim_save("virus in TR-23.gif") 
  

#1 Başka bir yol ama çalıştıramadım

  
  # Call back the plot
  library(magick)
  library(here)
  library(magrittr)
#  plot <- image_read(paste0(here("/"), "virus-chart.gif")) ### Güzel kod

  
   # Scale down the logo and give it a border and annotation
  # This is the cool part because you can do a lot to the 
  # image/logo before adding it
 
 
  
   # And bring in a logo

  logo_raw <- image_read("R-LadiesGlobal-sticker.png") 
  logo <- logo_raw %>%
    image_scale("100") %>% 
    image_background("white", flatten = TRUE) %>%
    image_border("white", "80x40") %>%
    image_annotate("produced by R Studio", color = "gray", size = 60, 
                   location = "+60+60", gravity = "northeast")
  logo
  # Stack them on top of each other
  final_plot <- image_append(image_scale(c(plot, logo), "500"), stack = TRUE)
  # And overwrite the plot without a logo
  image_write(final_plot, paste0(here("/"), last_plot()$labels$title, ".png"))

  
 library(gapminder) 
  gapminder %>%
    mutate(year = ymd(year, truncated = 2L)) %>%
    ggplot() +
    aes(year, lifeExp, size = pop, colour = country) +
    annotation_raster(tiger, ymin = ymin, ymax = ymax, xmin = xmin, xmax = 
                        xmax) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 12)) +
    labs(title = 'Year: {frame_time}', x = 'Year', y = 'life expectancy') +
  +9/
    3,201transition_time(year) +
    ease_aes('linear') +
    anim_save("animated_tiger_timeseries.gif")
  

  ggplot(datasaurus_dozen, aes(x=x, y=y))+
  geom_point()+
  theme_minimal() +
  transition_states(dataset, 3, 1) + 
  ease_aes('cubic-in-out')
  anim_save("anime.gif") 

  
  
  
  
  
  library(animation)
  desc = c("This is a super cool example of Gradient Descent")
  saveHTML({
    f1 = function(x, y) x^2 + 3 * sin(y)
    xx = grad.desc(f1, pi * c(-2, -2, 2, 2), c(-2 * pi, 2))
    
    xx$persp(col = "lightblue", theta = 30, phi = 30)
    
  },title = "Demo of Gradient Descent", description = desc,
  verbose = FALSE)
  
  
  library(animation)
  desc = c("This is a ")
  saveHTML({
  image_read(paste0(here("/"), "virus-chart.gif"))
  },title = "xxx", description = desc,verbose = FALSE)
  
  
  
  
  
  library(datasauRus)
  library(ggplot2)
  library(gganimate)
  # Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)

# Make a ggplot, but add frame=year: one image per year
ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

# Save at gif:
anim_save("271-ggplot2-animated-gif-chart-with-gganimate2.gif")

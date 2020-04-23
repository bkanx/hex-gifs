# Create hexagon .gif for R-Ladies EskisehR 
library(magick)
library(hexSticker)

# Animation

newlogo <- image_scale(image_read("R.png"), "x200")
intlogo <- image_scale(image_read("intR.png"), "x200")
oldlogo <- image_scale(image_read("RR.png"), "x200")
frames <- image_morph(c(oldlogo, intlogo, newlogo), frames = 16)
image_animate(frames)


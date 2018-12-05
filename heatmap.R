library(ggplot2)
load.image("Z:/CT-Mum/Visualization/product.jpg")

## if not already installed
install.packages("jpeg")  

library(jpeg)

?readJPEG()

img <- readJPEG("Z:/CT-Mum/Visualization/product.jpg", native = TRUE)

#this will display your image to test you read it correctly
if(exists("rasterImage")){
  plot(1:2, type='n')
  rasterImage(img,1,1,2,2)
}


d <- read.csv(file.choose())

ggplot(d, aes(x = X, y = Y))  + stat_density2d(geom = "tile", aes(fill = ..density..), contour = FALSE) + 
  scale_fill_gradient (low= "green", high="red") + geom_point()


##-----------------------------------------

d <- read.csv("Sample Heat Map Data.csv")

library(ggplot2)
library(jpeg)

img <- readJPEG("product.jpg")

ggplot(d, aes(x = X,y = Y))  + 
  #annotation_raster(img, xmin=50, xmax=200, ymin=50, ymax=150)+
  annotation_raster(img, xmin=0, xmax=834, ymin=0, ymax=476)+
  stat_density2d(geom = "raster", aes(alpha = ..density.., fill = ..density..) , contour = FALSE, show.legend = FALSE) +
  scale_fill_distiller(palette="RdYlBu", na.value="white") + 
  #scale_fill_distiller(palette = 'RdYlBu') +
  #scale_fill_gradient2(low="blue",high="red") +
  #geom_point(size=2)+
  scale_x_continuous(limits=c(0,dim(img)[2]),expand=c(0,0))+
  scale_y_continuous(limits=c(0,dim(img)[1]),expand=c(0,0))+
  coord_fixed() +
  theme(
    panel.background = element_rect(fill = "transparent") # bg of the panel
    , plot.background = element_rect(fill = "transparent", color = NA) # bg of the plot
    , panel.grid.major = element_blank() # get rid of major grid
    , panel.grid.minor = element_blank() # get rid of minor grid
    , legend.background = element_rect(fill = "transparent") # get rid of legend bg
    , legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    , axis.title.x=element_blank()
    , axis.text.x=element_blank()
    , axis.ticks.x=element_blank()
    , axis.title.y=element_blank()
    , axis.text.y=element_blank()
    , axis.ticks.y=element_blank()
  )


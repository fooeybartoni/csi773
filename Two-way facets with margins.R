# Adapted from ggplot2 Text Version 1
# Show including facet_grid margin
# 

# 0. Setup

library(ggplot2)

hwfacet <- theme_gray()+ theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
    colour=gray(.5), size=.2),
  panel.border=element_rect(fill=FALSE,colour=gray(.60)),
  panel.grid.minor.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.margin.x = unit(0.10,"cm"),
  panel.margin.y = unit(0.10,"cm"),
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(margin=margin(0,0,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)

# 1. Subsetting car data

# Old car data

head(mpg)
table(mpg$drv)

# Extract a subset of vehicles
#   exclude 5 cylinder cars and 
#   include just 4-wheel and front-wheel drive cars
#      (omits rear wheel drive cars

mpg2 <- subset(mpg, cyl != 5 & drv %in% c("4","f"))
table(mpg2[,c("cyl","drv")])

# 1.1 Make a two-way conditioned plot
#    using facet_ grid

windows(width=6,height=4)
ggplot(mpg2,aes(x=cty,y=hwy))+ 
  geom_point(shape=21,size=2,fill=rgb(0,.8,1),color=gray(0))+
  labs(x="City Miles Per Gallon",
     y="Highway Miles Per Gallon",
     title = paste("Popular Car Mileage: 1999 to 2008",
        "Conditioned By Cylinder Number and Drive Type",sep="\n"))+
  facet_grid(drv ~ cyl)+ hwfacet

# 1.2 Fit a line to each facet
#     Make the y=axis strip text label horizontal

windows(width=6,height=4)
ggplot(mpg2,aes(x=cty,y=hwy)) +
  geom_point(shape=21,size=2,fill=rgb(0,.8,1),color="black")+
  geom_smooth(method="lm", se = F, lwd=1, col="red")+ #Added line
  labs(x="City Miles Per Gallon",
     y="Highway Miles Per Gallon",
     title = paste("Popular Car Mileage: 1999 to 2008",
        "Conditioned By Cylinder Number and Drive Type",sep="\n"))+
  facet_grid(drv ~ cyl) +  hwfacet +
    theme(strip.text.y=element_text(angle=0))


1.3 Include margin panels the facet_grid argument

windows(width=6,height=4)
ggplot(mpg2,aes(x=cty,y=hwy)) +
  geom_point(shape=21,size=2,fill=rgb(0,.8,1),color=gray(0))+
  geom_smooth(method="lm", se = F, lwd=1, col="red")+ #Added line
  labs(x="City Miles Per Gallon",
     y="Highway Miles Per Gallon",
     title = paste("Popular Car Mileage: 1999 to 2008",
        "Conditioned By Cylinder Number and Drive Type",sep="\n"))+
  facet_grid(drv ~ cyl,margins=TRUE) +  hwfacet +
    theme(strip.text.y=element_text(angle=0))



# Horizontal stacked bar plot
# Without perceptual grouping

Due  Plot from 2.2  (1 pt)

#  0. Setup

library(micromapST)
library(ggplot2)
library(tidyr)


hw <- theme_gray() + theme(
  strip.background=element_rect(fill=rgb(.9,.95,1),
    colour=gray(.5), size=.2),
 
  panel.border=element_rect(fill=FALSE,colour=gray(.70)),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.margin.x = unit(0.10,"cm"),
  panel.margin.y = unit(0.05,"cm"),

  axis.ticks=element_blank(), 
  axis.text=element_text(colour="black"),

  axis.text.y=element_text(margin=margin(0,0,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)


# 1. Access and prepare the data

data(Educ8thData)
head(Educ8thData)

df <- Educ8thData[,-c(1,3)] #remove the first column
colnames(df)
colnames(df)=c("State","BelowBasic","AtBasic",
                "Proficient","Advanced") 
colnames(df)

# 1.1 Order the data/frame rows, select desired
#    variables and provide better names for facet labels

ord <- with(df,order(BelowBasic,AtBasic,
       Proficient,Advanced))
dfOrd <- df[ord,]

# 1.2 Fix factor levels for plotting order

# Full state names
tmp <- as.character(dfOrd$State)
dfOrd$State <- factor(tmp,rev(tmp))

# Postal codes as an option
# tmp <- as.character(rownames(dfOrd))
# dfOrd$PostalCode <- factor(tmp,rev(tmp)) 

# 1.3 A quick check

ggplot(dfOrd,aes(x=BelowBasic,y=State))+
geom_point(col="red",size=2.5) + 
labs(x="Percent Below Basic",y="",
 title="Math 8th Grade Proficiency 2011")


# 2. Produce a horizontally stacked bar plot for the
#    US. states and D.C 

# 2.1 Removed unwanted variables and produce a long form
#      data frame

colnames(dfOrd)
# Remove the average score
# Remove the postal codes
 
dfIndexed <- gather(dfOrd,
  key =Achievement,
  value="Percent",
  BelowBasic:Advanced, factor_key=TRUE) 

head(dfIndexed)
tail(dfIndexed)

# 2.2 Make the plot   

ggplot(dfIndexed,aes(x=State,y=Percent,fill=Achievement)) + 
geom_bar(stat="identity",alpha=I(1),color=gray(.2),width=.75,size=.4)+
 coord_flip() + hw + 
 theme(legend.position="top",
 axis.text.y=element_text(size = rel(.8)))+
 labs(fill="", y="Percent",
 title="Math 8th Grade Achievement 2011")


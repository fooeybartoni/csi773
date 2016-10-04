6# Row labeled plots: perceptual groups and multiple columns
# By Daniel B. Carr

# Due: Total points: 7

#  4. Would you rather the middle dot be the same
#     color as the rest of the dots.  
#     Yes or no. (1) for providing your opinion 
#
#  6. 6.3 make a superpose dotplot
#         version with three distinct
#         symbols and provide the
#         script (2)    
#     6.5 plot with a modified
#         dot size. (1)     
#
#  7. plot in 7.7   (1)
#  Provide your opinion how the data might best
#
#  What one plot or variant of plot would
#  you recommend for a serious discussion
#  of student achievement as indicated
#  by the four categories. (2)

#===================================================

# Data background 
#
# Source: National Center for Education Statistics (NCES)
# Program: National Assessment of Educational Progress (NAEP)
#   http://nces.ed.gov/nationsreportcard/naepdata/ 
# See the document about using the 
#    NAEP Explorer to obtain data.    
#    NAEP support production of statistical comparisons
# National Dashboard for 2015 
#   http://www.nationsreportcard.gov/dashboards/report_card.aspx
# 
# The mathematics and reading test of possible scores from 0 to 500
# NAEP definitions for state summer 
#   Performance:  The average score of state students
#   Achievement: 
#      The percent of state student scores
#      in the following categories defined
#      by cut points. 8th grade mathematics
#      cut points shown below.                  
#      Below Basic Basic, Proficient, Advanced
#         <262       262     299        333    
#=====================================================

# 0. Setup
#
# 1  Created variables from a data.frame.
#    make new data.frame and a dot plot
#
# 2. Reorder the data.frame rows and columns
#
# 3. Make the State factor levels   
#    match the data.frame row order
#    for states.
#
# 4. Add a factor specifying perceptual group numbers
#    and a factor specifying row numbers 
#    within each of the group
# 4.1 The perceptual grouping factor
# 4.2 The row within each group as a factor
#
# 5. Define colors for the five
#    row in each group 
# 5.1 Five different colors
# 5.2 Five color to highlight the
#     middle row
# 5.3 Produce a perceptually row-grouped
#     one variable dot plot
# 5.4 Comments
#
# 6. Making a three variable row-labeled dot plot
# 6.1 Use the tidyr's gather function to make an
#     indexed data.frame.
# 6.2 Clean up the factor level 
#     names for the three variables.
# 6.3 Produce a 3 variable plot with the same x-axis
#     scale and column widths 
# 6.4 facet_grid panel scale and space options
# 6.5 Another try using equal space
#     for x-axis panels
# 6.6 Comments
#
# 7.  Showing all four classes
# 7.1 Extract variables
# 7.2 Sort the states using more that one
#     variable break ties
# 7.3 Add a perceptual row-grouping factor
# 7.4 Add factor for rows within groups
# 7.5 Create an index dataed.frame
# 7.6 Clean up the level labels
# 7.7 Produce the plot

#=======================================

# 0. Setup

## Run

library(micromapST)
library(ggplot2)
library(tidyr)
library(dplyr)

rowTheme <- theme_gray()+ theme(
  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
    colour=gray(.5), size=.2),
  panel.border=element_rect(fill=FALSE,colour=gray(.80)),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.margin.x = unit(0.10,"cm"),
  panel.margin.y = unit(0.05,"cm"),
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(size=rel(.78),
    margin=margin(0,0,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)


hwTmp <- theme_gray()+ theme(
  strip.text.y = element_blank(),
  strip.background=element_rect(fill=rgb(.9,.95,1),
    colour=gray(.5), size=.2),
  panel.border=element_rect(fill=FALSE,colour=gray(.80)),
  panel.grid.minor.y = element_blank(),
#  panel.grid.major.y = element_blank(),
  panel.margin.x = unit(0.10,"cm"),
  panel.margin.y = unit(0.05,"cm"),
  axis.ticks=element_blank(),
  axis.text=element_text(colour="black"),
  axis.text.y=element_text(size=rel(.78),
    margin=margin(0,0,0,3)),
  axis.text.x=element_text(margin=margin(-1,0,3,0))
)

# Define a function for use later
colName <- function(x){
  ints= 1:length(x)
  names(ints)=x
  return(ints)
}

#============================================

# 1. Created variables from a data.frame.
#    make new data.frame and a dot plot

# The Educ8thData data.frames
# is in the micromapST packages
#
# The data comes from the National Center for
# Education Statistics (NCES) and its
# National Assessment of Education Performance
# (NAEP). 
 
head(Educ8thData)

ed <- Educ8thData  # use shorter name

# Create variables using mutate()

ed1 <- mutate(ed,
        Proficient_or_Above = PctProficient + PctAdvanced,
        Basic_or_Above = PctAtBasic + Proficient_or_Above)
head(ed1)

# Make a row-labeled dot plot

windows(width=6,height=9)
ggplot(ed1,aes(x=Proficient_or_Above,
   y=reorder(State,Proficient_or_Above)))+
  geom_point(shape=21,size=2.4,fill="cyan",color="black")+
  labs(x="Percent Proficient or Above",y="",
    title="NAEP 2011: 8th Grade Math")+ hwTmp +
  theme(axis.text.y =element_text(size=rel(.7),
    color='purple',face='bold'))

# 2. Reorder the data.frame rows and columns 

ord <-with(ed1,
      order(Basic_or_Above, Proficient_or_Above,
      PctAdvanced, decreasing=TRUE))
           
ed2 <- ed1[ord,c(1:6,9,8,7)]
head(ed2)

# 3. Make State factor levels   
#    match the data.frame row order
#    for states.
#  
#    Put Massachesetts at the top y-axis
#    by reversing the order

tmp <-as.character(ed2$State)
ed2$State <-factor(tmp,levels=rev(tmp))
levels(ed2$State)

# Massachusetts is the 51st case and will plot at the top
# of the y-axis as desired.

# 4. Add factor specifying perceptual group numbers
#    and a factor specifying row numbers 
#    within each of the groups.

# 4.1 The perceptual grouping factor
#
#   We could use 1:11 instead of 11:1 
#   below and change the facet_grid 
#   as.table=TRUE argument.  

classes <- paste("G",11:1,sep="")
classes
reps <- c(5,5,5,5,5,1,5,5,5,5,5)
ed2$Grp <- factor(rep(classes,reps),level=classes)
ed2$Grp

# 4.2 The row within each group as a factor

top <- rep(1:5,5)
top
subs <- c(top,1,top)
subs
labs <- c('1', '2',
          '3', '4', '5')
tmp <- labs[subs]
ed2$Row <- factor(tmp,levels=labs)
ed2$Row

# 5. Define colors for the five
#    row in each group 

# 5.1 Five different colors
# 5.2 Five color to highlight the
#       middle row 

# light red, orange, green,
# greenish blue,violet

rowColor<- rgb(
  red  = c(1.00, 1.00, 0.00, 0.10, 0.80),
  green= c(0.10, 0.50, 0.75, 0.65, 0.45),
  blue = c(0.10, 0.00, 0.00, 1.00, 1.00)
)

# 5.2 Five color to highlight the
#       middle row

rowColorMiddle = c("cyan","cyan","red",
  "cyan","cyan")

# 5.3 Produce a perceptually row-grouped
#     one variable dot plot

windows(width=6,height=9)

p <- 
   ggplot(ed2, aes(x=Basic_or_Above,
     y=State,fill=Row, group=Grp)) +
   labs(x="Percent Basic or Above", y="States",
     title="NAEP 2011: 8th Grade Math")+
   geom_point(shape=21,size=I(2.5))+
   scale_fill_manual(values=rowColorMiddle)+
   guides(fill=FALSE)+ 
   facet_grid(Grp ~ ., scale="free_y", space="free" )+
   rowTheme
p

# 5.4 Comments:

# Perceptual groups of 5 are a bit large. General
# guidance recommends limited a perceptual group
# to four or fewer items
#
# Pairing each label on the left of the plot with
# a dot on the other side of the plot can be
# attention demanding if not error prone.
# Plotting the middle data point in a group
# with a different color creates 2-1-2 pattern.  
# If we note that MN (Minnesotta) is
# the middle state in top group of five
# we can easily locate it as the corresponding middle red dot
# on the other side of the plot and be less likely
# to miss identify the 1st, 2nd, 4th or 5th dots.  

# The plot is subject to further refinements
#
# A possible next step could be to plot the
# middle state in each group of 5 red.  
# This would reduce the need to explain the
# red dot with a legend and might facilitate
# pairing in terms of speed and accuracy. 
#
# Sometimes the row names on adjacent rows
# are close touching. The would happen if
# two labels lines were used in the titel.
# This makes the names harder to read
# than necessary. 
# 
# The gap between lines can be increase in several
# ways:  
#        Make the plot window taller
#        Reduce the height of the labeling
#          area at the top and bottom
#        Decrease the font size,
#        Reduce the vertical margin between panels
#        
# In the past I advocated for following the graphic convention
# by left aligning the row names. 

# 6. Making a three variable row-labeled dot plot

# We will use ggplot columns of facets with a column
# facet each variable. For this we need to make and
# make a index data frame.

    
# 6.1 Use the tidyr's gather function to make an
#     indexed data.frame.
#
#     This will 
#     1) stack the variables values to be plotted in
#        value column. We label this column   
#     2) create key factor to indicate 
#        which variable is which in the stacked column.
#        We label this column
#     3) Replicate other index factors appropriately 

  

# Select the variables we need and procede. 

colName(colnames(ed2)) # a convenience with chosing to
                       # specify number rather than names

edIndex1  <- gather(ed2[,c(2,7:11)],
   key = Achievement,value=Percent,
   Basic_or_Above:PctAdvanced,factor_key=TRUE)
   # Select columns to stack

head(edIndex1)
tail(edIndex1)

# 6.2 Clean up the factor level 
#     names for the three variables.

newNames = c("Basic or Above",
  "Proficient or Above", "Advanced")
levels(edIndex1$Achievement) <- newNames 


# 6.3 Produce a 3 variable plot with the same x-axis
#     scale and column widths. 

windows(width=7, height=9)

p3 <-
(ggplot(edIndex1,aes(x=Percent,y=State,fill=Row,group=Grp))
  + labs(title= "NAEP: 2011 Math 8th Grade Achievement",
      x="Percent", y="States")
  + geom_point(shape=21,size=I(2.2))
  + scale_fill_manual(values=rowColor, guide=FALSE)
  + facet_grid(Grp ~ Achievement, scale="free_y", space="free")
  + rowTheme
)

p3

# 6.4 facet_grid panel scale and space options

# Above all columns had the same scale and the
# same width.  Much the columns were empty. 
#
# Below we let facet_grid zoom in on the data
# range for each panel and adjust the panel widths
# base on the relative range for each panel.

windows(width=8, height=9)
p6.4 <-
(ggplot(edIndex1,aes(x=Percent,y=State,fill=Row,group=Grp))
  + labs(title= "NAEP Scores: 2011 Math 8th Grade Achievement",
      x="Percent", y="States")
  + geom_point(shape=21,size=2.5)
  + scale_fill_manual(values=rowColor, guide=FALSE)
  + facet_grid(Grp ~ Achievement, scale="free", space="free")
  + rowTheme
)
p6.4

# 6.5 Another try using equal space for x-axis panels

windows(width=7, height=9)

p6.5 <-
(ggplot(edIndex1,aes(x=Percent,y=State,fill=Row,group=Grp))
  + labs(title= "NAEP Scores: 2011 Math 8th Grade Achievement",
      x="Percent", y="States")
  + geom_point(shape=21,size=I(2.8))
  + scale_fill_manual(values=rowColor, guide=FALSE)
  + facet_grid(Grp ~ Achievement, scale="free",space="free_y")
  + rowTheme
)
p6.5

# 6.6 Comments
#
# Section 6 reduced the four achievement classes to three. 
#
# Using the Basic and Above column, readers should be
# able to infer the number in the Below Basic category
# by subtraction   If 48 percent in DC students are
# at the basic and above levle then 52 percent
# perform at th below based level.
#
# Reader from neighboring Virginia (see the 4th group down) can
# note that 78 percent of students assessed perform at levels basic
# or above, 40 percent at proficient level or above and 11 percent
# perform at an advanced level.
#
# Some little appearance details such as dot clipping
# are candidates for improvement

# 7.  Showing all four classes

# 7.1  Extract variables

names(ed1)
dfAchieve <- ed1[,c(1,4:7)]
colnames(dfAchieve) <-c("PostalCode",
  'BelowBasic','Basic','Proficient','Advanced')

head(dfAchieve)


# 7.2 Sort the states using more that one
#     variable break ties

# The sort order might vary depending on the audience.
# More than one plot could be produced to
# show alternative views. 

# Here we focus percent proficient can break ties by the
# percent advanced and then by the percent basic.   

ord <- with(dfAchieve, order(Proficient, Advanced,Basic,
  decreasing=FALSE)) # breaks ties
dfAchieve <- dfAchieve[rev(ord),]  # save a step?
dfAchieve

# 7.3 Add a perceptual row-grouping factor

classes <- paste("G",11:1,sep="")
reps <- rep(c(5,5,5,5,5,1,5,5,5,5,5))
dfAchieve$Grp <- factor(rep(classes,reps),level=classes)

# Put states with best scores last in
# the factor so they will plot on top.

ab <- as.character(dfAchieve$PostalCode)
dfAchieve$PostalCode <- factor(ab,rev(ab))
head(dfAchieve)

# 7.4 Add factor for rows within groups

top <- rep(1:5,5)
subs <- c(top,1,top)
labs <- c('1', '2',
          '3', '4', '5')
tmp <- labs[subs]
dfAchieve$Row <- factor(tmp,levels=labs)

# 7.5 Create an index data.frame

dfAchLong <- gather(dfAchieve,key=Achievement,
   value=Percent,BelowBasic,Basic,Proficient,Advanced,
   factor_key=TRUE)

head(dfAchLong)

# 7.6 Clean up the level labels

levels(dfAchLong$Achievement)

levels(dfAchLong$Achievement) <-
  c('Below Basic','Basic','Proficient','Advanced')

#  7.7 Produce the plot

p7.7 <-
(ggplot(dfAchLong,aes(x=Percent,y=PostalCode,fill=Row,group=Grp))
  + labs(title= "NAEP Scores: 2011 Math 8th Grade Achievement",
      x="Percent", y="States")
  + geom_point(shape=21,size=I(2.3))
  + scale_fill_manual(values=rowColor, guide=FALSE)
  + facet_grid(Grp ~ Achievement, scale="free", space="free_y")
  + rowTheme
)
p7.7

# Note the high negative correlation between below basic and
# proficient.



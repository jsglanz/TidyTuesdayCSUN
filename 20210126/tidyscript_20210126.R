#Load data
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

#Load libraries
library(tidyr)
library(dplyr)
library(plyr)
library(readr)
library(plotrix)
library(ggplot2)
library(forcats)

plastics$year<-as.character(plastics$year)#change year from numeric to character to be able to tidy by this group

plastics<-plastics[,-4]#remove empty column

#Filter by top polluters
pa<- plastics %>% filter(parent_company==c("The Coca-Cola Company","PepsiCo","Nestlé"))
View(pa)

#I want to first sum by year and parent company for each plastic type
hdpe <- aggregate(pa$hdpe,
                  by = list(pa$year,
                            pa$parent_company),
                  FUN = 'sum', na.rm=TRUE)
colnames(hdpe) <- c("year","parent_company","hdpe")
pet <- aggregate(pa$pet,
                 by = list(pa$year,
                           pa$parent_company),
                 FUN = 'sum', na.rm=TRUE)
ldpe <- aggregate(pa$ldpe,
                  by = list(pa$year,
                            pa$parent_company),
                  FUN = 'sum', na.rm=TRUE)
pp <- aggregate(pa$pp,
                by = list(pa$year,
                          pa$parent_company),
                FUN = 'sum', na.rm=TRUE)
ps <- aggregate(pa$ps,
                by = list(pa$year,
                          pa$parent_company),
                FUN = 'sum', na.rm=TRUE)
o <- aggregate(pa$o,
               by = list(pa$year,
                         pa$parent_company),
               FUN = 'sum', na.rm=TRUE)
pvc <- aggregate(pa$pvc,
                 by = list(pa$year,
                           pa$parent_company),
                 FUN = 'sum', na.rm=TRUE)
gt <- aggregate(pa$grand_total,
                 by = list(pa$year,
                           pa$parent_company),
                 FUN = 'sum', na.rm=TRUE)

p<-cbind(hdpe, pet$x, ldpe$x,pp$x, ps$x, o$x, pvc$x,gt$x)#Then bind each of the sums into one dataframe
colnames(p) <- c("year","parent_company","hdpe","pet","ldpe","pp","ps","o","pvc","gt")#rename columns in new dataframe
View(p)


#One way to make plastic type into tidy format (aka wide to long)
p_long<-p %>% pivot_longer(cols=hdpe:pvc,names_to="plastic.type",values_to="count",values_drop_na = TRUE) #change so plastic types are in tidy format; cols= columns to convert to long format, names_to= name of new categorical column, values_to= names column with values in cells of columns you're tidying and values_drop_na= drops any NAs if there are any
View(p_long)


#I want to add classification for each plastic type's recyclability (this is general since it depends on the local facilities in each country):
p_long$recyclable<-p_long$plastic.type %>% fct_collapse(Recyclable= c("hdpe","pet","ldpe","pp"), Nonrecyclable=c("ps","o","pvc"))

#add column for proportion of total plastics in each plastic type
p_long$prop<-(p_long$count/p_long$gt)

#Make donut charts for each company
###Nestle
np<-p_long %>% filter(parent_company=="Nestlé")

#Compute the cumulative percentages (top of each rectangle)
np$ymax = cumsum(np$prop)

#Compute the bottom of each rectangle
np$ymin = c(0, head(np$ymax, n=-1))

#Compute label position
np$labelPosition <- (np$ymax + np$ymin) / 2
#
#Compute a good label
np$label <- paste0(np$plastic.type, "\n  ", np$count)#"\n  " put space between the plastic type and its count

# Make the plot
j<-ggplot(np, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=recyclable)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=2) +
  scale_color_brewer(palette=2)+
  coord_polar(theta="y") + #turns bar plot into a ring shape
  xlim(c(0, 4)) +
  theme_void() #gets rid of gray background in plot

#Add company logo to center of donut plot
logo <- image_read("desktop/nestle.png")
j
grid::grid.raster(logo, x = 0.4, y = 0.5, just = c('center','center'), width = unit(1, 'inches'))#x and y are values between 0 and 1 with 0 being the far bottom or left and 1 being the far top or right of the plot, width controls the size of the logo

###PepsiCo
pep<-p_long %>% filter(parent_company=="PepsiCo")

#Compute the cumulative percentages (top of each rectangle)
pep$ymax = cumsum(pep$prop)

#Compute the bottom of each rectangle
pep$ymin = c(0, head(pep$ymax, n=-1))

#Compute label position
pep$labelPosition <- (pep$ymax + pep$ymin) / 2
#
#Compute a good label
pep$label <- paste0(pep$plastic.type, "\n  ", pep$count)#"\n  " put space between the plastic type and its count

#Make the plot
j<-ggplot(pep, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=recyclable)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=2) +
  scale_color_brewer(palette=2)+
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_void() 

#Add company logo to center of plot
logo <- image_read("desktop/PEPSICO.png")
j
grid::grid.raster(logo, x = 0.4, y = 0.5, just = c('center','center'), width = unit(2, 'inches'))

###Coca-cola
cc<-p_long %>% filter(parent_company=="The Coca-Cola Company", year=="2019")

#Compute the cumulative percentages (top of each rectangle)
cc$ymax = cumsum(cc$prop)

#Compute the bottom of each rectangle
cc$ymin = c(0, head(cc$ymax, n=-1))

#Compute label position
cc$labelPosition <- (cc$ymax + cc$ymin) / 2

#Compute a good label
cc$label <- paste0(cc$plastic.type, "\n  ", cc$count)#"\n  " put space between the plastic type and its count

# Make the plot
j<-ggplot(cc, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=recyclable)) +
  geom_rect() +
  geom_label( x=4, aes(y=labelPosition, label=label), size=4) +
  scale_fill_brewer(palette=2) +
  scale_color_brewer(palette=2)+
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme_void() 

#Add company logo to center of plot
logo <- image_read("desktop/cocacola.png")
j
grid::grid.raster(logo, x = 0.4, y = 0.5, just = c('center','center'), width = unit(2.5, 'inches'))

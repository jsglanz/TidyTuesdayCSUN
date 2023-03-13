#Load data
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")
View(state_pop)

#unique allows you to see all the different levels within the dataframe column you specify
parktype<-unique(park_visits$unit_type)
parktype

library(dplyr)
library(forcats)

#interested in parks near bodies of water so subset those parks using the following
###Order is important here, do this first before collapsing all river parks into one river name
parksubs <- select(filter(park_visits, unit_type %in% c("National Seashore","National River", "Scenic and Recreational River", "National Scenic Riverway", "Wild and Scenic River", "National Recreation River", "National River and Recreation Area","National Scenic River","National Lakeshore")),c(1,7:8,10,11:12))
View(parksubs)

parksubs$year<-as.numeric(parksubs$year)#convert year to numeric to see change in visitors over time

parksubs<-parksubs[-c(27:52),]#remove Totals to only include annual numbers of visitors

#many different types of parks with rivers, so to collapse them into one that encompasses all of them do the following:
parksubs$park_type<-parksubs$unit_type %>% fct_collapse(Seashore= "National Seashore", Lakeshore="National Lakeshore", River= c("National River", "Scenic and Recreational River", "National Scenic Riverway", "Wild and Scenic River", "National Recreation River", "National River and Recreation Area","National Scenic River"))

#Which states and years are included when we subset by parks with water?
state<-unique(parksubs$state)
state

year<-unique(parksub$year)
year

#filter states and years in state_pop dataset to coincide with parksub data
popsub<-state_pop%>%
  filter(state %in% c("MD","TN","WV","NC","GA","NY","MN","SD","VA","TX","MI","CA","FL","MO","NE","WI","PA","MA","IN","AR"))
View(popsub)

popsub<-popsub%>%
  filter(year%in% c("1967","1988","1993",  "1955",  "1976",  "2011",  "2004",  "1984",  "1987",  "1985",  "1966",  "1973", "1970",  "1980",  "1964", "2016",  "2015" , "2014" , "2013" , "2012" , "2010",  "2009",  "2008",  "2007",  "2006",  "2005" , "2003" , "2002",  "2001" , "2000",  "1999",  "1998",  "1997",  "1996",  "1995" , "1994",  "1992",  "1991",  "1990",  "1989",  "1986",  "1983",  "1982",  "1981" , "1979" , "1978" , "1977" ,"1975" , "1974",  "1972",  "1971" , "1969",  "1968" , "1965",  "1963" , "1962" , "1961",  "1960" , "1959" , "1958",  "1957" , "1956" ))

#Merge parksubs and popsub so total state population from popsub merges with the correct state-year row in parksubs
#aka merge data frames by multiple columns
jointsub<-merge(parksubs, popsub, by =c('year','state'))
View(jointsub)

#Calculate proportion of population that visited parks
jointsub$prop<-(jointsub$visitors/jointsub$pop)

#Plot proportion of state population visiting national parks by type, region, and across years
library(ggpubr)
par(oma=c(5,5,5,5)) # all sides have 3 lines of space
par(mar=c(5,4,4,2) + 0.1)
p<-ggscatter(jointsub, x = "year", y = "prop", 
         shape= "region",color="park_type", 
          xlab = "Year", ylab = "Proportion of State Population Visiting National Parks",palette=c("#E69F00","#009E73","#56B4E9"), repel=TRUE)+
  labs(shape= "Region", color="Park Type")+stat_cor(aes(color=park_type),label.x=1955)

#Save enlarged version to capture entire plot
ggsave("p.png", p, width = 10, height = 10, units = "in")

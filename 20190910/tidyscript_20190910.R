#Load data
tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")

safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

View(tx_injuries)
View(safer_parks)

#Load libraries
library(magrittr)
library(tidyverse)
library(scatterpie)
library(maps)
library(mapdata)

#obtain proportion of injuries per industry sector in each state
library(dplyr)
sumby2<-safer_parks %>% group_by(acc_state, industry_sector) %>% summarise(sumy = sum(num_injured)) #sum number of injuries by sector in each state
View(sumby2)
colnames(sumby2)[3]<-'numinj'#renames sum column

sumbyst<-safer_parks %>% group_by(acc_state) %>% summarise(sumy = sum(num_injured)) #sum number of injuries across all sectors in each state
View(sumbyst)
colnames(sumbyst)[2]<-'totinj'#renames sum column

jointsum<-merge(sumby2, sumbyst, by ='acc_state') #merges sumby2 and sumbyst by state
View(jointsum)

jointsum$prop<-(jointsum$numinj/jointsum$totinj) #calculate proportion of total injuries occurred in each industry sector of each state

#Which states are present in dataset?
state<-unique(sumbyst$acc_state)
state

#create data frame of states' lat-long data
region <- data.frame(
  lon = c( -91.831833, -111.093735,-119.417931,-105.782066,-73.087746,-81.515755,-82.900078,-93.097702,-114.742043,-89.398529,-98.4842,-84.2700,-71.3824,-76.6413,-69.4455,-85.6024,-94.6859,-91.8318,-110.3626,-79.0193,-99.9018,-74.4057,-105.8701,-116.4194,-74.0060,-82.9071,-97.0929,-120.5542,-77.1945,-81.1637,-86.5804,-99.9018,-111.0937,-78.6569,-120.7401,-88.7879,-80.4549,-107.2903),
  lat = c( 35.201050, 34.048927,36.778259,39.550053,41.603222,27.664827,32.165623,41.878002,44.068203,40.633125, 39.0119,37.8393,42.4072,39.0458,45.2538,44.3148,46.7296,37.9643,46.8797,35.7596,41.4925,40.0583,34.5199,38.8026,40.7128,40.4173,35.0078,43.8041,41.2033,33.8361,35.5175,31.9686,39.3210,37.4316,47.7511,43.7844,38.5976,43.0760),
  acc_state = c( "AR", "AZ", "CA", "CO", "CT","FL","GA","IA","ID","IL","KS","KY","MA","MD","ME","MI","MN","MO","MT","NC","NE","NJ","NM","NV","NY","OH","OK","OR","PA","SC","TN","TX","UT","VA","WA","WI","WV","WY"),
  stringsAsFactors = FALSE) #create dataframe of that lat-long data

#bind lat-long data with injury data
datreg <- dplyr::right_join(jointsum, region) %>% 
  as.data.frame() #binds lat-long data with injury data
View(datreg)
datreg<-datreg[,-3] #remove the sums rows from earlier since we only need the proportion data

dat_wide<- spread(datreg,industry_sector,prop) #converts dataframe from long to wide, important for mapping pie charts
View(dat_wide)

dat_wide[is.na(dat_wide)] <- 0 #turns all NA's into 0's
View(dat_wide)
dat_wide<-dat_wide[-1,]#remove Alaska for just contiguous states

#make map
us <- map_data("state")


p1 <- ggplot(us, aes(long, lat)) +
  geom_map(map = us, aes(map_id = region), fill = NA, color = "black") +
  coord_quickmap()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                         panel.background = element_blank(), axis.line = element_line(colour = "black"))
 

print(p1)

#make map with pie charts of proportion of recreational injuries by sector in each state
p2 <- p1 +
  # The original value is r = sqrt(total)/2000, but the pie charts are smaller,
  # the lower number of the denominator, the bigger the pie chart
  # the numerator in r makes the radius of the pie chart relative to the total number of recreational injuries in each state
  geom_scatterpie(aes(x = lon, y = lat, group = acc_state, r = sqrt(totinj)/20), 
                  data = dat_wide,
                  cols = c("amusement ride", "recreation", "unknown","water park"),
                  color = "#252728",
                  size = 0.25) +
  geom_text(data = region, 
            aes(lon, lat, label = acc_state, fontface = 2),
            nudge_x = 0.05,
            size = 2,
            color = "#FF3333") +
  scale_fill_manual(
    breaks = c("amusement ride", "recreation", "unknown","water park"),
    labels = c("amusement ride", "recreation", "unknown","water park"),
    values = c("amusement ride" = "#003366",
               "recreation" = "#CCCC66",
               "unknown" = "#CC3366", "water park"="green")) +
  labs(title = "Distribution of Recreation Injuries",
       caption = "Data Source: Tidy Tuesday",
       fill = NULL) +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = c(0.96, -0.02),
        legend.justification = c(1, 0),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.caption = element_text(face = "italic"))

print(p2)

#Save enlarged version to capture entire plot
ggsave("p2.png", p2, width = 10, height = 10, units = "in")

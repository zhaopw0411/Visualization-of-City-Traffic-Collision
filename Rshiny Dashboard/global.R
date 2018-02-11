####################### global.R #####################
library(dplyr)

# Data cleaning to get rid of records with no geo info
# Leaflet bindings are a bit slow; for now we'll just sample to compensate (10000 samples)
# Original data NYPD_Motor_Vehicle_Collisions.csv is 119.5 MB, which is too large to be included in this repo

############### Code ##############
# vc <- read.csv("data/NYPD_Motor_Vehicle_Collisions.csv") # 618,358 observations
# vc <- subset(vc,!(is.na(vc['ZIP.CODE']))) # 470,687 observations
# set.seed(100)
# vc <- vc[sample.int(nrow(vc), 10000),]
# write.csv(vc, file = 'data/NYPD_Motor_Vehicle_Collisions (10000 obs).csv')
###################################

# Import truncate data (vc is short for vehicle collision)

vc <- result_ny #read.csv('data/NYPD_Motor_Vehicle_Collisions (10000 obs).csv')
vc <- subset(vc,!(is.na(vc['zip_code'])))

#count number of accidents at the same location(latitude and longitude)
cleantable <- vc %>%
  group_by(longitude,latitude) %>%
  mutate(count = n(),totalinjury = sum(number_of_persons_injured),
         totaldeath = sum(number_of_persons_killed))
cleantable <- cleantable[!duplicated(cleantable$location), ]

cleantable <- cleantable %>%
  select(
    Borough = borough,
    Zipcode = zip_code,
    Street1 = on_street_name,
    Street2 = cross_street_name,
    Accidents = count,
    Injuries = totalinjury,
    Deaths = totaldeath,
    Lat = latitude,
    Long = longitude,
    Daytime = daytime)

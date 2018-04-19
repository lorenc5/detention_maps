########################################### 
# Loren Collingwood                       #
# with Jason Morin                        #
# Started: 4/17/2018                      #
# 112 Congress with 111 Lines             #
# 112 = 2011-2012 -- based on 2000 Census #
# Goal is to eventually merge with Bills  #
# data, do a spatial points overlay, then #
# merge with 113/114 Congress data        #
###########################################

############
# Packages #
############

library(lubridate)
library(USAboundaries); library(USAboundariesData)
library(sf)
library(rgdal)
library(mapview)
library(htmlwidgets)
library(sp)
library(lubridate)
library(tigris)
library(leaflet)
library(tidyverse)
library(plyr)
library(dplyr)

#################
# Set Directory #
#################

setwd("~/Dropbox/sanctuary_paper2/r_code/data/prisons_data"); list.files()

##########################################
# Read in ICE Location Points Shape File #
##########################################

ice_sp <- rgdal::readOGR(dsn="ice_facility_points", stringsAsFactors = F)

###########
# Recodes #
###########

# Best Known Contract Initiation Date Setting #
ice_sp$contract_init_date <- ymd(ice_sp$B_K_C_I); head(ice_sp$contract_init_date)
sort(ice_sp$contract_init_date)

# Contract Expiration Date Setting #
ice_sp$contract_expire_date <- ymd(ice_sp$B_K_C_E); head(ice_sp$contract_expire_date)
sort(ice_sp$contract_expire_date)

# Contract Initiated during 111th Congres #
ice_sp$contract_init_111 <- 0
ice_sp$contract_init_111[ice_sp$contract_init_date >= as.Date("2009-01-03") &
                         ice_sp$contract_init_date < as.Date("2011-01-03")] <- 1 
table(ice_sp$contract_init_111)

# Contract Initiated Prior to 111, stayed through 114th
ice_sp$contract_init_prior_111 <- 0
ice_sp$contract_init_prior_111[ice_sp$contract_init_date < as.Date("2009-01-03") &
                               is.na(ice_sp$contract_expire_date)] <- 1
table(ice_sp$contract_init_prior_111)

# Contract initiated during 112th Congress #
ice_sp$contract_init_112 <- 0
ice_sp$contract_init_112[ice_sp$contract_init_date >= as.Date("2011-01-03") &
                         ice_sp$contract_init_date < as.Date("2013-01-03")] <- 1 
table(ice_sp$contract_init_112)

# Contract Initiated Prior to 112, then not expired #
ice_sp$contract_init_prior_112 <- 0
ice_sp$contract_init_prior_112[ice_sp$contract_init_date < as.Date("2011-01-03") &
                               is.na(ice_sp$contract_expire_date)] <- 1
table(ice_sp$contract_init_prior_112)

# Contract Initiated Prior to 112, or during it, then not expired -- can check as asterisk test
ice_sp$contract_init_prior_during_112 <- 0
ice_sp$contract_init_prior_during_112[ice_sp$contract_init_date < as.Date("2013-01-03") &
                                      is.na(ice_sp$contract_expire_date)] <- 1
table(ice_sp$contract_init_prior_during_112)

# Contract initiate during 113th Congress (new districts)
# Put it here just for flagging/subsetting, etc.
ice_sp$contract_init_113 <- 0
ice_sp$contract_init_113[ice_sp$contract_init_date >= as.Date("2013-01-03") &
                         ice_sp$contract_init_date < as.Date("2015-01-03")] <- 1 
table(ice_sp$contract_init_113)

# Contract Expires 113th Congress #
ice_sp$contract_expire_113 <- 0
ice_sp$contract_expire_113[ice_sp$contract_expire_date >= as.Date("2013-01-03") &
                           ice_sp$contract_expire_date < as.Date("2015-01-06")] <- 1 
table(ice_sp$contract_expire_113)

###################################
# Contract Expires 114th Congress #
###################################

ice_sp$contract_expire_114 <- 0
ice_sp$contract_expire_114[ice_sp$contract_expire_date >= as.Date("2015-01-06") &
                           ice_sp$contract_expire_date < as.Date("2017-01-03")] <- 1 
table(ice_sp$contract_expire_114)

############################################
# Create Private Prison File for Later Use #
############################################

ice_sp_prior_112 <- ice_sp[ice_sp$contract_init_prior_112 ==1 &
                           ice_sp$prvt_w_==1, ]; dim(ice_sp_prior_112@data)
ice_sp_during_112 <- ice_sp[ice_sp$contract_init_112 ==1 &
                            ice_sp$prvt_w_==1, ]; dim(ice_sp_during_112@data)
ice_sp_prior_during_112 <- ice_sp[ice_sp$contract_init_prior_during_112 == 1 &
                                  ice_sp$prvt_w_==1, ]; dim(ice_sp_prior_during_112@data)
###########################
# Get Coordinates for Map #
###########################

lng <- coordinates(ice_sp)[,1] # long
lat <- coordinates(ice_sp)[,2] # long
lng_2 <- coordinates(ice_sp_prior_112)[,1] # Prior 112 Lng
lat_2 <- coordinates(ice_sp_prior_112)[,2] # Prior 112 Lat
lng_3 <- coordinates(ice_sp_during_112)[,1] # During 112 long
lat_3 <- coordinates(ice_sp_during_112)[,2] # During 112 Lat
lng_4 <- coordinates(ice_sp_prior_during_112)[,1] # Prior/During 112 long
lat_4 <- coordinates(ice_sp_prior_during_112)[,2] # Prior/During 112 Lat

####################################
# Bring in Congressional 2010 File #
####################################

######################
# 2010 CD Shape File #
######################
# Downloaded from Census/Tigris Website #
us_congressional_2010 <- rgdal::readOGR(dsn="gz_2010_us_500_11_20m",
                                        stringsAsFactors=F)

m <- leaflet(us_congressional_2010) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -98.35, lat = 39.50, zoom = 04) %>%
  addPolygons(fillColor = "white",
              color = "black",
              weight = 0.5) %>%
  addCircleMarkers(lng=lng_2, lat = lat_2, radius = 1, 
                   label = as.character(ice_sp_prior_112$Name),
                   labelOptions = labelOptions(noHide = T, 
                   textOnly = TRUE, textsize = "4px")) %>%
  addCircleMarkers(lng=lng_3, lat=lat_3, color="purple", radius=3, 
                   label= as.character(ice_sp_during_112$Name) ,
                   labelOptions = labelOptions(noHide = T, 
                   textOnly = TRUE, textsize = "5px")) 
m

#################################
# Spatial Polygon/Point Overlay #
#################################

# Private Prison owned/managed, Prior/During 112th #
cong_over <- sp::over(x=ice_sp_prior_during_112, y= us_congressional_2010) # Any ICE Facility
cong_over <- cong_over[, c("GEO_ID","STATE", "CD")] # take just variables I want 

# Count Number of Facilities in District #
district_split <- split(cong_over, cong_over$GEO_ID)
prison_dist_count <- plyr::ldply(district_split, function(x) nrow(x))
colnames(prison_dist_count) <- c("GEO_ID", "ice_count_priv")

##############################################
# Merge with Congressional district Map Info #
##############################################

congress_112_m <- merge(us_congressional_2010, prison_dist_count, 
                       by.x="GEO_ID", by.y="GEO_ID", all.x=T); dim(congress_112_m) 

# Replace NAs in Count with 0
congress_112_m$ice_count_priv[is.na(congress_112_m$ice_count_priv)] <- 0

# To Do: #
# Merge with 111/112 data, Jason's Building, it's almost there #
# Need to create state/district merger #

# Then merge with 113 Data: IV is changes in private facilities in district
# Subset to only MCs who remain in office
# DV: Count of Sponsor/Co-sponsorship, probably needs to be scaled
# Model Diff-Diff where change in Private Facility Status is 'treatment'



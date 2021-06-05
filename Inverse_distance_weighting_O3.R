#load packages
library(rgdal)
library(raster)
library(dplyr)
library(gstat)
library(sf)
#library(spatstat)
#set workding directory
setwd("XXXX/XXXX")

#load ozone 3 year average data (2018 - 2020)
oz_3yr_ave <- read.csv("./XXXX.csv")

#Create subset data frames for each month
#calculate average monthly 3 yr average ozone at each monitoring site
oz_month <- oz_3yr_ave %>%
  group_by(Unique_ID, Month) %>%
  summarise(month_ave_daily_oz = mean(oz_3yr_ave), Site.ID = mean(Site.ID))
#Add latitude and longitude columns back in
#group original data by unique ID
oz_3yr_ave_site_coord <- oz_3yr_ave %>% 
  group_by(Site.ID) %>%
  summarise(lat = mean(SITE_LATITUDE), long = mean(SITE_LONGITUDE))
#Merge with data containing lat and long for each monitoring site
oz_month <- merge(oz_month, oz_3yr_ave_site_coord, by = "Site.ID")

#Create data frame for each month
oz_may <- oz_month %>% filter(Month == 5)
oz_june <- oz_month %>% filter(Month == 6)
oz_july <- oz_month %>% filter(Month == 7)
oz_aug <- oz_month %>% filter(Month == 8)
oz_sept <- oz_month %>% filter(Month == 9)
oz_oct <- oz_month %>% filter(Month == 10)

#Create list of data frames
oz_list <- list(oz_may, oz_june, oz_july, oz_aug, oz_sept, oz_oct)

#Import counties
CO_counties <- st_read("XXXX.shp")
#convert to sp object
CO_counties_sp <- as(CO_counties, "Spatial")

#project ozone points
#convert to sp format and project
for(i in 1:length(oz_list)) {
  a <- oz_list[[i]]
  coordinates(a)=~long+lat
  proj4string(a) <- CRS("+proj=longlat +datum=NAD83")
  a <- spTransform(a, CRS("+init=epsg:26913"))
  df.name <- paste("month", i)
  assign(df.name, a)
} 

# Run the IDW
#create empty grid at same extent as CO to take IDW results
grd <- as.data.frame(spsample(CO_counties_sp, "regular", n=50000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Make grid correct projection
proj4string(grd) <- proj4string(`month 1`)

# create list of month sp objects to be used in idw
oz_idw_list <- list(`month 1`, `month 2`, `month 3`, `month 4`, `month 5`, `month 6`)

#create list of empty grids for results
grd <- as.data.frame(spsample(CO_counties_sp, "regular", n=75000))
names(grd) <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE
fullgrid(grd)    <- TRUE
proj4string(grd) <- proj4string(`month 1`)
grd_list <- do.call("list", replicate(6, grd, simplify = FALSE))

#Run the IDW and convert results to rasters
# create empty list for raster results
idw_results <- list()
# run IDW
for(i in 1:length(oz_idw_list)) {
  #a <- oz_idw_list[[i]]
  oz_idw <- gstat::idw(month_ave_daily_oz~1, oz_idw_list[[i]], newdata=grd_list[[i]], idp = 2.0)
  oz_idw_rast <- raster(oz_idw)
  oz_idw_rast_clip <- mask(oz_idw_rast, CO_counties_sp)
  idw_results[[i]] <- oz_idw_rast_clip
}

#save each IDW raster as a tif
month <- c("May", "June", "July", "Aug", "Sept", "Oct")
for(i in 1:length(idw_results)) {
  rast.name <- paste0(month[i], '.tif')
  writeRaster(idw_results[[i]], filename = rast.name)
}

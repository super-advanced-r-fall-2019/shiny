library(leaflet)
library(dplyr)
library(rgdal)
library(lubridate)

# ideas for exercises
# plot different subsets of data
  # decade planted - bonus points if you can figure out why this one doesnt work
  # genus
# change color
  # private
  # public

privLeafIcon <- makeIcon(
  iconUrl = "_ionicons_svg_md-leaf.svg",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94
)

publicLeafIcon <- makeIcon(
  iconUrl = "_ionicons_svg_ios-leaf.svg",
  iconWidth = 38, iconHeight = 95,
  iconAnchorX = 22, iconAnchorY = 94
)

LeafIcons <- iconList(private = privLeafIcon, public = publicLeafIcon)

Trees <- read.csv("~/Desktop/shiny-lect/mapping/Trees.csv")
names(Trees)[c(47,48)] <- c("longitude", "latitude")
private_vs_public <- ifelse(Trees[, "OWNERSHIP"] == "PRIV", "private", "public")

Trees <- cbind(Trees, private_vs_public = private_vs_public)
Trees$PLANTED_DATE <- as.Date(Trees$PLANTED_DATE)
decade <- year(Trees$PLANTED_DATE) - year(Trees$PLANTED_DATE) %% 10
Trees <- cbind(Trees, decade = decade)

Trees <- Trees[!is.na(Trees$decade) & !is.na(Trees$latitude) & !is.na(Trees$longitude), ]

# decades
# 1950s
# 1960s
# 1970s
# 1980s
# 1990s
# 2000s
# 2010s

m <- leaflet(Trees[Trees$decade == 1990, ]) %>% #subset in here!
  addTiles() %>%
  addMarkers(~longitude, ~latitude, icon = ~LeafIcons[private_vs_public], 
             label = ~COMMON_NAME,
             clusterOptions = markerClusterOptions())
m


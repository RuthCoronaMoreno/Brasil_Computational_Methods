# --------------------------------------------------#
# Scientific computing
# ICTP/Serrapilheira 2022
# Topic: Introduction to spatial data in R
# --------------------------------------------------#

library(sf)
library(tmap)
library(dplyr)
data(World)
# package tmap has a syntax similar to ggplot. The functions start all with tm_
tm_shape(World) +
  tm_borders()


head(World)
names(World)
class(World)
dplyr::glimpse(World)

plot(World[1])
plot(World[,1])
plot(World[1,])

plot(World["pop_est"])

#Plot Mexico
Mex=filter(World, name=='Mexico')
plot(Mex)


head(World[, 1:4])
class(World)
class(World$geometry)
head(sf::st_coordinates(World))
no_geom <- sf::st_drop_geometry(World)
class(no_geom)
st_bbox(World)


#Manipulating sf objects
names(World)
unique(World$continent)

World %>%
  filter(continent == "North America") %>%
  tm_shape() +
  tm_borders()


#You can also create new variables and use them in your maps:
World %>%
  mutate(our_countries = if_else(iso_a3 %in% c("COL","BRA", "MEX"), "red", "grey")) %>%
  tm_shape() +
  tm_borders() +
  tm_fill(col = "our_countries") +
  tm_add_legend("fill",
                "Countries",
                col = "red")


#########################################################
#Loading, ploting, and saving a shapefile from the disk
#########################################################
#install.packages("rnaturalearth")
#install.packages("remotes")
#remotes::install_github("ropensci/rnaturalearthhires")
library(rnaturalearth)
library(rnaturalearthhires)
mx <- ne_states(country = "mexico", returnclass = "sf")
plot(mx)

dir.create("data/shapefiles", recursive = TRUE)
st_write(obj = mx, dsn = "data/shapefiles/mx.shp", delete_layer = TRUE)

mx2 <- read_sf("data/shapefiles/mx.shp")
class(mx)
class(mx2)

plot(mx)
plot(mx2)

####################################################
#Loading, ploting, and saving a raster from the disk
####################################################

library(raster)
dir.create(path = "data/raster/", recursive = TRUE)
tmax_data <- getData(name = "worldclim", var = "tmax", res = 10, path = "data/raster/")
plot(tmax_data)

is(tmax_data) #the data are a raster stack, several rasters piled
dim(tmax_data)
extent(tmax_data)
res(tmax_data)

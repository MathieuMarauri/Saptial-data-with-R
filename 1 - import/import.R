
# Different ways to import spatial data into R.

# Packages --------------------------------------------------------------------------

library('sp') # sp class 
library('sf') # sf class 
import::from('rgdal', 'readOGR') # read shape files
library('osmdata') # extract data from open street map
library('osrm') # shortest route between two points and isochrone
library('ggplot2') # fortify sp files and plot maps
library('microbenchmark') # benchmark computation time


# Shape files -----------------------------------------------------------------------

# Read shp files, a folder that contains at least (.shp, .shx, .dbf, .prj)
# Data from https://gadm.org/download_country.html

# using rgdal
singapore_sp <- readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
singapore_sp <- readOGR(dsn = 'data/SGP_adm_shp', layer = 'SGP_adm0')

# plot country map
singapore_df <- fortify(singapore_sp, region = 'ISO')
ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_fixed() + 
  theme_void() + 
  theme(panel.border = element_rect(colour = 'grey30', size = 0.5, fill = 'transparent'))

# using sf
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp', layer = 'SGP_adm0')

# plot
ggplot(data = singapore_sf) + 
  geom_sf(colour = 'grey40', fill = 'grey30') + 
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))

# benchmark reading times
reardogr <- function() readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
readogr_layer <- function() readOGR(dsn = 'data/SGP_adm_shp', layer = 'SGP_adm0')
stread <- function() st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
stread_layer <- function() st_read(dsn = 'data/SGP_adm_shp', layer = 'SGP_adm0')
benchmark <- microbenchmark(reardogr(), readogr_layer(), stread(), stread_layer(),
                            times = 50)
autoplot(benchmark)

# save result and clean session
saveRDS(singapore_df, '1 - import/data/singapore.rds')
saveRDS(benchmark, '1 - import/data/benchmark_read.rds')
rm(singapore_sp, singapore_sf, singapore_df, reardogr, readogr_layer, stread, stread_layer)


# Read kml file ---------------------------------------------------------------------

# Read kml files using rgdal and sf.
# Data from https://data.gov.sg/dataset/master-plan-2014-planning-area-boundary-web

# using rgdal 
singapore_sp <- readOGR(dsn = 'data/singapore_areas.kml')

# using sf
singapore_sf <- st_read(dsn = 'data/singapore_areas.kml')

# benchmarck
kml_rgdal <- function() readOGR(dsn = 'data/singapore_areas.kml')
kml_sf <- function() st_read(dsn = 'data/singapore_areas.kml')
shp_rgdal <- function() readOGR(dsn = 'data/singapore_areas')
shp_sf <- function() st_read(dsn = 'data/singapore_areas')
benchmark <- microbenchmark(kml_rgdal(), kml_sf(), shp_rgdal(), shp_sf(), 
                            times = 50)
autoplot(benchmark)

# save results and clean session
saveRDS(benchmark, '1 - import/data/benchmark_read2.rds')
rm(singapore_sp, singapore_sf, kml_rgdal, kml_sf, shp_rgdal, shp_sf, benchmark)


# From open street map --------------------------------------------------------------

# Extracting data from open street map. 

# singapore bounding box
singapore_bbox <- matrix(c(103.604201, 
                           1.182086, 
                           104.089178, 
                           1.477715), nrow = 2)

# get the streets inside bbox from osm
query <- opq(bbox = singapore_bbox)
query <- add_osm_feature(opq = query, 
                         key = 'highway',
                         key_exact = FALSE,
                         value_exact = FALSE,
                         match_case = FALSE)

# create sf object and keeps lines
streets <- osmdata_sf(q = query)
streets <- streets$osm_lines

# map
ggplot(data = streets) +
  geom_sf(colour = 'grey40', size = 0.5) + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'))

# save result and clean session
saveRDS(streets, '1 - import/data/streets.rds')
rm(singapore_bbox, query, streets)


# Shortest route --------------------------------------------------------------------

# Find the sorthest route between two points.

# point of interest
poi <- data.frame(id = 1:4, 
                  long = c(103.864325, 103.874916, 103.989693, 103.789615), 
                  lat = c(1.281949, 1.305004, 1.359878, 1.404454))

# extract shortest path between two points
shotest_path_1 <- osrmRoute(src = c(1, 103.864325, 1.281949),
                            dst = c(2, 103.874916, 1.305004),
                            sp = TRUE)
shotest_path_2 <- osrmRoute(src = c(1, 103.989693, 1.359878),
                            dst = c(2, 103.789615, 1.404454),
                            sp = TRUE)

# fortify to plot 
shotest_path_1 <- fortify(shotest_path_1, region = 'src')
shotest_path_2 <- fortify(shotest_path_2, region = 'src')

# imports streets for plot
streets <- readRDS('1 - import/data/streets.rds')

# map
ggplot() +
  geom_sf(data = streets, colour = 'grey40', size = 0.5) +
  geom_point(data = poi, mapping = aes(x = long, y = lat, group = id), 
             colour = 'dodgerblue4', shape = 17, size = 3) + 
  geom_path(data = shotest_path_1, mapping = aes(x = long, y = lat, group = group), 
            color = 'maroon4') +
  geom_path(data = shotest_path_2, mapping = aes(x = long, y = lat, group = group), 
            color = 'maroon4') +
  xlim(c(103.77, 103.99)) + 
  ylim(c(1.27, 1.42)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))

# Computing shortest route for all combinations of points in a data frame

# shortest route for all combination
for (i in 1:(nrow(poi) - 1)) {
  for (j in (i+1):nrow(poi)) {
    route <- osrmRoute(src = as.numeric(poi[i, ]),
                       dst = as.numeric(poi[j, ]),
                       sp = TRUE)
    route <- st_as_sf(route)
    if (exists("shortest_path")) {
      shortest_path <- rbind(shortest_path, route)
    } else {
      shortest_path <- route
    }
  }
}

# add id to fortify
shortest_path$id <- paste(shortest_path$src, shortest_path$dst, sep = '_')
shortest_path <- as(shortest_path, 'Spatial')
shortest_path <- fortify(shortest_path, region = 'id')

# plot
ggplot() +
  geom_sf(data = streets, colour = 'grey40', size = 0.5) +
  geom_point(data = poi, mapping = aes(x = long, y = lat, group = id), 
             colour = 'dodgerblue4', shape = 17, size = 3) + 
  geom_path(data = shortest_path, mapping = aes(x = long, y = lat, group = group), 
            color = 'maroon4') +
  xlim(c(103.77, 103.99)) + 
  ylim(c(1.27, 1.42)) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))

# clean session
rm(poi, i, j, route, shortest_path, streets, shortest_path_1, shortest_path_2)


# Isochrone -------------------------------------------------------------------------

# Isochrone map with osm

# point of interest
poi <- data.frame(id = 1:3, 
                  long = c(103.864325, 103.989693, 103.789615), 
                  lat = c(1.281949, 1.359878, 1.404454))

# extract isochrone
isochrone_1 <- osrmIsochrone(loc = poi[1, c('long', 'lat')], 
                             breaks = c(0, 5, 10, 15)) %>% 
  st_as_sf(.)
isochrone_2 <- osrmIsochrone(loc = poi[2, c('long', 'lat')], 
                             breaks = c(0, 5, 10, 15)) %>% 
  st_as_sf(.)
isochrone_3 <- osrmIsochrone(loc = poi[3, c('long', 'lat')], 
                             breaks = c(0, 5, 10, 15)) %>% 
  st_as_sf(.)


# map
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')

ggplot(data = singapore_sf) + 
  geom_sf(colour = 'grey30', fill = 'grey30') + 
  geom_sf(data = isochrone_1, mapping = aes(fill = min), alpha = 0.5, show.legend = FALSE) + 
  geom_sf(data = isochrone_2, mapping = aes(fill = min), alpha = 0.5, show.legend = FALSE) +
  geom_sf(data = isochrone_3, mapping = aes(fill = min), alpha = 0.5, show.legend = FALSE) +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent'))


# Spatial data manipulation. Treatments are presented for sf and for sp class.

# Packages --------------------------------------------------------------------------

library('sf') # sf class
library('sp') # sp class
library('rgdal') # read shape file 
library('dplyr') # filter spatial data
library('spdplyr') # manipulation of sp objects
library('broom') # tidy sp objects to data frame
# devtools::install_github('tidyverse/ggplot2')
library('rgeos') # manipulate sp objects
library('ggplot2') # plot maps
library('gridExtra') # mulitple plot on the same page
library('microbenchmark') # benchmark processing time
library('spatialEco') # point in poly function


# Class coercion ---------------------------------------------------------------------

# How to go from one class to the other. Sf to sp and sp to sf. Data frame to and from sp or sf.

# import data as sp
singapore_sp <- readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')

# import data as sf
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')

# sp to sf
singapore_sf2 <- st_as_sf(singapore_sp)
all.equal(singapore_sf, singapore_sf2)

# sf to sp
singapore_sp2 <- as(object = singapore_sf, Class = 'Spatial')
all.equal(singapore_sp, singapore_sp2)

# sp to data frame (broom is used as ggplot::fortify is to be deprecated according to help)
singapore_df <- tidy(x = singapore_sp, region = 'ISO')
# singapore_df <- fortify(model = singapore_sp, region = 'ISO')
# a data frame with long, lat, order, hole, piece, group and id

# data frame to sp point
poi <- data.frame(id = 1:4, 
                  long = c(103.864325, 103.874916, 103.989693, 103.789615), 
                  lat = c(1.281949, 1.305004, 1.359878, 1.404454))
poi$name <- c('Gardens by the Bay', 'National Stadium', 'Airport', 'Zoo')

# choose appropriate projection
poi_sp <- SpatialPointsDataFrame(coords = poi[, c('long', 'lat')], 
                                 data = poi,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

# data frame to sf
poi_sf <- st_as_sf(x = poi, coords = c('long', 'lat'))

# clean session
rm(singapore_sp, singapore_sf, singapore_sp2, singapore_sf2, singapore_df, poi, poi_sp, poi_sf)


# Simplify shapes -------------------------------------------------------------------

# Simplify the level of details of the shapes.

# Using sp

# import data 
singapore_sp <- readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')

# plot data without simplification
singapore_df <- fortify(singapore_sp, region = 'ISO')
singapore_sp_plot <- ggplot(data = singapore_df, 
                            mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_fixed() + 
  theme_void() + 
  labs(title = 'No simplification') + 
  theme(plot.title = element_text(hjust = 0.5))

# simplify at different levels using rgeos::gSimplify
singapore_sp_1 <- gSimplify(singapore_sp, tol = 0.001)
singapore_df <- fortify(singapore_sp_1, region = 'ISO')
singapore_sp_1 <- ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_fixed() + 
  theme_void() + 
  labs(title = '0.001') + 
  theme(plot.title = element_text(hjust = 0.5))

singapore_sp_2 <- gSimplify(singapore_sp, tol = 0.005)
singapore_df <- fortify(singapore_sp_2, region = 'ISO')
singapore_sp_2 <- ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_fixed() + 
  theme_void() + 
  labs(title = '0.005') + 
  theme(plot.title = element_text(hjust = 0.5))

singapore_sp_3 <- gSimplify(singapore_sp, tol = 0.01)
singapore_df <- fortify(singapore_sp_3, region = 'ISO')
singapore_sp_3 <- ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_fixed() + 
  theme_void() + 
  labs(title = '0.01') + 
  theme(plot.title = element_text(hjust = 0.5))

# multiple plots on the same page
grid.arrange(singapore_sp_plot, singapore_sp_1, singapore_sp_2, singapore_sp_3, 
             ncol = 2, nrow = 2)

# Using sf

# import data 
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')

# plot data without simplification
singapore_sf_plot <- ggplot(data = singapore_sf) + 
  geom_sf(fill = 'grey20', colour = 'grey20') + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'No simplification')

# simplify at different levels using st_simplify from dev version
singapore_sf_1 <- st_simplify(singapore_sf, preserveTopology = TRUE, dTolerance = 0.001)
singapore_sf_1_plot <- ggplot(data = singapore_sf_1) + 
  geom_sf(fill = 'grey20', colour = 'grey20') + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = '0.001')

singapore_sf_2 <- st_simplify(singapore_sf, preserveTopology = TRUE, dTolerance = 0.005)
singapore_sf_2_plot <- ggplot(data = singapore_sf_2) + 
  geom_sf(fill = 'grey20', colour = 'grey20') + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = '0.005')

singapore_sf_3 <- st_simplify(singapore_sf, preserveTopology = TRUE, dTolerance = 0.01)
singapore_sf_3_plot <- ggplot(data = singapore_sf_3) + 
  geom_sf(fill = 'grey20', colour = 'grey20') + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'), 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = '0.01')

# multiple plots on the same page
grid.arrange(singapore_sf_plot, singapore_sf_1_plot, singapore_sf_2_plot, singapore_sf_3_plot, 
             ncol = 2, nrow = 2)

# benchmark
singapore_sp <- readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
simplify_sp <- function() gSimplify(singapore_sp, tol = 0.0008)
simplify_sf <- function() st_simplify(singapore_sf, 
                                      preserveTopology = TRUE, 
                                      dTolerance = 0.0008)
benchmark <- microbenchmark(simplify_sp(), simplify_sf(), times = 50)
autoplot(benchmark)

# save results and clean session
saveRDS(benchmark, '2 - manipulation/data/benchmark_simplify.rds')
rm(singapore_sp, singapore_sp_plot, singapore_df, singapore_sp_1, singapore_sp_2, 
   singapore_sp_3, singapore_sf_1, singapore_sf_2, singapore_sf_3, singapore_sf_plot, 
   singapore_sf, benchmark, singapore_sf_1_plot, singapore_sf_2_plot, singapore_sf_3_plot, 
   simplify_sp, simplify_sf)


# Filter spatial data ---------------------------------------------------------------

# Dplyr can be used to filter/select/transform a spatial data object (sf or sp). The
# spatial class will be sticky. Joins can only be done between an sf object and a pure
# data frame. Package spdplyr provides methods for spatial objects for most verbs.


# Union objects ---------------------------------------------------------------------

# Union several polygons into one

# Using sf

# union two specific areas into one
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')

# add colors to areas that will be unioned
areas_sf <- areas_sf %>% 
  mutate(color = ifelse(OBJECTID %in% c(20, 34, 50), 1, 0))

# plot areas
plot <- ggplot(data = areas_sf) + 
  geom_sf(mapping = aes(fill = factor(color)), colour = 'grey40') + 
  theme_void() + 
  scale_fill_manual(values = c('grey30', 'dodgerblue4'), guide = 'none') +
  labs(title = 'Areas to union') +
  theme(panel.grid.major = element_line(colour = 'transparent'), 
        plot.title = element_text(hjust = 0.5))

# union specific areas 
areas_union_sf <- areas_sf %>% 
  filter(OBJECTID %in% c(20, 34, 50))
areas_union_sf <- st_union(areas_union_sf)
areas_union_sf <- st_sf(id = 1, geometry = areas_union_sf)

# plot
plot_union <- ggplot(data = areas_sf) + 
  geom_sf(fill = 'grey30', colour = 'grey40') + 
  geom_sf(data = areas_union_sf, fill = 'dodgerblue4', colour = 'grey40') +
  theme_void() + 
  scale_fill_manual(values = c('dodgerblue4', 'grey30')) +
  labs(title = 'Areas grouped') +
  theme(panel.grid.major = element_line(colour = 'transparent'), 
        plot.title = element_text(hjust = 0.5))

grid.arrange(plot, plot_union, ncol = 2, nrow = 1)

# Using sp

# union two specific areas into one
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')

# add colors to areas that will be unioned
areas_df <- tidy(areas_sp, region = 'OBJECTID')

areas_df <- areas_df %>% 
  left_join(data.frame(id = c('20', '34', '50'), color = 1), by = 'id')
areas_df$color[is.na(areas_df$color)] <- 0

# plot areas
plot <- ggplot(data = areas_df, 
               mapping = aes(x = long, y = lat, group = group, fill = factor(color))) + 
  geom_polygon(colour = 'grey40') + 
  scale_fill_manual(values = c('grey30', 'dodgerblue4'), guide = 'none') +
  labs(title = 'Areas to union') +
  theme_void() + 
  coord_fixed() +
  theme(panel.grid.major = element_line(colour = 'transparent'), 
        plot.title = element_text(hjust = 0.5))

# union specific areas 
areas_union_sp <- areas_sp %>% 
  filter(OBJECTID %in% c(20, 34, 50))
areas_union_sp <- gUnionCascaded(areas_union_sp)
areas_union_sp <- SpatialPolygonsDataFrame(Sr = areas_union_sp, data = data.frame(id = 1))
areas_union_df <- tidy(areas_union_sp, region = 'id')

# plot
plot_union <- ggplot(data = areas_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'grey40', fill = 'grey30') + 
  geom_polygon(data = areas_union_df, mapping = aes(x = long, y = lat, group = group),
               colour = 'grey40', fill = 'dodgerblue4') +
  labs(title = 'Areas grouped') +
  theme_void() + 
  coord_fixed() +
  theme(panel.grid.major = element_line(colour = 'transparent'), 
        plot.title = element_text(hjust = 0.5))

grid.arrange(plot, plot_union, ncol = 2)

# benchmark union functions on all areas
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
union_sp <- function() gUnionCascaded(areas_sp)
union_sf <- function() st_union(areas_sf)

benchmark <- microbenchmark(union_sp(), union_sf(), times = 50)
autoplot(benchmark)

# save results and clean session
saveRDS(benchmark, '2 - manipulation/data/benchmark_union.rds')
rm(areas_sp, areas_sf, areas_df, union_sp, union_sf, benchmark, plot_union, plot, 
   areas_union_sp, areas_union_sf, areas_union_df)


# Projection ------------------------------------------------------------------------

# Change projection to have all spatial data in the same projection.

# Problem when different porjection
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
areas_df <- tidy(x = areas_sp, region = 'OBJECTID')
poi <- data.frame(id = 1:4, 
                  long = c(103.864325, 103.874916, 103.989693, 103.789615), 
                  lat = c(1.281949, 1.305004, 1.359878, 1.404454),
                  name = c('Gardens by the Bay', 'National Stadium', 'Airport', 'Zoo'))
plot1 <- ggplot() + 
  geom_polygon(data = areas_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed() + 
  theme_bw() + 
  theme(text = element_text(family = 'Georgia'))

plot2 <- ggplot(data = poi, mapping = aes(x = long, y = lat)) + 
  geom_point(colour = 'dodgerblue4') +
  coord_fixed() + 
  theme_bw() + 
  theme(text = element_text(family = 'Georgia'))

grid.arrange(plot1, plot2, ncol = 2, nrow = 1)

# Using sp

# import data
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
areas_sp <- spTransform(x = areas_sp, 
                        CRSobj = CRS("+proj=longlat +datum=WGS84"))

# simplify to avoid the topology exception created with the new projection and create a
# spatialPolygonDataFrame
areas_sp <- gSimplify(areas_sp, tol = 0.00001)
areas_sp <- SpatialPolygonsDataFrame(Sr = areas_sp, 
                                     data = data.frame(id = 1:323), 
                                     match.ID = FALSE)

# map
areas_df <- tidy(x = areas_sp, region = 'id')
ggplot(data = areas_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'grey40', fill = 'grey30') + 
  geom_point(data = poi, mapping = aes(x = long, y = lat), colour = 'white') + 
  coord_fixed() +
  theme_void()

# Using sf

# import data
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
areas_sf <- st_transform(x = areas_sf, crs = "+proj=longlat +datum=WGS84")

# simplify to avoid toplogy exception
areas_sf <- st_simplify(areas_sf, preserveTopology = TRUE, dTolerance = 0.00001)

# map
ggplot(data = areas_sf) + 
  geom_sf(colour = 'grey40', fill = 'grey30') + 
  geom_point(data = poi, mapping = aes(x = long, y = lat), colour = 'white') + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'),
        panel.border = element_rect(colour = 'grey30', size = 0.5, fill = 'transparent'))

# benchmark on subzones
subzones_sp <- readOGR(dsn = 'data/singapore_subzones/MP14_SUBZONE_NO_SEA_PL.shp')
subzones_sf <- st_read(dsn = 'data/singapore_subzones/MP14_SUBZONE_NO_SEA_PL.shp')
transform_sp <- function() spTransform(x = subzones_sp, 
                                       CRSobj = CRS("+proj=longlat +datum=WGS84"))
transform_sf <- function() st_transform(x = subzones_sf, 
                                        crs = "+proj=longlat +datum=WGS84")
benchmark <- microbenchmark(transform_sp(), transform_sf(), 
                            times = '50')
autoplot(benchmark)

# save results and clean session
saveRDS(benchmark, '2 - manipulation/data/benchmark_transform.rds')
rm(subzones_sp, subzones_sf, subzones_df, transform_sp, transform_sf, benchmark)


# Filter spatial data with polygons -------------------------------------------------

# Filer elements that are contained by a polygon. 

# Using sf

# import the data
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
streets_sf <- readRDS('1 - import/data/streets.rds')

# streets contained in the Singapore boundaries
contained <- st_covers(x = singapore_sf, y = streets_sf)
streets_sf <- streets_sf %>% slice(contained[[1]]) 

# map
ggplot(data = streets_sf) + 
  geom_sf(colour = 'grey40') + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'))

# Using sp

# import the data
singapore_sp <- readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
streets_sp <- readRDS('1 - import/data/streets.rds')
streets_sp <- as(streets_sp, 'Spatial')

# streets contained in the Singapore boundaries
contained <- gCoveredBy(streets_sp, singapore_sp, byid = TRUE)
streets_sp <- streets_sp %>% 
  filter(as.logical(contained))

# coerce to data frame and map
streets_df <- tidy(x = streets_sp, region = 'osm_id')
ggplot(data = streets_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_path(colour = 'grey40') + 
  coord_fixed() + 
  theme_void()

# benchmark
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
streets_sf <- readRDS('1 - import/data/streets.rds')
singapore_sp <- readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
streets_sp <- readRDS('1 - import/data/streets.rds')
streets_sp <- as(streets_sp, 'Spatial')
filter_sf <- function() st_covers(x = singapore_sf, y = streets_sf)
filter_sp <- function() gCoveredBy(streets_sp, singapore_sp, byid = TRUE)
benchmark <- microbenchmark(filter_sf(), filter_sp(), times = 30)
autoplot(benchmark)

# save results and clean session
saveRDS(benchmark, '2 - manipulation/data/benchmark_filter.rds')
rm(singapore_sf, streets_sf, singapore_sp, streets_sp, streets_df, contained, filter_sf, 
   filter_sp, benchmark)


# Associate points with polygons ----------------------------------------------------

# Find the polygons that contains a given point to add information to the points (or the
# polygons).

# Using sf

# polygon data with its associated data
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp', 
                    quiet = TRUE) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  select(OBJECTID, PLN_AREA_N, geometry)

# point data with its associated information
poi <- data.frame(id = 1:4, 
                  long = c(103.864325, 103.874916, 103.989693, 103.789615), 
                  lat = c(1.281949, 1.305004, 1.359878, 1.404454),
                  name = c('Gardens by the Bay', 'National Stadium', 'Airport', 'Zoo')) %>% 
  st_as_sf(coords = c('long', 'lat'), crs = st_crs(areas_sf))

# joining using st_covers
result <- st_join(x = poi, y = areas_sf, join = st_covered_by, left = FALSE)
# resulting geometry is for the polygons but with additionnal data from the points

# Using sp

# polygon data with its associated data
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp', 
                    verbose = FALSE) %>% 
  spTransform(CRSobj = CRS("+proj=longlat +datum=WGS84")) %>% 
  select(OBJECTID, PLN_AREA_N)

# point data with its associated information
poi <- data.frame(id = 1:4, 
                  long = c(103.864325, 103.874916, 103.989693, 103.789615), 
                  lat = c(1.281949, 1.305004, 1.359878, 1.404454),
                  name = c('Gardens by the Bay', 'National Stadium', 'Airport', 'Zoo')) 
poi_sp <- SpatialPointsDataFrame(coords = poi[, c('long', 'lat')], 
                                 data = poi,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

# find the areas that covers the points
result <- point.in.poly(poi_sp, areas_sp)
# resulting geometry is for the points but with additionnal data from the polygons

# clean session
rm(areas_sf, poi, result, poi_sp, areas_sp)


# Centroids -------------------------------------------------------------------------

# Get the centroids of polygons, usefull for plotting names. 

# Using sp

# import data
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')

# get the centroid of each area
area_centroids_sp <- gCentroid(areas_sp, byid = TRUE, id = areas_sp@data$PLN_AREA_N)

# coerce to data frame and map
area_centroids_df <- data.frame(area_centroids_sp)
area_centroids_df$name <- rownames(area_centroids_df)
areas_df <- tidy(x = areas_sp, region = 'OBJECTID')
ggplot(data = areas_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'grey40', fill = 'grey30') + 
  geom_text(data = area_centroids_df, mapping = aes(x = x, y = y, label = name, group = name), 
            colour = 'white', size = 1) + 
  coord_fixed() + 
  theme_void()

# Using sf

# import data
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')

# get the centroid of each area
area_centroids_sf <- st_centroid(areas_sf)

# coerce to data frame to plot names
area_centroids_df <- st_coordinates(area_centroids_sf)
area_centroids_df <- data.frame(area_centroids_df)
area_centroids_df$name <- areas_sf$PLN_AREA_N

# map
ggplot(data = areas_sf) + 
  geom_sf(colour = 'grey40', fill = 'grey30') + 
  geom_text(data = area_centroids_df, mapping = aes(x = X, y = Y, label = name), 
            colour = 'white', size = 1) + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'))

# benchmark
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
centroids_sp <- function() gCentroid(areas_sp, byid = TRUE)
centroids_sf <- function() st_centroid(areas_sf)
benchmark <- microbenchmark(centroids_sp(), centroids_sf(), times = 50)
autoplot(benchmark)

# save results and clean session
saveRDS(benchmark, '2 - manipulation/data/benchmark_centroids.rds')
rm(areas_sf, area_centroids_sf, area_centroids_df, areas_sp, area_centroids_sp, centroids_sp,
   centroids_sf, benchmark, areas_df)


# Plot benchmark --------------------------------------------------------------------

# Benchmark time needed to plot a map using sp or sf objects. Benchmarks are done in a
# markdown file because of issue 2252 of ggplot2

# Singapore areas

# import data
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp', verbose = FALSE)
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp', quiet = TRUE)

# benchmark
plot_sp <- function() plot(ggplot(data = areas_sp, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon())
plot_sf <- function() plot(ggplot(data = areas_sf) + geom_sf())
benchmark <- microbenchmark(plot_sp(), plot_sf(), times = 20)
autoplot(benchmark)
saveRDS(benchmark, '2 - manipulation/data/benchmark_plot_areas.rds')

# Singapore streets

# import data
streets_sf <- readRDS('1 - import/data/streets.rds')
streets_sp <- as(streets_sf, 'Spatial')

# benchmark
plot_sp <- function() {
  plot(ggplot(data = streets_sp, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon())
}
plot_sf <- function() {
  plot(ggplot(data = streets_sf) + geom_sf())
}
benchmark <- microbenchmark(plot_sp(), plot_sf(), times = 10)
autoplot(benchmark)
saveRDS(benchmark, '2 - manipulation/data/benchmark_plot_streets.rds')
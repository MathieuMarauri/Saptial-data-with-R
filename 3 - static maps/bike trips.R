
# Map of bikes trips in Austin, Texas.

# Packages ----------------------------------------------------------------

library('dplyr') # dataset manipulation
library('osmdata') # extracting spatial data 
library('ggplot2') # plots
library('sf') # sf class manipulation
library('mapsapi') # get direction from google api (no keys required and works with sf)
library('osrm') # get direction from osm api


# Import data -------------------------------------------------------------

# Import and prepare data for visualization: creation of one table with number
# of trips by time between two stations. 

# import trips data
trips <- readRDS('3 - static maps/data/trips.rds')

# number of trips trips by start and end station
trips <- trips %>% 
  group_by(start_station_id, end_station_id) %>% 
  summarise(count = n_distinct(trip_id))

# import stations coordinates
station <- readRDS('3 - static maps/data/stations.rds')
station <- station %>% 
  filter(status == 'active')


# Austin streets --------------------------------------------------------------

# Get the streets of Austin from osm

# austin bbox derived from min and max of station longitude and latitude
austin_bbox <- matrix(c(min(station$longitude) - 0.02, 
                        min(station$latitude) - 0.015, 
                        max(station$longitude) + 0.02, 
                        max(station$latitude) + 0.02), 
                      ncol = 2)

# build the query to get the streets geometry
query <- opq(bbox = austin_bbox)
query <- add_osm_feature(opq = query, 
                         key = 'highway',
                         key_exact = FALSE,
                         value_exact = FALSE,
                         match_case = FALSE)

# extract the streets 
streets <- osmdata_sf(q = query)
streets <- streets$osm_lines

# map the streets
# ggplot() + 
#   geom_sf(data = streets)

# clean session
rm(query, austin_bbox)


# Direction between bike stations -------------------------------------

# The direction between all pairs of station are extracted from google and from osm.

# table with all pairs of stations
station_pairs <- unique(station$station_id) %>%
  combn(m = 2, simplify = TRUE) %>%
  t(.) %>%
  as.data.frame(.) %>%
  setNames(c('src_id', 'dest_id'))

# add long/lat to the source station and the destination station
station_pairs <- station_pairs %>%
  left_join(station, by = c('src_id' = 'station_id')) %>%
  select(-name, -status) %>%
  rename(src_long = longitude, src_lat = latitude) %>%
  left_join(station, by = c('dest_id' = 'station_id')) %>%
  select(-name, -status) %>%
  rename(dest_long = longitude, dest_lat = latitude)

# function to extract the geometry attributes of the direction using google api
getDirection <- function(origin_long, origin_lat, destination_long, destination_lat,
                         mode = 'bicycling') {
  origin <- c(origin_long, origin_lat)
  destination <- c(destination_long, destination_lat)
  direction <- mp_directions(
    origin = origin,
    destination = destination,
    alternatives = FALSE,
    mode = 'bicycling'
  )
  direction <- mp_get_routes(direction)
  return(direction$geomerty)
}

# direction between all pair of station
directions <- station_pairs %>%
  mutate(geometry = st_sfc(NULL, crs = st_crs(streets)))
for (i in 1:nrow(directions)) {
  directions$geometry[i] <- tryCatch(getDirection(origin_long = directions$src_long[i],
                                            origin_lat = directions$src_lat[i],
                                            destination_long = directions$dest_long[i],
                                            destination_lat = directions$dest_lat[i]),
                                        error = function(cond) NULL)
}
saveRDS(directions, '3 - static maps/data/directions_google.rds')

# number of routes found
sum(sapply(directions$geometry, function(x) length(x) == 0)) / nrow(directions)

# direction between all pair of station using osm api
directions <- station_pairs %>%
  mutate(geometry = st_sfc(NULL, crs = st_crs(streets)))
for (i in 1:nrow(directions)) {
  route <- osrmRoute(src = c(directions$src_id[i],
                             directions$src_long[i],
                             directions$src_lat[i]),
                     dst = c(directions$dest_id[i],
                             directions$dest_long[i],
                             directions$dest_lat[i]),
                     sp = TRUE)
  directions$geometry[i] <- st_as_sf(route)$geometry
}
saveRDS(directions, '3 - static maps/data/directions_osm.rds')

# number of routes found
sum(sapply(directions$geometry, function(x) length(x) == 0)) / nrow(directions)

# map
ggplot() +
  geom_sf(data = station_pairs, mapping = aes(geometry = geometry))

# clean session
rm(i, getDirection, station_pairs)


# Merge trips and paths  --------------------------------------------------

# Average number of trips between all pairs of stations by hour

# load directions
directions <- readRDS('3 - static maps/data/directions_osm.rds')

# id of the pair of stations to merge routes and number of trips
directions <- directions %>% 
  mutate(id = paste(src_id, dest_id, sep = '_'))

# create two ids in trips to have the two directions
trips <- trips %>% 
  mutate(id_1 = paste(start_station_id, end_station_id, sep = '_'),
         id_2 = paste(end_station_id, start_station_id, sep = '_'))

# add number of trips to directions
directions <- directions %>% 
  select(id, geometry) %>% 
  left_join(trips, by = c('id' = 'id_1')) %>% 
  left_join(trips, by = c('id' = 'id_2')) %>% 
  mutate(count = count.x + count.y) %>% 
  select(id, count, geometry)

# remove directions with no trips
directions <- directions %>% 
  filter(!is.na(count))

# convert to sf object
directions <- st_sf(directions, geometry = directions$geometry)

# clean session
rm(trips)


# Number of trips by street ---------------------------------------------------------

# The number of trips is given by route. It is necessary to have it by streets (or even
# part of streets). The methodology is described in
# https://stackoverflow.com/questions/49239791/interception-of-routes-with-number-of-trips

# convert the directions geometry to a data.table
routes_df <- data.frame(st_coordinates(directions))

# join with data from directions
directions$id <- 1:nrow(directions)
directions_dt <- directions
st_geometry(directions_dt) <- NULL

routes_df <- routes_df %>% 
  left_join(directions_dt, by = c('L1' = 'id'))

# create from and to column by shifting coordinates to have the minimal segment of routs
# possible. (assuming that coordinates are equally spaced).
routes_df <- routes_df %>% 
  group_by(L1) %>% 
  mutate(X_to = lead(X, n = 1L),
         Y_to = lead(Y, n = 1L)) %>% 
  ungroup(.)

# count number of trips by coordinates pair (add order id because summarise reorder result)
routes_df <- routes_df %>% 
  filter(!is.na(X_to)) %>% 
  mutate(segment_id = 1:n()) %>% 
  group_by(X, Y, X_to, Y_to) %>% 
  summarise(n_trips = sum(count), segment_id = unique(segment_id)[1]) %>% 
  arrange(segment_id) %>% 
  ungroup(.)

# Recreate the sf object
# add line_id to keep order
routes_df$line_id <- 1:nrow(routes_df)

# create two tables to then create the linestrings 
dt_from <- routes_df %>% 
  select(X, Y, n_trips, line_id)
dt_to <- routes_df %>% 
  select(X = X_to, Y = Y_to, n_trips, line_id)

# add order column to have linestring in proper order
dt_from <- dt_from %>% 
  mutate(line_sequence = 1)
dt_to <- dt_to %>% 
  mutate(line_sequence = 2)

# bind to and from tables and order then 
routes_df <- bind_rows(dt_from, dt_to) %>% 
  arrange(line_id, line_sequence)

# create one line string for line_id * n_trips 
routes_sf <- routes_df %>% 
  group_by(line_id, n_trips) %>% 
  summarise(geometry = st_linestring(x = matrix(c(X, Y), ncol = 2)) %>% 
              st_sfc(.)) %>% 
  st_as_sf(.)

# add crs so that routes can be plotted on the streets layer
st_crs(routes_sf) <- st_crs(streets)

# log transform to sea the differcne in count 
routes_sf$n_trips <- log(routes_sf$n_trips)

# plot
# ggplot() + geom_sf(data = routes_sf, mapping = aes(color = n_trips))

# clean session
rm(routes_df, dt_from, dt_to, directions_dt, directions)


# Map -------------------------------------------------------------------------------

# theme for the map
theme_dark_map <- function(base_size = 12) {
  theme_bw(base_size) +
    theme(text = element_text(color = "#ffffff"),
          rect = element_rect(fill = "#000000", color = "#000000"),
          plot.background = element_rect(fill = "#000000", color = "#000000"),
          panel.background = element_rect(fill = "#000000", color = "#000000"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}

# final map
ggplot() + 
  geom_sf(data = streets, color = 'grey30') + 
  geom_sf(data = routes_sf, 
          mapping = aes(color = n_trips, alpha = n_trips), 
          show.legend = FALSE) + 
  scale_color_gradient(low = "dodgerblue3", high = "#ffffff") + 
  geom_point(data = station, mapping = aes(x = longitude, y = latitude), 
             color = 'yellow', 
             size = 0.4) + 
  scale_x_continuous(limits = c(-97.785, -97.705)) + 
  scale_y_continuous(limits = c(30.245, 30.295)) + 
  theme_dark_map()

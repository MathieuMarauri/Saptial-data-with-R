
# World map with different projections and main cities.

# Packages --------------------------------------------------------------------------

library('dplyr') # dataset manipulation
library('sf') # spatial data class
library('ggplot2') # data viz


# World and city data ---------------------------------------------------------------

# world data from maptools package
data(wrld_simpl, package = "maptools")

# convert world to sf
world <- st_as_sf(wrld_simpl)

# population by city
cities <- data.table::fread('3 - static maps/data/world_cities.csv', data.table = FALSE)
cities <- cities %>% 
  arrange(-pop) %>% 
  slice(1:200)

# graticule from
# http://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-graticules/
graticule <- st_read(dsn = '3 - static maps/data/graticule/ne_10m_graticules_15.shp')

# simple map
ggplot() +
  # world map
  geom_sf(data = world) +
  # cities
  geom_point(data = cities, mapping = aes(x = lng, y = lat, size = pop), 
             color = 'dodgerblue4', alpha = 0.8, show.legend = FALSE) + 
  # graticule
  geom_sf(data = graticule, color = 'grey40', linetype = 'dashed', size = 0.3) + 
  # remove all elements but the map
  theme_void() + 
  # theme_void does not get rid of panel for sf
  theme(panel.grid.major = element_line(colour = 'transparent'))


# Robin projection --------------------------------------------------------------

# projection
projections <- c('+proj=robin', '+proj=eck1', '+proj=eck3', '+proj=eck6')
projection <- projections[4]

# world 
data(wrld_simpl, package = "maptools")
world <- st_as_sf(wrld_simpl) %>% 
  st_transform(crs = projection)

# cities
cities <- data.table::fread('3 - static maps/data/world_cities.csv', data.table = FALSE)
cities <- cities %>% 
  arrange(-pop) %>% 
  slice(1:200) %>% 
  st_as_sf(coords = c('lng', 'lat'), 
           crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(crs = projection)

# graticule
graticule <- st_read(dsn = '3 - static maps/data/graticule/ne_10m_graticules_15.shp') %>% 
  st_transform(crs = projection)

# map
ggplot() +
  # world map
  geom_sf(data = world, size = 0.3) +
  # cities
  geom_sf(data = cities, mapping = aes(size = pop), 
          color = 'dodgerblue4', alpha = 0.8) + 
  # graticule
  geom_sf(data = graticule, color = 'grey40', linetype = 'dashed', size = 0.3) + 
  # remove all elements but the map
  theme_void() + 
  # theme_void does not get rid of panel for sf
  theme(panel.grid.major = element_line(colour = 'transparent'))


# Ortho projection ------------------------------------------------------------------

# I could not make it work with sf so move to sp

# world 
data(wrld_simpl, package = "maptools")
world <- wrld_simpl %>%
  fortify(region = 'ISO2')

# cities
cities <- data.table::fread('3 - static maps/data/world_cities.csv', data.table = FALSE)
cities <- cities %>% 
  arrange(-pop) %>% 
  slice(1:200)

# graticule
graticule <- rgdal::readOGR(dsn = '3 - static maps/data/graticule/ne_10m_graticules_15.shp') %>% 
  fortify(region = 'dd')

# map
ggplot() +
  # world map
  geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), 
               size = 0.3) +
  # cities
  geom_point(data = cities, mapping = aes(x = lng, y = lat, size = pop), 
             color = 'dodgerblue4', alpha = 0.8) + 
  # graticule
  geom_path(data = graticule, mapping = aes(x = long, y = lat, group = group), 
            color = 'grey40', linetype = 'dashed', size = 0.3) + 
  # new projection
  coord_map(projection = 'ortho', orientation = c(10, -70, 30)) +
  # remove all elements but the map
  theme_void()




# Static maps examples

# Packages --------------------------------------------------------------------------

library('data.table') # read csv files
library('dplyr') # dataset and sf manipulation
library('tidyr') # wide to long transformation
library('stringi') # string manipulation
library('sf') # sf class
library('ggplot2') # data viz
library('osmdata') # get spatial data


# Basic plots -----------------------------------------------------------------------

# How to plot sf and sp objects.

# Plot sp object

# import data
singapore_sp <- readRDS('data/SGP_adm0.rds')

# fortify data (done by ggplot2 but gives better understanding of what is inside)
singapore_df <- fortify(singapore_sp, region = 'ISO')
glimpse(singapore_df)

# lat/long ok, id gives the id of the element (one subregion), group gives the rows that
# go together (used to plot polygons and path; same as giving color or size but all
# element are the same), order give the order into the points are to be plotted not to
# have a complete mess (example?). Hole defines if the resulting polygon is part of the
# country or if it is a hole (example). 

# map
ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon()


# World map -------------------------------------------------------------------------

# projections: https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/OverviewCoordinateReferenceSystems.pdf

# get shape data for every country of the world
data(wrld_simpl, package = "maptools")

# convert to sf
world <- st_as_sf(wrld_simpl)

# simple map
ggplot() +
  geom_sf(data = world)

# antarctic center
world <- st_transform(world, crs = '+proj=laea +lat_0=-90')

# clean session
rm(wrld_simpl, world)

# Major cities population -----------------------------------------------------------

# Map of the world with point for major cities, sized according to population.

# get shape data for every country of the world
data(wrld_simpl, package = "maptools")

# convert to sf
world <- st_as_sf(wrld_simpl)

# data for cities: http://simplemaps.com/data/world-cities
cities <- fread('3 - static maps/data/world_cities.csv')
cities <- cities %>% 
  arrange(-pop) %>% 
  slice(1:100)

# simple map
ggplot() +
  # world map
  geom_sf(data = world) +
  # cities
  geom_point(data = cities, mapping = aes(x = lng, y = lat, size = pop)) + 
  # remove all elements but the map
  theme_void() + 
  # theme_void does not get rid of panel for sf
  theme(panel.grid.major = element_line(colour = 'transparent')) 

# with a new projection
world <- st_transform(world, crs = '+proj=moll')

# cities
cities <- st_as_sf(x = cities, coords = c('lng', 'lat'), crs = '+proj=longlat +datum=WGS84')
cities <- st_transform(cities, crs = '+proj=moll')

ggplot() +
  # world map
  geom_sf(data = world) +
  # cities
  geom_sf(data = cities, mapping = aes(size = pop)) + 
  # remove all elements but the map
  theme_void() + 
  # theme_void does not get rid of panel for sf
  theme(panel.grid.major = element_line(colour = 'transparent')) 

# Styled map: inspired by http://luminocity3d.org/WorldCity/#3/12.00/10.00

# get shape data for every country of the world
data(wrld_simpl, package = "maptools")

# convert to sf and remove antartica and greenland
world <- st_as_sf(wrld_simpl) %>% 
  filter(!ISO2 %in% c('AQ', 'GL'))

# population by city over time
world_cities <- fread(file = '3 - static maps/data/world_city_pop_time.csv', 
                      data.table = FALSE, 
                      header = TRUE)

# keep values for years 1950-1990-2015-2030
world_cities <- world_cities %>% 
  select(-Index, -`Country Code`, -`Country or area`, -`City Code`, -Note) %>% 
  gather(`1950`, `1990`, `2015`, `2030`, key = 'year', value = 'pop') %>% 
  select(city = `Urban Agglomeration`, lat = Latitude, long = Longitude, year, pop)

# convert lat, long and pop to numeric  
world_cities <- world_cities %>% 
  mutate(lat = as.numeric(stri_replace_all_fixed(str = lat, pattern = ',', replacement = '.')),
         long = as.numeric(stri_replace_all_fixed(str = long, pattern = ',', replacement = '.')),
         pop = as.numeric(stri_replace_all_fixed(str = pop, pattern = ' ', replacement = '')))

# create dataset with cities for Europe, Asia and the US
europe_cities <- world_cities %>% 
  filter(lat >= 30 & lat <= 70 & long >= -20 & long <= 30)
asia_cities <- world_cities %>% 
  filter(lat >= -10 & lat <= 40 & long >= 90 & long <= 150)
us_cities <- world_cities %>% 
  filter(lat >= 20 & lat <= 50 & long >= -140 & long <= -60)

# function that plot the map and restict view 
plotMap <- function(data, xlim = c(-200, 200), ylim = c(-70, 90)) {
  ggplot() +
    # world map
    geom_sf(data = world, colour = 'grey80', fill = 'white') +
    # city population in 2015
    geom_point(data = filter(data, year == '2030'), 
               mapping = aes(x = long, y = lat, size = pop, color = year)) +
    # city population in 2000
    geom_point(data = filter(data, year == '2015'), 
               mapping = aes(x = long, y = lat, size = pop, color = year)) +
    # city population in 1980
    geom_point(data = filter(data, year == '1990'), 
               mapping = aes(x = long, y = lat, size = pop, color = year)) +
    # city population in 1950
    geom_point(data = filter(data, year == '1950'), 
               mapping = aes(x = long, y = lat, size = pop, color = year)) +
    # restrict view
    xlim(xlim) + 
    ylim(ylim) +
    # remove all elements but the map
    theme_void() +
    # choose better colors for the years
    scale_color_manual(values = c(`1950` = '#003344', `1990` = '#057AA2', `2015` = '#72DBFF', 
                                  `2030` = '#C9FBFB'), name = 'Year') +
    # change size range and remove size from legend
    scale_size_continuous(range = c(0.2, 10), guide = 'none') +
    # theme_void does not get rid of panel for sf
    theme(panel.grid.major = element_line(colour = 'transparent'), 
          panel.background = element_rect(fill = 'grey80'),
          legend.key = element_rect(fill = 'grey80'))
}

# world map
plotMap(data = world_cities)

# Europe map
plotMap(data = europe_cities, xlim = c(-20, 30), ylim = c(30, 70))

# Asia map
plotMap(data = asia_cities, xlim = c(90, 150), ylim = c(-10, 40))

# US map
plotMap(data = us_cities, xlim = c(-140, -60), ylim = c(20, 50))

# clean session
rm(cities, world, wrld_simpl, world_cities, europe_cities, asia_cities, us_cities, plotMap)


# Europe urban areas ----------------------------------------------------------------

# Map of Europe urban areas inspired from
# http://spatial.ly/2017/03/mapping-european-population/ Data is from
# http://ec.europa.eu/eurostat/fr/web/gisco/geodata/reference-data/population-distribution-demography/geostat

# # import data
# europe <- st_read(dsn = 'data/geostat/GEOSTATReferenceGrid/Grid_ETRS89_LAEA_1K-ref_GEOSTAT_POP_2011_V2_0_1.shp')
# population <- data.table::fread('data/geostat/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv')
# 
# # keep population and country and merge with spatial data
# europe <- population %>% 
#   select(country = CNTR_CODE, population = TOT_P, id = GRD_ID) %>% 
#   inner_join(europe, by = c('id' = 'GRD_ID'))
# 
# # plot the data
# ggplot() + 
#   geom_sf(data = europe %>% slice(sample(1:nrow(europe), size = 100000)), 
#           mapping = aes(geometry = geometry, alpha = population), 
#           size = 0.1, fill = 'white', show.legend = FALSE) + 
#   theme_void() + 
#   theme(panel.background = element_rect(fill = 'black'))

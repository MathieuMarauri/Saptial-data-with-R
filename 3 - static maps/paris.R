
# Create a map of Paris with the 'rive gauche' buildings in one color and the 'rive
# droite' buildings in another one. First buildings are extracted using osmdata (for the
# center of Paris to avoid getting too many buildings). To be able to properly separate
# the buildings in two groups the Seine is used as the boundary. A polygon is constructed
# using the Seine line then buildings covered by the polygon created with the Seine as the
# top border are classified as 'rive gauche'. The polygon is created using the Seine and
# two straight lines coming from each extreme point of the Seine et going to the bottom
# left corner of the map. 

# Packages --------------------------------------------------------------------------

library('sf') # spatial data framework
library('osmdata') # extrcat data from osm api
library('dplyr') # data frame manipulation
library('ggplot2') # data viz


# Paris buildings -------------------------------------------------------------------

# paris (center) bounding box
# paris_bbox <- matrix(c(2.314197, 
#                        48.828605, 
#                        2.401340, 
#                        48.868988), nrow = 2)
paris_bbox <- matrix(c(2.261734, 
                       48.815849, 
                       2.414546, 
                       48.890285), nrow = 2)

# build the query to get the buildings
query <- opq(bbox = paris_bbox, timeout = 30000)
query <- add_osm_feature(opq = query, 
                         key = 'building',
                         key_exact = FALSE,
                         value_exact = FALSE,
                         match_case = FALSE)

# extract the buildings
buildings <- osmdata_sf(q = query)
buildings <- buildings$osm_polygons

# keep id and geometry to simplify object
buildings <- buildings %>% 
  select(osm_id, geometry)

# map
ggplot() + 
  geom_sf(data = buildings, size = 0.1)

# save results and clean session
saveRDS(buildings, '3 - static maps/data/buildings_paris.rds')
rm(query)


# The Seine -------------------------------------------------------------------------

# build the query to get the Seine
query <- opq(bbox = paris_bbox, timeout = 30000)
query <- add_osm_feature(opq = query, 
                         key = 'water',
                         value = 'river',
                         key_exact = FALSE,
                         value_exact = FALSE,
                         match_case = FALSE)

# extract the Seine 
seine <- osmdata_sf(q = query)
seine <- seine$osm_lines

# only keep id and geometry to simplify object
seine <- seine %>% 
  select(osm_id, geometry)

# create a polygon out of the paris bounding box to filter the seine elements that are in
# it (bounding box extended a bit to make sure all south buildings are properly
# classified)
paris_bbox2 <- st_bbox(c(xmin = 2.261734 - 0.1, ymin = 48.815849 - 0.1, 
                         xmax = 2.414546 + 0.1, ymax = 48.890285 + 0.1), 
                       crs = st_crs(seine)) %>% 
  st_as_sfc(.)

# get the seine lines that are inside the paris box
seine_paris <- st_covers(paris_bbox2, seine)

# filter the seine to only keep the part that is in paris
seine <- seine %>% 
  slice(seine_paris[[1]])

# map
ggplot() + 
  geom_sf(data = seine)

# clean session
rm(query, paris_bbox, paris_bbox2, seine_paris)


# Polygon boundaries ----------------------------------------------------------------

# get the boundaries of the Seine to then find the coordinates of the extreme points
seine_boundary <- st_boundary(seine$geometry)

# table of the coordinates of the boundaries of the lines composing the Seine
coordinates_seine <- st_coordinates(seine_boundary) %>% 
  as.data.frame(.) %>% 
  select(long = X, lat = Y)

# find the coordiantes of the extreme points (top left and top bottom)
bottom_point <- min(coordinates_seine$lat)
left_point <- min(coordinates_seine$long)

# define the coordinates of the 3 points used to construct the left and the bottom border
bottom_point <- coordinates_seine %>% 
  filter(lat == bottom_point) %>% 
  as.numeric(.)
left_point <- coordinates_seine %>% 
  filter(long == left_point) %>% 
  as.numeric(.)
bottom_left_point <- c(left_point[1], bottom_point[2])

# define the borders of the polygon, the left one, the bottom one and the one defined by
# the Seine. The last one is the union of the Seine object composed of several lines.
left_border <- st_linestring(rbind(left_point, bottom_left_point))
bottom_border <- st_linestring(rbind(bottom_point, bottom_left_point))
# extract the multilinestring ([[1]]) from the sfc_multilinestring so it can be
# concatenate with the other borders (linestring)
seine_border <- st_union(seine$geometry)[[1]] 

# the borders are combined in the same geometry object and cast to multistring, then
# unioned
borders <- st_sfc(seine_border, left_border, bottom_border) %>% 
  st_cast("MULTILINESTRING") %>% 
  st_union(.) %>% 
  .[[1]]

# create un polygon with the borders and convert to sfc to be able to add a projection
polygon <- st_polygonize(borders) %>% 
  st_sfc(.)

# visualize the polygon 
polygon %>% 
  st_sf(.) %>%  {
    ggplot() + 
      geom_sf(data = ., mapping = aes(geometry = `.`))
  }

# clean session
rm(seine, seine_boundary, coordinates_seine, bottom_point, left_point, bottom_left_point, 
   left_border, bottom_border, seine_border, borders)


# Filter buildings ------------------------------------------------------------------

# projection of polygon set to the same one as buildings for st_covers to be applied
st_crs(polygon) <- st_crs(buildings)

# buildings inside the polygon
buildings_covered <- st_covers(polygon, buildings)

# 'rive droite' buildings
buildings_droite <- buildings %>% 
  slice(setdiff(1:nrow(buildings), buildings_covered[[1]]))

# 'rive gauche' buildings
buildings_gauche <- buildings %>% 
  slice(buildings_covered[[1]])

# save results and clean session
saveRDS(list(gauche = buildings_gauche, droite = buildings_droite), 
        '3 - static maps/data/buildings_filter.rds')
rm(buildings_covered, polygon, buildings)


# Paris map -------------------------------------------------------------------------

buildings <- readRDS('3 - static maps/data/buildings.rds')
buildings_droite <- buildings$droite
buildings_gauche <- buildings$gauche

# map
ggplot() + 
  geom_sf(data = buildings_droite, 
          fill = 'navajowhite', colour = 'navajowhite', size = 0.1) + 
  geom_sf(data = buildings_gauche, 
          fill = 'mistyrose', colour = 'mistyrose', size = 0.1) + 
  theme_void() + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(panel.background = element_rect(fill = 'grey60'), 
        panel.grid.major = element_line(colour = 'transparent'))

ggplot() + 
  geom_sf(data = buildings_droite, 
          fill = 'coral', colour = 'coral', size = 0.1) + 
  geom_sf(data = buildings_gauche, 
          fill = 'lightsteelblue4', colour = 'lightsteelblue4', size = 0.1) + 
  theme_void() + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme(panel.grid.major = element_line(colour = 'transparent'))

# save results and clean session
rm(buildings, buildings_gauche, buildings_droite)

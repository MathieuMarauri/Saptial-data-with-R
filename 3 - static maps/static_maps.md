---
title: "How to plot spatial data with R?"
output:
  html_document:
    keep_md: true
    theme: readable
    highlight: tango
---





---

This is the third part of a serie of posts about spatial data with R. The first two parts were about importing data into R and manipulating data with R. In this document I will try to show you how to make great maps with R. 

Although I will try to explain everything I use, it would be better if you have some knowledge about `ggplot2`. It is the package that I will use to plot the maps and some parts might be easier to understand if you have basics knowledge. 

##### _What you will not find here?_

This post is not about interactive maps. Nor it is a complete list of all the different kind of static maps you can do with R. I will only use `ggplot2` to plot maps hence you will not find any example using base plot function nor functions from other packages. 

##### _What you will find here?_

I will provide code to make basic maps and I will (hopefully) make nicer maps for the sole purpous of making maps. I hope that you'll find some examples useful or at least distracting. 

<br>

## How to plot _sf_ and _sp_ objects?

---

If you followed the first two parts, you know there are two different ways to store and manipulate data with R. In this first section I will show the minimal code you need to plot _sf_ or _sp_ objects.

### Plot _sp_ objects

Spatial data can consist of polygons, lines and points. As usual with `ggplot2` each type has its own `geom_*()` function. Polygons are plotted using `geom_polygon()`, lines using `geom_path()` and points, well, using `geom_point()`. 

Note that `geom_path()` is the way to go and not `geom_line()` as the latter will not link points in their true order but in the order of the x-axis (see `geom_path()` [documentation](http://ggplot2.tidyverse.org/reference/geom_path.html)).

But before actually making the map you want to convert you _spatial*dataFrame_ to a regular data.frame. It is not necessary as the `ggplot()` function will handle it itself but if you want to understand what goes inside the mapping argument you have to at least give a look at the result of `ggplot2::fortify()` or `bromm::tidy()`. 

The resulting data frame will have 7 columns: __long__, __lat__, __order__, __hole__, __piece__, __group__ and __id__. 

+ The __long__ and __lat__ columns are self explained. 
+ The __id__ column corresponds to the `region` argument of `ggplot2::fortify()` or `bromm::tidy()`. It gives the identifier of the original elements e.g. countries. It is the column you have to use if you want to add data to your map (population by country, ...) using a join. 
+ The __group__ column is used to tell `ggplot()` which elements are to be drawn together. Without it it would not be possible to have two separate lines for example because `geom_path()` would link the two. 
+ The __hole__ column defines polygons that are not part of the global geometry. You can see it as an island when you plot seas or a lake when you plot land. Basically if you want to change the fill color of your polygon, hole element will not be affected. 
+ The __order__ column define the order onto which the points must be plotted. If the order of the row is changed somehow then the resulting map will have straight lines connecting points thar are far away from each other. 
+ The __piece__ column is a mystery to me. I do not know exactly what it represents and I never had to use it. 

To help you visualize the effects of the different columns, I will make some simple examples of what could go wrong. 

#### The group column

Let's see the importance of the group column by trying to plot two simple distinct lines. The lines are created using _sp_ package and then converted to data frame using `broom::tidy()`. The creation of a _spatialLInesDataframe_ must follow several steps: create a _Line_ object, from this _Line_ create a _Lines_ object then create a _SpatialLines_ out of several lines and finally construct the _sp_ object adding a data slot (with rownmaes matching the ids of the _SpatialLines_).


```r
# creata Line objects
line_1 <- Line(cbind(c(1, 2, 3), c(3, 2, 2)))
line_2 <- Line(cbind(c(1, 2, 3), c(1, 1.5, 1)))

# create Lines object
line_1 <- Lines(list(line_1), ID = "a")
line_2 <- Lines(list(line_2), ID = "b")

# create the spatialLine
lines <- SpatialLines(list(line_1, line_2))

# create the spatialLinesDataFrame
lines_sp <- SpatialLinesDataFrame(lines, data = data.frame(id = c('a', 'b'), row.names = c('a', 'b')))
```

Now let's plot this two simple lines with and without the group column.


```r
lines_df <- broom::tidy(lines_sp, region = 'id')
without_group <- ggplot(data = lines_df, mapping = aes(x = long, y = lat)) + 
  geom_path() + 
  labs(title = 'Without the group variable')
with_group <- ggplot(data = lines_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_path() + 
  labs(title = 'With the group variable')
grid.arrange(without_group, with_group, ncol = 2)
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/group2-1.png" alt="The importance of the group column" width="75%" />
<p class="caption">The importance of the group column</p>
</div>

Without grouping, `geom_path()` attemps to link all the points, resulting in the wrong graph. 

Here is an example with an actual map.

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/group3-1.png" alt="The importance of the group column, second example" width="100%" />
<p class="caption">The importance of the group column, second example</p>
</div>

Whenever you have a plot looking like that, with straight lines crossing your map, you should first check that you have the group aesthetic then check if you did not modify the order of you rows, as explained below.

#### The order column

The __order__ colum has sligthly the same effects as the __group__ column although it is not directly used in the call to `ggplot()`. It defines the order of the rows of the data frame and `ggplot()` through `geom_path()` or `geom_polygon()` plots the points in the order they appear in the data argument. Hence if you alter the order of the rows of the data frame you obtain after `fortify()` or `bromm::tidy()` you should always reorder your data frame using the order column. Altering the order of the data will usely be done when you want to add information joining your data with another table. To avoid modifying the order you either have to reorder your data before plotting (or use `sort = FALSE` in the `merge()` function). 

If I want to plot the same lines as before but first I modify the order of the rows, I obtain a different result.

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/order-1.png" alt="The importance of the order column" width="75%" />
<p class="caption">The importance of the order column</p>
</div>

An example with an actual map. 

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/order2-1.png" alt="The importance of the order column, second example" width="100%" />
<p class="caption">The importance of the order column, second example</p>
</div>

### Plot _sf_ objects

Of course, with _sf_ objects you can also have polygons, lines and points. And the plots are also made using `ggplot2` but the difference is that you use `geom_sf()` in every case. The function actually plots the __geometry__ column of the _sf_ object. It is the type of this column that is respnsible for the type of the plot. 

An _sf_ object is a data frame with one column, usually named _geometry_, containing the spatial data. One [vignette](https://cran.r-project.org/web/packages/sf/vignettes/sf1.html#sf-objects-with-simple-features) of the `sf` package gives a great explanation of the structure of _sf_ objects. What you need to know is that the geometry column contains the spatial data (the polygons or the lines or ...). If you have points (or lines or ...) in this particular column then `geom_sf()` will plot points (or lines or ...). 

Note that as of March 2018 you have to use the development version of `ggplot2` to have the `geom_sf()` function. I have the version 2.2.1.9000, I guess versions higher than that should perfectly work. 


```r
devtools::install_github('tidyverse/ggplot2')
```

If `geom_sf()` is loaded, you can simply plot data without transforming the geometry column first. If you do not want to use the development version of `ggplot2` then you have to coerce you _sf_ object to an _sp_ object and finally to a dataframe. 


```r
areas_sf <- st_read(dsn = '../data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp', quiet = TRUE)
ggplot() + 
  geom_sf(data = areas_sf, mapping = aes(geometry = geometry))
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/sf1-1.png" alt="Plot spatial data using geom_sf()" width="75%" />
<p class="caption">Plot spatial data using geom_sf()</p>
</div>

Some things worth noting:

+ the default colours of `geom_sf()` are different than the ones for `geom_polygon()`. 
+ the data argument can of course be given in `ggplot()` directly but then you cannot specify the geometry aesthetic. Although `geom_sf()` is supposed to find the geometry column on his one, if your spatial column has a different name it might not find it. Furthermore if you have several spatial columns then you will have to specify the one you want in the geometry argument. More info can be found in the [documentation](http://ggplot2.tidyverse.org/reference/ggsf.html) of the function.
+ if you want to use `theme_void()` to get rid of everything but the plot, you will have to manually remove the panel grid using `colour = 'transparent'`. 

### The `ggplot2` grammar

If you want to add color, size, shape, ... you can do it as you would for any other visualisation using the `mapping` argument to map a layer to a variable (the size of a population, the different types of points of interet, ...) or directly for the entire map. 
<br> 

## World map

---

In this section I will illustrate different projections using a world map with the top 200 cities in terms of population. The data for the population by city can be found [here](http://simplemaps.com/data/world-cities). Graticules are added (lines corresponding to parallels of latitude and meridians of longitude) to see the impact of the projection (data can be downloaded [here](http://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-graticules/)). 


```r
library('dplyr')
library('sf')
library('ggplot2') # dev version 2.2.1.9000
```

#### The classic projection

The "WGS84" system is the most widely used. 


```r
# world data from maptools package
data(wrld_simpl, package = "maptools")

# convert world to sf
world <- st_as_sf(wrld_simpl)

# population by city
cities <- data.table::fread('data/world_cities.csv', data.table = FALSE)
cities <- cities %>%
  arrange(-pop) %>% 
  slice(1:200)

# graticules
graticule <- st_read(dsn = 'data/graticule/ne_10m_graticules_15.shp', quiet = TRUE)

# map
ggplot() +
  geom_sf(data = world) +
  geom_point(data = cities, mapping = aes(x = lng, y = lat, size = pop), 
             color = 'dodgerblue4', alpha = 0.8, show.legend = FALSE) + 
  geom_sf(data = graticule, color = 'grey40', linetype = 'dashed', size = 0.3) + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'))
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/world_classic-1.png" alt="Projection = &quot;+proj=longlat +datum=WGS84 +no_defs&quot;" width="100%" />
<p class="caption">Projection = "+proj=longlat +datum=WGS84 +no_defs"</p>
</div>

#### Robinson projection


```r
# robinson projection
projection <- '+proj=robin'

# world 
data(wrld_simpl, package = "maptools")
world <- st_as_sf(wrld_simpl) %>% 
  st_transform(crs = projection)

# cities
cities <- data.table::fread('data/world_cities.csv', data.table = FALSE)
cities <- cities %>% 
  arrange(-pop) %>% 
  slice(1:200) %>% 
  st_as_sf(coords = c('lng', 'lat'), 
           crs = '+proj=longlat +datum=WGS84 +no_defs') %>% 
  st_transform(crs = projection)

# graticule
graticule <- st_read(dsn = 'data/graticule/ne_10m_graticules_15.shp', quiet = TRUE) %>% 
  st_transform(crs = projection)

# map
ggplot() +
  geom_sf(data = world, size = 0.3) +
  geom_sf(data = cities, mapping = aes(size = pop), 
          color = 'dodgerblue4', alpha = 0.8, show.legend = FALSE) + 
  geom_sf(data = graticule, color = 'grey40', linetype = 'dashed', size = 0.3) + 
  theme_void() + 
  theme(panel.grid.major = element_line(colour = 'transparent'))
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/world_robin-1.png" alt="Projection = &quot;+proj=robin&quot;" width="100%" />
<p class="caption">Projection = "+proj=robin"</p>
</div>

#### Orthographic projection

To plot the world as a globe the _+proj=ortho_ must be used. As I could not make it work with `sf` and `geom_sf()` (see [this issue](https://github.com/r-spatial/sf/issues/509)) I did it with `sp`. The `coord_map()` is used to modify the projection in this case. 


```r
library('sp')

# world 
data(wrld_simpl, package = "maptools")
world <- wrld_simpl %>%
  fortify(region = 'ISO2')

# cities
cities <- data.table::fread('data/world_cities.csv', data.table = FALSE)
cities <- cities %>% 
  arrange(-pop) %>% 
  slice(1:200)

# graticule
graticule <- rgdal::readOGR(dsn = 'data/graticule/ne_10m_graticules_15.shp', verbose = FALSE) %>% 
  fortify(region = 'dd')

# map
ggplot() +
  geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), 
               size = 0.3) +
  geom_point(data = cities, mapping = aes(x = lng, y = lat, size = pop), 
             color = 'dodgerblue4', alpha = 0.8, show.legend = FALSE) + 
  geom_path(data = graticule, mapping = aes(x = long, y = lat, group = group), 
            color = 'grey40', linetype = 'dashed', size = 0.3) + 
  coord_map(projection = 'ortho', orientation = c(30, 0, 0)) +
  theme_void()
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/world_ortho-1.png" alt="Orientation c(30, 0, 0)" width="100%" />
<p class="caption">Orientation c(30, 0, 0)</p>
</div>


```r
ggplot() +
  geom_polygon(data = world, mapping = aes(x = long, y = lat, group = group), 
               size = 0.3) +
  geom_point(data = cities, mapping = aes(x = lng, y = lat, size = pop), 
             color = 'dodgerblue4', alpha = 0.8, show.legend = FALSE) + 
  geom_path(data = graticule, mapping = aes(x = long, y = lat, group = group), 
            color = 'grey40', linetype = 'dashed', size = 0.3) + 
  coord_map(projection = 'ortho', orientation = c(10, -70, 30)) +
  theme_void()
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/world_ortho2-1.png" alt="Orientation c(10, -70, 30)" width="100%" />
<p class="caption">Orientation c(10, -70, 30)</p>
</div>


<br>

## Paris map

---

I present here a map of Paris buildings. The buildings north of the Seine are in one color and the ones south of it are in an other. This map is inspired by [this one](https://www.r-graph-gallery.com/246-oliver-obrien-dataart/) from Oliver O'Brien. 

To make such a plot I followed 5 steps (the main packages or functions I used are also given).

1. Extraction of the buildings shape. `osmdata`.
2. Extraction of the Seine shape to be used as boundary to properly separate the buildings into two groups. `osmdata`.
3. Create a polygon that will cover the buildings south of the Seine. The Seine is the north boundary, the south and east ones are defined with straight lines from extreme points of the Seine and the bottom-left point of the map. `st_boundary()`, `st_coordinates`, `st_linestring()`, `st_union()`, `st_polygonize()` and `st_sfc()`.
4. Filter the buildings that are on the polygon juste creared. `st_crs()` and `st_covers()`.
5. Plot the map. `geom_sf()`.


```r
library('sf')
library('osmdata')
library('dplyr')
library('ggplot2')
```

#### The Paris building

The shapes of the buildings are extracted from the Open Street Map API using `osmdata`.


```r
# paris bounding box
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
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/buildings_map-1.png" alt="Paris buildings" width="75%" />
<p class="caption">Paris buildings</p>
</div>

#### The Seine 

Since I want to plot the buildings on the _rive gauche_ (the south of the Seine) in one color and the buildings in the _rive droite_ (the north of the Seine) in another one I need to filter the buildings according to their position relatively the Seine. The first step is to get the Seine spatial data.


```r
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
# it (bounding box extended a bit to make sure all south buildings are properly classified)
paris_bbox2 <- st_bbox(c(xmin = 2.261734 - 0.001, ymin = 48.815849 - 0.001, 
                         xmax = 2.414546 + 0.001, ymax = 48.890285 + 0.001), 
                       crs = st_crs(seine)) %>% 
  st_as_sfc(.)

# get the seine lines that are inside the paris box
seine_paris <- st_covers(paris_bbox2, seine)

# filter the seine to only keep the part that is in paris
seine <- seine %>% 
  slice(seine_paris[[1]])
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/seine_map-1.png" alt="The Seine" width="75%" />
<p class="caption">The Seine</p>
</div>

#### The polygon

To filter the buildings that are south of the Seine the idea is to construct a polygon that would cover all the buildings in the south. One way to do that is to use the Seine as a boundary and to create two lines to "close" the polygon. To create these lines I need the extreme points of the Seine (the one at the north west and the one at the south east). 


```r
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
```

Two borders are created using these points. 


```r
# define the borders of the polygon, the left one, the bottom one and the one defined by
# the Seine. The last one is the union of the Seine object composed of several lines.
left_border <- st_linestring(rbind(left_point, bottom_left_point))
bottom_border <- st_linestring(rbind(bottom_point, bottom_left_point))
# extract the multilinestring ([[1]]) from the sfc_multilinestring so it can be
# concatenate with the other borders (linestring)
seine_border <- st_union(seine$geometry)[[1]]

# the borders are combined in the same geometry object and cast to multilinestring, then unioned
borders <- st_sfc(seine_border, left_border, bottom_border) %>% 
  st_cast("MULTILINESTRING") %>% 
  st_union(.) %>%
  .[[1]]
```

And the polygon is finally created.


```r
# create un polygon with the borders and convert to sfc to be able to add a projection
polygon <- st_polygonize(borders) %>% 
  st_sfc(.)
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/polygon_map-1.png" alt="The filtering polygon" width="75%" />
<p class="caption">The filtering polygon</p>
</div>

#### Filter the buildings

Now that I have a polygon covering the builfings of the _rive_gauche_ I can filter them.


```r
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
```

#### The map

Here is the final result.


```r
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
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/paris_map-1.png" alt="Paris map" width="75%" />
<p class="caption">Paris map</p>
</div>

<br> 

## Bike trips

---

I show here the number of bike trips between all pairs of bike stations in Austin, Texas. 

The goal is to map the number of trips that went through each street of Austin. I followed these steps:

1. Get the data for the number of trips and bike stations localisation. 
2. Get the streets data. `osmdata`
3. Find the shortest route between all pairs of stations. `osrm`
4. Compute the number of trips by streets as some shortest routes have common parts. 
5. Plot the map. `ggplot2`

### Import the data

The data comes from [Kaggle](https://www.kaggle.com/jboysen/austin-bike). I cleaned it a bit and aggregated it at the pair of stations level. 


```r
# import trips data
trips <- readRDS('data/trips.rds')

# number of trips trips by start and end station
trips <- trips %>% 
  group_by(start_station_id, end_station_id) %>% 
  summarise(count = n_distinct(trip_id))

# import stations coordinates
station <- readRDS('data/stations.rds')
station <- station %>% 
  filter(status == 'active')
```

### The streets

The data to plot the streets of Austin are extracted with `osmdata`. 


```r
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
```

### Shortest route

I use the Open Street Map API with the `osrm` package to get the shortest route between pairs of stations.


```r
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

# direction between all pairs of stations using osm api
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
```



I then join the routes with the trips data to have the number of trips by route.


```r
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
```

### Number of trips by street

I want to have the number of trips by street (actually by segment of street). The methodology is given in [this SO question](https://stackoverflow.com/questions/49239791/interception-of-routes-with-number-of-trips). Thanks to _SymbolixAU_ to have answered my question. I just transformed the workflow using `dplyr`. 


```r
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
```

### The map


Finally I can plot the map.


```r
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
  theme_bw() +
  theme(text = element_text(color = "#ffffff"),
        rect = element_rect(fill = "#000000", color = "#000000"),
        plot.background = element_rect(fill = "#000000", color = "#000000"),
        panel.background = element_rect(fill = "#000000", color = "#000000"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
```

<div class="figure" style="text-align: center">
<img src="static_maps_files/figure-html/bike_map-1.png" alt="Bike trips in Austin" width="75%" />
<p class="caption">Bike trips in Austin</p>
</div>

## Conclusion

You can do great maps in R (see [this map](http://spatial.ly/2017/03/mapping-european-population/) and [this one](http://spatial.ly/2017/04/population-lines-how-and-why-i-created-it/) for great visualization and associated R code from [this blog](http://spatial.ly/)). In this post I focused only on static maps with `ggplot2`. I plan on making a use case on interactive maps, possibly through a shiny app. 

<br>

<cite> -- Mathieu Marauri</cite>

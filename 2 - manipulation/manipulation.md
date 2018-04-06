---
title: "How to manipulate spatial data with R?"
output:
  html_document:
    keep_md: true
    theme: readable
    highlight: tango
---





---

This is the second part of a serie of posts about spatial data in R. The first part was about getting data into R. Here I will focus on manipulating the data. 

We are used to work with data frames but spatial data is not stored as a regular data frame and manipulating it requires extra efforts. I wrote this document to try to show you that with the right set of tools it can be quite easy to work with spatial data in R.

#### _What you will not find here?_

This post is not about spatial data analysis, although manipulating such data can help a lot. You will not find any model to analyze spatial data. You will also not find anything regarding creating your own spatial data from scratch. It might be useful for testing purpose but as I did not have to do such spatial data creation in my early projects on spatial data I keep that for another time (you might find something related to creating spatial data from scratch in the next post of this serie).

##### _What you will find here?_

I will provide sample codes to perform some treatments on spatial data. Such treatments are:

+ regular filtering, selecting, mutating
+ simplifying shapes
+ unioning spatial objects
+ filtering spatial objects with polygons
+ accessing the centroid of a polygon

It is not a complete list of all the things you could do with spatial data but merely what I encountered while working on spatial data. 

#### _How is spatial data stored in R?_

If you tried to import spatial data into R using `rgdal::readOGR` and/or `sf::st_read` you may have noticed that there are two classes of spatial objects in R: _sf_ and _sp_. Each class has a different way to store the data and also a different set of functions that can be applied on it. The _sp_ class is the oldest one I think and _sf_ class is more _tidy-like_. I do not have a preference yet but as I write this document I tend to like _sf_ more. A comparison can be found at the [end](comparison-between-sp-and-sf) of this document.

The following sections will contain a sample code for each class and a benchmark. Before exploring the different treatments I will show you how to go from one class to the other.

The main packages used for the spatial data manipulation are `sf` and `sp`/`rgeos` for the _sf_ class and the _sp_ class respectively. 


```r
install.packages('sf')
install.packages('sp')
install.packages('rgeos')

library('sf')
library('sp')
library('rgeos')
```

Other packages are used in this section: [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) to plot the maps and [microbenchmark](https://cran.r-project.org/web/packages/microbenchmark/index.html) to benchmark functions. 

<br>

## From _sp_ to _sf_ and vice-versa

---

Event if you can stick with one class/package to do everything you need, sometimes you may find the way to solve your issue in one package quicker than in the other. Knowing how to go from _sp_ to _sf_ and back easily could be quite useful. I would recommend you to try to use only the _sp_ class or only the _sf_ class but when I was discovering spatial data I was just going to whatever seems the easiest and that required going from one class to the other which felt like a pain. 

#### The _sf_ class

Spatial data stored as an sf object is a data frame with a list column containing the geometry of the data. The other columns are additionnal data such as names or values attached to each geometric element. 

#### The _sp_ class

Spatial data stored as an sp object is a list with a slot data containing the additional data and one slot with the geometry. This slot contains as many elements as there are polygons or lines or points in the data. 

The structure of the classes will be much more explicit to you once you start playing with it.


```r
# import data as sp
singapore_sp <- readOGR(dsn = '../data/SGP_adm_shp/SGP_adm0.shp', verbose = FALSE)

# import data as sf
singapore_sf <- st_read(dsn = '../data/SGP_adm_shp/SGP_adm0.shp', quiet = TRUE)
```

#### _sp_ to _sf_


```r
singapore_sf <- st_as_sf(singapore_sp)
```

#### _sf_ to _sp_


```r
singapore_sp <- as(object = singapore_sf, Class = 'Spatial')
```

#### _sp_ to data frame


```r
# Using broom::tidy
singapore_df <- tidy(x = singapore_sp, region = 'ISO')

# using ggplot2::fortify
singapore_df <- fortify(model = singapore_sp, region = 'ISO')
```

The `region` argument must be provided so that the resulting data frame is correct. Since points defining the geometry are now stored as two columns long/lat it is necessary to be able to group the points according to their parent polygon or line. The `region` argument usually is the id of the data slot of the _sp_ object. 

#### data frame to _sp_

Data frames with coordinates for points can be coerced to _sp_ objects. 


```r
poi <- data.frame(id = 1:4, 
                  long = c(103.864325, 103.874916, 103.989693, 103.789615), 
                  lat = c(1.281949, 1.305004, 1.359878, 1.404454),
                  name = c('Gardens by the Bay', 'National Stadium', 'Airport', 'Zoo'))
##   id     long      lat               name
## 1  1 103.8643 1.281949 Gardens by the Bay
## 2  2 103.8749 1.305004   National Stadium
## 3  3 103.9897 1.359878            Airport
## 4  4 103.7896 1.404454                Zoo
poi_sp <- SpatialPointsDataFrame(coords = poi[, c('long', 'lat')], 
                                 data = poi,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
```

#### data frame to _sf_


```r
poi_sf <- st_as_sf(x = poi, coords = c('long', 'lat'))
```

<br>

## Spatial data manipulation

---

Your spatial data can contain additionnal data (either in the data slot for the _sp_ class or in the columns of the _sf_ object). If you want to manipulate this data without losing the spatial information you can do it directly using `dplyr` as you would do with regular data. 

For the _sp_ class to be sticky (or for the dplyr verbs to work on the _sp_ class) the `spdplyr` package is needed. Simply load it using `library` and the filter/select/mutate/... functions will work as usual for you. 

Note that if you filter an _sf_ table using `data.table` the spatial class will be lost even if you keep the geometry column. 

Also note that the joins you might want to do to add some extra information on your data are to be done on the data slot for _sp_ objects and not directly on the _sp_ object. 

The classic situation in which such manipulations are needed are when you want to add the population by area for example or map only the streets with at least some number of trips. 
Merging two spatial object can also be done, an example is given below in [section](merging-spatial-objects) about joins.

<br>

## Simplify shapes

---

Simplification of shapes can be done to reduce the size of the object or simplify the look of the map. It also might get useful if you reproject your data to keep a valid spatial object (see the [projections section](#projections) for more details on this point).


```r
# Using sp/rgeos
singapore_sp <- gSimplify(singapore_sp, tol = 0.0008)

# Using sf
singapore_sf <- st_simplify(singapore_sf, preserveTopology = TRUE, dTolerance = 0.0008)
```

Using different level for the simplification, different maps can be obtained.

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/simplify_plot-1.png" alt="Maps obtained with different level of simplification" width="75%" />
<p class="caption">Maps obtained with different level of simplification</p>
</div>

In this document I will not show the code used to make the plots as the third part of this serie will be on plotting spatial data. If you want the code you can look at the source code of this document on Github.

Here is a processing time benchmark of the two methods.


```r
simplify_sp <- function() gSimplify(singapore_sp, 
                                    tol = 0.0008)
simplify_sf <- function() st_simplify(singapore_sf,
                                      preserveTopology = TRUE,
                                      dTolerance = 0.0008)
microbenchmark(simplify_sp(), simplify_sf(), times = 50)
# Unit: milliseconds
#          expr       min        lq     mean    median        uq       max neval cld
# simplify_sp()  3.226186  3.281553  3.72163  3.473284  3.789259  5.665372    50  a 
# simplify_sf() 22.074803 22.816449 25.15525 25.009049 26.950101 30.496866    50   b
```

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/bench_simplify_plot-1.png" alt="Processing times in milliseconds." width="50%" />
<p class="caption">Processing times in milliseconds.</p>
</div>

The `rgeos::gSimplify` function is faster than the equivalent _sf_ version. 

<br>

## Unioning spatial objects

---

It is possible to create a single spatial object out of several ones. A polygon made of several smaller polygons will lost its inner boundaries. This is useful if you want to build bigger administrative regions out of smaller ones for example and you do not have access to the corresponding raw shape files. 

Three areas of Singapore are combined to form one area.


```r
# Using sp/rgeos
# import data
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
# filter 3 areas
areas_union_sp <- areas_sp %>% 
  filter(OBJECTID %in% c(20, 34, 50))
# union the selected areas
areas_union_sp <- gUnionCascaded(areas_union_sp)

# Using sf
# import data
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
# filter 3 areas
areas_union_sf <- areas_sf %>% 
  filter(OBJECTID %in% c(20, 34, 50))
# union the selected areas
areas_union_sf <- st_union(areas_union_sf)
```

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/union_plot-1.png" alt="A nex area without inside boundaries" width="90%" />
<p class="caption">A nex area without inside boundaries</p>
</div>

Here is a processing time benchmark of the two methods. The benchmark is done on the union of all the areas of Singapore.


```r
union_sp <- function() gUnionCascaded(areas_sp)
union_sf <- function() st_union(areas_sf)

microbenchmark(union_sp(), union_sf(), times = 50)
# Unit: milliseconds
#       expr      min       lq     mean   median       uq      max neval cld
# union_sp() 237.6865 245.3070 268.6502 262.4769 278.4054 520.6266    50   b
# union_sf() 232.4635 239.7844 254.9096 252.1791 268.2144 287.5434    50  a 
```

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/bench_union_plot-1.png" alt="Processing times in milliseconds." width="50%" />
<p class="caption">Processing times in milliseconds.</p>
</div>

<br>

## Projections

---

There are many projections and some are better suited for specific situations. The main idea is that some projections preserve distance and others direction. But as you "zoom" to smaller and smaller areas e.g. city it is safe to consider that both distance and direction are preserved. 

You usually do not have to worry about projections when working with spatial data (at least it is what I experienced as I never had to go really deep in spatial data analysis). You may want to change the projection associated with your data, if any, in two scenari: you have two sources of data with two different coordinates referense system (crs), you want to plot your data in a different way. 

This section is only about transforming a spatial object projection so that you have a unique coordinates reference system accross all your data. 

Here is an example of some points of interest of Singapore being plotted on the Singapore areas without any transformation.

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/projection_plot1-1.png" alt="Tow different coordinate scales" width="100%" />
<p class="caption">Tow different coordinate scales</p>
</div>

The points are at the bottom of the plot, completely out of Singapore. It is because the points are plotted with coordinates in the range of 100 for longitude and 1 for latitude whereas the Singapore areas are in range of 10 000. Transforming the projection is the way to go to solve this issue. 


```r
# Using sp
areas_sp <- spTransform(x = areas_sp, CRSobj = CRS("+proj=longlat +datum=WGS84"))

# Using sf
areas_sf <- st_transform(x = areas_sf, crs = "+proj=longlat +datum=WGS84")
```

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/projection_plot2-1.png" alt="The Singapore map and the point of interest have the same projection" width="75%" />
<p class="caption">The Singapore map and the point of interest have the same projection</p>
</div>

After changing the projection it might be necessary to simplify the resulting object to avoid the following error when trying to plot the map: <span style = "color:red;">`Error in rgeos::gUnaryUnion(spgeom = SpP, id = IDs) : TopologyException: ...`</span>.

Also note that it is necessary for an object to have a crs defined to apply a transformation. 

The benchmark for the two functions is given here. It is done on the subzones of Singapore. 


```r
subzones_sp <- readOGR(dsn = 'data/singapore_subzones/MP14_SUBZONE_NO_SEA_PL.shp')
subzones_sf <- st_read(dsn = 'data/singapore_subzones/MP14_SUBZONE_NO_SEA_PL.shp')
transform_sp <- function() spTransform(x = subzones_sp, 
                                       CRSobj = CRS("+proj=longlat +datum=WGS84"))
transform_sf <- function() st_transform(x = subzones_sf, 
                                        crs = "+proj=longlat +datum=WGS84")
microbenchmark(transform_sp(), transform_sf(), times = '50')
# Unit: milliseconds
#           expr      min       lq      mean   median        uq      max neval cld
# transform_sp() 75.26166 83.18882 111.20221 88.81994 159.23243 186.8417    50   b
# transform_sf() 19.59641 22.59226  30.38338 24.15836  26.59191 103.8530    50  a 
```

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/bench_projection_plot-1.png" alt="Processing times in milliseconds" width="50%" />
<p class="caption">Processing times in milliseconds</p>
</div>

The _sf_ function is faster by a factor of almost 4.

<br>

## Filtering spatial objects with polygons 

---

In the first part, _Spatial data import_, the streets of Singapore were extracted using the Open Street Map API. What was actually extracted are all the streets inside a rectangular area (corresponding to the bounding box of Singapore). Since Singapore is obviously not a rectangle I ended up having streets that are not actually in Singapore. 

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/streets_map-1.png" alt="Streets inside the boundoing box of Singapore" width="75%" />
<p class="caption">Streets inside the boundoing box of Singapore</p>
</div>

A solution to this problem is to keep the streets that are inside the polygons defined by the boundaries of Singapore. The idea is to find the streets that are covered, or contained, in the Singapore polygon. 


```r
# Using sf
contained <- st_covers(x = singapore_sf, y = streets_sf)
streets_sf <- streets_sf %>% slice(contained[[1]]) 

# Using sp/rgeos
contained <- gCoveredBy(x = streets_sp, y = singapore_sp, byid = TRUE)
streets_sp <- streets_sp %>% 
  filter(as.logical(contained))
```

After filtering the data the map is now what we would expect.

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/filtering_spatial_plot-1.png" alt="Streets inside Singapore after fileter" width="75%" />
<p class="caption">Streets inside Singapore after fileter</p>
</div>

The results of the bencchmark can be found below.


```r
singapore_sf <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
streets_sf <- readRDS('1 - import/data/streets.rds')
singapore_sp <- readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
streets_sp <- readRDS('1 - import/data/streets.rds')
streets_sp <- as(streets_sp, 'Spatial')
filter_sf <- function() st_covers(x = singapore_sf, y = streets_sf)
filter_sp <- function() gCoveredBy(streets_sp, singapore_sp, byid = TRUE)
microbenchmark(filter_sf(), filter_sp(), times = 30)
# Unit: milliseconds
#        expr      min       lq     mean   median       uq      max neval cld
# filter_sf() 615.4518 630.4792 641.0061 639.5686 649.5518 679.4009    30   b
# filter_sp() 598.6866 612.8764 629.2098 624.4578 647.6980 680.8159    30  a 
```

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/bench_filtering_plot-1.png" alt="Processing times in milliseconds" width="50%" />
<p class="caption">Processing times in milliseconds</p>
</div>

There is no real difference in performance. 

<br>

## Merging spatial objects

---

Merging spatial objects between then is not a direct task. When you merge two spatial objects it must be because you want to add information from one object to the other based on the geometries. It may be for points inside polygons, for lines intersecting other lines, ... 

The use case I encountered was adding information coming from census data to points. Adding the mean revenue, the population, the age distribution to points represented potential location of stores for exemple. In this case you want to add information from the region in which your point is. 

The following example illustrate this situation. It is a toy example as I add the name of the area in which the points are. 


```r
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
result <- spatialEco::point.in.poly(poi_sp, areas_sp)
```

Note that the package `spatialEco` is needed to merge the points and the areas. You may find a way to do the same thing using the `rgeos` package with [this tutorial](http://www.nickeubank.com/wp-content/uploads/2015/10/RGIS2_MergingSpatialData_part1_Joins.html). 

Also note that `st_join()` performs left or inner join and if you want to add the name of the points contained in the polygons to the polygon data then you should do `st_join(x = areas_sf, y = poi, join = st_covers, left = FALSE)` which is the opposite of what I did. 

<br>

## Centroids

---

I wanted to get the centroids of polygons to plot the names of administrative regions on a map. It can be useful for other problems such as finding the center of hexagons in hexmaps. 

Here I extract the centroids of the areas of Singapore.


```r
# Using sp/rgeos
area_centroids_sp <- gCentroid(areas_sp, byid = TRUE)

# Using sf
area_centroids_sf <- st_centroid(areas_sf)
```

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/centroids_plot-1.png" alt="Areas names displayed on the map" width="75%" />
<p class="caption">Areas names displayed on the map</p>
</div>

The _sp_ function is faster by a factor of almost 3. 


```r
areas_sp <- readOGR(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
areas_sf <- st_read(dsn = 'data/singapore_areas/MP14_PLNG_AREA_NO_SEA_PL.shp')
centroids_sp <- function() gCentroid(areas_sp, byid = TRUE)
centroids_sf <- function() st_centroid(areas_sf)
microbenchmark(centroids_sp(), centroids_sf(), times = 50)
# Unit: milliseconds
#           expr      min       lq     mean   median        uq      max neval cld
# centroids_sp() 3.046685 3.303105 4.144796 3.536294  3.915951 27.00014    50  a 
# centroids_sf() 7.963056 8.902215 9.510857 9.306786 10.013586 12.18908    50   b
```

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/bench_centroids_plot-1.png" alt="Processing times in milliseconds" width="50%" />
<p class="caption">Processing times in milliseconds</p>
</div>

<br>

## Comparison between _sp_ and _sf_

---

I compare the _sf_ and the _sp_ way of dealing with spatial data using benchmark expirement on the task described in this document along with reading and plotting. Because speed is not the only thing that should be considered when choosing the best approach (if there is one) I will also discuss the space allocated in memory for _sf_ and for _sp_ objects and give personal thoughts about the ease of use.

#### The speed benchmarks

All the benchmark were done on a macbook pro, Intel Core i7, 2,5 GHz, 16GB of ram. 

Here is the benchmark on the _plot_ task. Plotting is done using `ggplot2` (much more details on how to plot spatial data will be given in the thid part of the serie on _Spatial data with R_). The first benchmark is done on the Singapore areas and the second one on the streets. The amount of data to plot has of course an impact on the time needed to print the map. 

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/bench_plot1-1.png" alt="Processing times in milliseconds to plot Singapore areas" width="50%" />
<p class="caption">Processing times in milliseconds to plot Singapore areas</p>
</div>

<div class="figure" style="text-align: center">
<img src="manipulation_files/figure-html/bench_plot2-1.png" alt="Processing times in seconds to plot Singapore streets" width="50%" />
<p class="caption">Processing times in seconds to plot Singapore streets</p>
</div>

Plotting _sp_ objects is faster than plotting _sf_ for the areas and it is the other way around for the streets. As it is not something you will do over and over, the difference in plotting time does not really matter. 

I present here a table giving the median time for the different benchmark experiments. Times are given in milliseconds

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Task </th>
   <th style="text-align:center;"> SP </th>
   <th style="text-align:center;"> SF </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Read </td>
   <td style="text-align:center;"> 2410.0 </td>
   <td style="text-align:center;"> 9.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Simplify </td>
   <td style="text-align:center;"> 3.5 </td>
   <td style="text-align:center;"> 25.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Union </td>
   <td style="text-align:center;"> 262.0 </td>
   <td style="text-align:center;"> 252.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Projection </td>
   <td style="text-align:center;"> 88.8 </td>
   <td style="text-align:center;"> 24.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Filter </td>
   <td style="text-align:center;"> 624.0 </td>
   <td style="text-align:center;"> 639.0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Centroid </td>
   <td style="text-align:center;"> 3.5 </td>
   <td style="text-align:center;"> 9.3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Plot </td>
   <td style="text-align:center;"> 511.0 </td>
   <td style="text-align:center;"> 1180.0 </td>
  </tr>
</tbody>
</table>

Depending on the task, _sf_ or _sp_ is the fastest. The greatest difference concerns the importation into R of the data where _sf_ is clearly better.

If your specific use case requires performing some operation over and over then you might want to chose one framework over the other depending on the task. But the overhead of having to switch from one class to the other may not worth it.

It is a tie.

#### The memory allocation

I used different objects in this document: the Singapore boundaries, the Singapores areas, the Singapore subzones and the streets of Singapore. Here is the amount of memory allocated for each of these objects depending on the class, _sf_ or _sp_.

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;"> Object </th>
   <th style="text-align:center;"> SP </th>
   <th style="text-align:center;"> SF </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;"> Boundaries </td>
   <td style="text-align:center;"> 190 KB </td>
   <td style="text-align:center;"> 154 KB </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Areas </td>
   <td style="text-align:center;"> 894 KB </td>
   <td style="text-align:center;"> 688 KB </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Subzones </td>
   <td style="text-align:center;"> 2 MB </td>
   <td style="text-align:center;"> 1.2 MB </td>
  </tr>
  <tr>
   <td style="text-align:center;"> Streets </td>
   <td style="text-align:center;"> 1.4 GB </td>
   <td style="text-align:center;"> 1.3 GB </td>
  </tr>
</tbody>
</table>

The _sf_ class needs less memory than the _sp_ class. It is not a significant difference, especially for large objects. 

Once again it is a tie.

#### Ease of use

Once you get used to them the two framework are really easy to work with (the learning curve is quite steep at the beginning, at least it was for me). 

The _sp_ framework is I think older than the _sf_ one hence it might be easier to get information/help for _sp_, this is at least what I experienced. Starting from scratch with _sf_ might get a bit harder as it will be more difficult to find answers. Plotting _sp_ objects requires coercing to data frame if you want to map layers such as _color_ or _fill_ for example.

On the other hand _sf_ package is self contained. You do not need `rgdal` to read the data and `rgeos` to manipulate it. Besides function names all start with `st_` which makes it easier to work with. I had some difficulties finding information on some functions as they are under `geos_*` functions descriptions and therefore not seen in the table of contents of the package description page. `ggplot2` can be used directly on the _sf_ object although the development version (2.2.1.9000) is require as version 2.2.1 does not have `geom_sf()`.

Both packages are easy to use with the pipe operator as the spatial object is often the first argument of the functions.

I tend to like _sf_ better but it is purely subjective and mainly because I need to load only one package and it is easier to use with `ggplot2`.

#### Conclusion

My best recommendation would be to try both and pick the framework you prefer. I personnaly now try to stick to the _sf_ framework. Anyway the most important thing is to be able to do exactly what you want, no matter which framework you use, no matter if you need to go from one class to the other a couple times.

On the next post I will present how to plot _sf_ and _sp_ objects. I will also demonstrate how to recreate a map like [this one](https://www.r-graph-gallery.com/246-oliver-obrien-dataart/). 

<br>

<cite> -- Mathieu Marauri</cite>

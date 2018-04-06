---
title: "How to import spatial data into R?"
output:
  html_document:
    keep_md: true
    theme: readable
    highlight: tango
---





---

This is the first part of a serie of posts about spatial data in R. Working with spatial data with R might be difficult the first time, this point aim to give example codes that will help you get strated. I am not an expert on spatial data but I worked on projects involving data and maps and I thought I could do a little review of what R has to offer to those interested in spatial data and R. This first part of this serie focuses on getting data into R.

##### _What is spatial data?_

Spatial data is data used to map things. It can be country boundaries, roads, special points of interests in a city, ... Basically it is a set of coordinates of points. There are 3 main categories:

+ points: locations of bike sharing station or bus stops in a city, cities in a country, ...
+ lines: roads, rivers, ...
+ polygons: regions inside a country, blocks inside a city, ...

##### _What you will not find here?_

This post is not about finding spatial data. There are numerous websites where you can find such data and depending on your specific need it might be an official government website or the github repo of someone who have been working on the same subject as you. That said, I found the Global Administative Areas website ([gadm](http://gadm.org/)) or the Diva Gis [website](http://www.diva-gis.org/gdata) to be a good start whenever you need spatial data about administrative areas of a country.

##### _What you will find here?_

I will show you how to import two types of spatial data files (shape files and kml files) into R ([in this section](#read-spatial-data)) and how to get data directly from the Open Street Map API ([in this section](#get-spatial-data-using-an-api)).

<br>

## Read spatial data

---

#### Shape files

The packages needed to read shape files and kml files are [sf](https://CRAN.R-project.org/package=sf) and [rgdal](https://CRAN.R-project.org/package=rgdal). Other packages are used in this section, [ggplot2](https://CRAN.R-project.org/package=ggplot2) to plot the maps and [microbenchmark](https://CRAN.R-project.org/package=microbenchmark) to benchmark functions. 

R provides two classes for spatial data: __`sp`__ and __`sf`__. Of course these classes are associated with their own packages and their own set of functions. The `sp` package is loaded with `rgdal` so you do not need to explicitly call `library(sp)`. 


```r
install.packages('sf')
install.packages('sp')
install.packages('rgdal')

library('sf') # st_read function, sf class
library('sp') # sp class
library('rgdal') # readOGR function
```

Reading spatial data into R can be done using `rgdal::readOGR()` or `sf::st_read()`. The functions have the same arguments. 


```r
# using rgdal
singapore <- readOGR(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
# alternative specification of the arguments
singapore <- readOGR(dsn = 'data/SGP_adm_shp', layer = 'SGP_adm0')

# using sf
singapore <- st_read(dsn = 'data/SGP_adm_shp/SGP_adm0.shp')
# alternative specification of the arguments
singapore <- st_read(dsn = 'data/SGP_adm_shp', layer = 'SGP_adm0')
```

<div class="figure" style="text-align: center">
<img src="import_files/figure-html/singapore_map-1.png" alt="Map of Singapore" width="75%" />
<p class="caption">Map of Singapore</p>
</div>

The `dsn` argument can contain either the .shp filename or the path to the folder with the shape files. The two ways are completely equivalent. When giving `dsn` as a folder path, it is not mandatory to provide `layer` but if the folder contains more than one shape then the first one is read with a warning. Specifying the layer ensures you read what you want. 

#### Kml files

Spatial data can also come as a kml file. It is read the same way as before, using either `rgdal::readOGR()` or `sf::st_read()`. 


```r
# using rgdal 
singapore <- readOGR(dsn = 'data/singapore_areas.kml')

# using sf
singapore <- st_read(dsn = 'data/singapore_areas.kml')
```

#### Benchmark

If speed is a concern to you, here is a benchmark of the two ways to read spatial data for the two different files. The results are obtained with a macbook pro, Intel Core i7, 2,5 GHz, 16GB of ram.


```r
readOGR_kml <- function() readOGR(dsn = 'data/singapore_areas.kml')
st_read_kml <- function() st_read(dsn = 'data/singapore_areas.kml')
readOGR_shp <- function() readOGR(dsn = 'data/singapore_areas')
st_read_shp <- function() st_read(dsn = 'data/singapore_areas')
microbenchmark(st_read_shp(), st_read_kml(), readOGR_shp(), readOGR_kml(), times = 50)
# Unit: milliseconds
#        expr         min          lq       mean     median         uq        max neval  cld
# kml_rgdal() 1307.999737 1351.141718 1376.21199 1377.21967 1400.41824 1442.55815    50    d
#    kml_sf()   75.023978   78.904805   82.97262   82.57872   86.00686   93.08332    50  b  
# shp_rgdal()  758.045765  815.433999  827.97331  829.29199  842.02652  880.32384    50   c 
#    shp_sf()    9.584409    9.974714   10.53054   10.33558   10.90389   13.62025    50 a 
```

<div class="figure" style="text-align: center">
<img src="import_files/figure-html/benchamrk_import_plot-1.png" alt="Processing times in milliseconds (log-scale)." width="50%" />
<p class="caption">Processing times in milliseconds (log-scale).</p>
</div>

Clearly `sf::st_read()` is faster. Reading _shp_ files is faster than reading _kml_ file although note that [this vignette](https://cran.r-project.org/web/packages/sf/vignettes/sf1.html#benchmarks) gives different results.

<br>

## Get spatial data using an API

---

Spatial data can also be directly imported into R using the [osmdata](https://cran.r-project.org/web/packages/osmdata/index.html) and the [osrm](https://cran.r-project.org/web/packages/osrm/index.html) packages. 


```r
install.packages('osmdata')
install.packages('osrm')

library('osmdata')
library('osrm')
```

#### Spatial elements

`osmdata` package can be used to extract data from the Open Street Map api. Pretty much every information about an area can be extracted (roads, boundaries, rivers, buildings, ...). The complete list of items can be found [here](https://wiki.openstreetmap.org/wiki/Map_Features).

To get data you define a bounding box (basically the min and max coordinates of the area you want data about) and then you call the API to extract a specific types of data (highways, trees, ...). Finally you build your spatial object. 

Here the streets in the area of Singapore are extracted. 


```r
# singapore bounding box
singapore_bbox <- matrix(c(103.604201, 
                           1.182086, 
                           104.089178, 
                           1.477715), nrow = 2)

# get the streets inside the bbox
query <- opq(bbox = singapore_bbox)
query <- add_osm_feature(opq = query, 
                         key = 'highway')

# create sf object from the query and keep lines (as highway are stored as lines)
streets <- osmdata_sf(q = query)
streets <- streets$osm_lines
```

<div class="figure" style="text-align: center">
<img src="import_files/figure-html/streets_map-1.png" alt="Streets of Singapore" width="75%" />
<p class="caption">Streets of Singapore</p>
</div>

Notice that we provided a rectangular bounding box so we also get streets that are outside Singapore. It is possible to only keep streets inside Singapore using `sf::st_covers()`. An example of how to use this function will be given in the part II of this serie, _Manipulate spatial data with R_. 

#### Shortest route

`osrm` package can be used to get the shortest route between two points. It also gives the distance and the estimated transport time associated with this route. The function is `osrm::osrmRoute()`.


```r
# extract shortest path between two points
shotest_path <- osrmRoute(src = c(1, 103.864325, 1.281949),
                          dst = c(2, 103.874916, 1.305004),
                          sp = TRUE)
```

The shortest paths between two pairs of points in Singapore are extracted. 

<div class="figure" style="text-align: center">
<img src="import_files/figure-html/shortest_map-1.png" alt="Shortest paths between two sets of points in Singapore." width="75%" />
<p class="caption">Shortest paths between two sets of points in Singapore.</p>
</div>

#### Isochrone

Isochrone maps are maps displaying the areas reachable in a certain amount of time from one specific point. 

Here I get the areas within 10 and 15 minutes driving around 3 points in singapore. 


```r
isochrone <- osrmIsochrone(loc = c(1, 103.864325, 1.281949),
                           breaks = c(0, 5, 10, 15))
```

<div class="figure" style="text-align: center">
<img src="import_files/figure-html/osrm_iso_plot-1.png" alt="Isochrones for 3 different points. Areas within 10 and 15 minutes are displayed." width="75%" />
<p class="caption">Isochrones for 3 different points. Areas within 10 and 15 minutes are displayed.</p>
</div>

<br>

## Conclusion

If you followed this post all the way here I hope that you are now able to properly import data into R. I only presented functions to get data from the Open Street Map API but there are also numerous other packages that can be used to get data from Google Maps ([mapsapi](https://CRAN.R-project.org/package=mapsapi), [googleway](https://CRAN.R-project.org/package=googleway), ...). This packages have great documentation and it should be easy to use them.

The second part of this serie will be about manipulating spatial data with R. 

<br>

<cite> -- Mathieu Marauri</cite>

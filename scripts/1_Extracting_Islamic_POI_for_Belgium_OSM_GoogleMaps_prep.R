#############################################################################################################
#############################################################################################################

## ---------------------------
##
## OSM and Google Maps: Extracting Islamic POI for Belgian Municipalities
##
## Author: Dra. Daria Dementeva
##
##
## Email: daria.dementeva@kuleuven.be
##
## ---------------------------
##
##   2022-2023
##
## ---------------------------

## Set Working Directory 

# setwd("...")  

# For Reproducibility

set.seed (05081997)

################################################################################################################ 
# Install packages
################################################################################################################ 

if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("sp")) install.packages("sp")
if(!require("ggmap")) install.packages("ggmap")
if(!require("BelgiumMaps.StatBel")) install.packages("BelgiumMaps.StatBel")
if(!require("leaflet")) install.packages("leaflet")
if(!require("igraph")) install.packages("igraph")
if(!require("tidygeocoder")) install.packages("tidygeocoder")

################################################################################################################ 
# Load packages 
################################################################################################################ 

library(tidyverse)
library(osmdata)
library(sf)
library(sp)
library(ggmap)
library(BelgiumMaps.StatBel)
library(leaflet)
library(igraph)
library(tidygeocoder)

################################################################################################################ 
# Check available tags and load data
################################################################################################################ 

# head(available_features())
# head(available_tags("amenity"))
data("BE_ADMIN_MUNTY")

################################################################################################################ 
# Install helper function to go around with the geometry entry for polygons (centroid reduction)
################################################################################################################ 

# Convert sfc_POINT to x/y Columns 

# Adapted from https://github.com/r-spatial/sf/issues/231

sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

# 1. Obtain OSM Data

###############################################################################################################
# Restaurants (Overall, 391 Islamic/Muslim Restaurants)
###############################################################################################################

rest <- getbb("Belgium") %>% # !!! mind this changes day-by-day as OSM gets updated
  opq() %>%
  add_osm_feature("amenity", "restaurant")

str(rest) 

rest_data <- osmdata_sf(rest)
rest_data

rest_points <- rest_data[["osm_points"]]
rest_polygons <- rest_data[["osm_polygons"]]

# write.csv(rest_points, "rest_points_2025_06.csv")
# write.csv(rest_polygons, "rest_polygons_2025_06.csv")

rest_points_final <- rest_points[, c("osm_id", "name", "cuisine","diet:halal", "geometry")]
nrow(rest_points_final) # 55453
rest_polygons_final <- rest_polygons[, c("osm_id", "name", "cuisine","diet:halal", "geometry")]
nrow(rest_polygons_final) # 3773

# Subset by Belgium (because bbox captures France, the Netherlands, Germany and Luxembourg)

# Points

rest_points_upd <- sfc_as_cols(rest_points_final)
x_long = as.vector(rest_points_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(rest_points_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # overwrite CRS once again to be sure
pt.in.poly2_pois <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 

rest_points_upd_df <- as.data.frame(rest_points_upd)

rest_points_overlayed <- cbind(rest_points_upd_df, pt.in.poly2_pois)

rest_points_overlayed_belgium <- rest_points_overlayed[!is.na(rest_points_overlayed$CD_MUNTY_REFNIS), ] # rows 36767

# Polygons

rest_polygons_final$centroids <- st_centroid(rest_polygons_final$geometry)

rest_polygons_upd <- sfc_as_cols(rest_polygons_final, rest_polygons_final$centroids)

x_long = as.vector(rest_polygons_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(rest_polygons_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # set CRS 
pt.in.poly_rest <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 

rest_polygons_upd_df <- as.data.frame(rest_polygons_upd)

rest_polygons_overlayed <- cbind(rest_polygons_upd_df, pt.in.poly_rest)

rest_polygons_overlayed_belgium <- rest_polygons_overlayed[!is.na(rest_polygons_overlayed$CD_MUNTY_REFNIS), ] # rows 2605


# Subset by Cuisine (entries not NA)

rest_points_overlayed_belgium_final <- rest_points_overlayed_belgium[!is.na(rest_points_overlayed_belgium$cuisine),] # 5403
rest_polygons_overlayed_belgium_final <- rest_polygons_overlayed_belgium[!is.na(rest_polygons_overlayed_belgium$cuisine),] # 1251

# df <- as.data.frame(table(unique(rest_points_overlayed_belgium_final$cuisine)))
# df <- as.data.frame(table(unique(rest_polygons_overlayed_belgium_final$cuisine)))

# Assign Islamic/Muslim Cuisines, Text Matching/Name Recognition

keyword_groups <- list(
  
  "African"= c("abyssinienne", "african","ethiopian", "cameroonian","congolese", "erithrean", "west_african", "african;couscous", "african;ethiopian", "belgian;congolese"),
  
  "Islamic"= c("algerian", "arab", "arabic", "afghan", "couscous","egyptian", "falafel", "kebab", "turkish","lebanese", "libyan","maghrebi", "maroccan", "mauritian", "middle_eastern","oriental", "pakistani", "palestinian", "persian", "pita", "shawarma", "shoarma", "pitta", "durum","kebab", "sandwich;turkish", "sandwich;moroccan", "syrian", "tajine","tunisian","pasta;turkish", "grill;moroccan", "african;tunisian", "african;turkish", "arab;asian;grill;indian;kebab;pasta;turkish", "	arab;couscous", "arab;grill", "arab;kebab", "arab;lebanese", "arabic;tajine", "barbecue;kebab;pizza;steak_house", "belgian;turkish", "burger;kebab;friture, burger;kebab;italian;pizza", "friture;kebab;turkish", "grill;chicken;kebab;barbecue;turkish", "grill;moroccan", "indian;bangladeshi", "iranian", "kebab;barbecue;arab", "kebab;egyptian","kebab;grill", "kebab;pizza", "kebab;pizza;turkish;moroccan", "kebab;turkish", "lebanese;arab", "lebanese;grill;italian;pasta", "lebanese;mediterranean", 
               "lebanese;mediterranean;arabic", "lebanese;palestinian", "lebanese;syrian", "middle eastern", "middle_eastern;lebanese;syrian", "Moroccan", "moroccan;arab", "moroccan;breakfast;lunch", "pizza-kebab", "pizza;grill;pasta;pita", "pizza;grill;pita", "pizza;kebab", "pizza;pasta;pita", "pizza;turkish", "sandwich;pasta;turkish", "syrian;lebanese", "turkish;pita;döner;fries", "döner", "persian;french", "chicken"),
  
  "European"= c("armenian", "balkan", "basque","bavarian","bourgondisch", "bistro", "breakfast", "british", "brunch", "bulgarian", "churro", "corsican", "creole","croatian", "danish", "dutch", "fish", "fondue", "french", "georgian","german", "greek", "hungarian", "italian","pizza", "luxembourgish", "pancake", "pasta", "patisserie","polish", "portuguese", "romanian", "russian", "scandinavian", "seafood", "spanish", "soup", "steak_house", "swiss", "tapas", "traditional", "italia;pizza", "mediterranean", "	barbecue", "grec"),
  
  "Belgian"= c("belgian","brasserie", "Brasserie", "croquettes", "crepes","flemish","fried_food","friterie",
               "friture","waffle","regional"),
  
  "Asian" = c("asian", "japanese", "ramen", "sushi", "korean","vietnamese", "thai", "indian", "cambodian","chinese","curry", "filipino", "indonesian", "malaysian", "nepalese", "noodle","ramen","tibetan","wok"),
  
  "American" = c("american","bagel", "burger","hawaiian","poke"),
  
  "Latin American" =  c("argentinian","empanada", "brasilian", "brazilian","caribbean","colombia","colombian", "latin_american", "mexican","nicaraguan","peruvian", "suriname", "tex-mex", "venezuelan"))


# Function to match keywords to groups
assign_group <- function(text, keyword_groups) {
  for (group in names(keyword_groups)) {
    if (any(grepl(paste(keyword_groups[[group]], collapse = "|"), text, ignore.case = TRUE))) {
      return(group)
    }
  }
  return("Other")
}

# Apply the function to each text
groups <- sapply(rest_points_overlayed_belgium_final$cuisine, assign_group, keyword_groups = keyword_groups)

# Combine text and groups

# Points 

rest_points_final_grouped <- data.frame(rest_points_overlayed_belgium_final, Group = groups)
table(rest_points_final_grouped$Group)
#View(rest_points_final_grouped[,c(1:3, 9,28)])

# Polygons

groups_rest <- sapply(rest_polygons_overlayed_belgium_final$cuisine, assign_group, keyword_groups = keyword_groups)
rest_polygons_overlayed_belgium_final_grouped <- data.frame(rest_polygons_overlayed_belgium_final, Group = groups_rest)
table(rest_polygons_overlayed_belgium_final_grouped$Group)
#View(rest_polygons_overlayed_belgium_final_grouped[,c(1:4, 10,29)])


################################################################################################################
# Fast-Food (Overall, 682 Islamic/Muslim Fast-Food Points)
################################################################################################################ 

fast_food <- getbb("Belgium") %>% # !!! mind this changes day-by-day as OSM gets updated
  opq() %>%
  add_osm_feature("amenity", "fast_food")

str(fast_food) 

fast_food_data <- osmdata_sf(fast_food)
fast_food_data

fast_food_points <- fast_food_data[["osm_points"]]
fast_food_polygons <- fast_food_data[["osm_polygons"]]

fast_food_points_final <- fast_food_points[, c("osm_id", "name", "cuisine","diet:halal", "geometry")] # 22223
fast_food_polygons_final <- fast_food_polygons[, c("osm_id", "name", "cuisine","diet:halal", "geometry")] # 1764

# write.csv(fast_food_points, "fast_food_points_2025_06.csv")
# write.csv(fast_food_polygons, "fast_food_polygons_2025_06.csv")

# Subset by Belgium

# Points

fast_food_points_final_upd <- sfc_as_cols(fast_food_points_final)
x_long = as.vector(fast_food_points_final_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(fast_food_points_final_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # overwrite CRS once again to be sure
pt.in.poly2_pois <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 

fast_food_points_final_upd_df <- as.data.frame(fast_food_points_final_upd)

fast_food_points_overlayed <- cbind(fast_food_points_final_upd_df, pt.in.poly2_pois)

fast_food_points_overlayed_belgium <- fast_food_points_overlayed[!is.na(fast_food_points_overlayed$CD_MUNTY_REFNIS), ] # rows 14802

# Polygons

fast_food_polygons_final$centroids <- st_centroid(fast_food_polygons_final$geometry)

fast_food_polygons_final_upd <- sfc_as_cols(fast_food_polygons_final, fast_food_polygons_final$centroids)

x_long = as.vector(fast_food_polygons_final_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(fast_food_polygons_final_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # set CRS 
pt.in.poly_ff <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 

fast_food_polygons_upd_df <- as.data.frame(fast_food_polygons_final_upd)

fast_food_polygons_overlayed <- cbind(fast_food_polygons_upd_df, pt.in.poly_ff)

fast_food_polygons_overlayed_belgium <- fast_food_polygons_overlayed[!is.na(fast_food_polygons_overlayed$CD_MUNTY_REFNIS), ] # rows 1304
 

# Subset by Cuisine (not NA)

fast_food_points_final_subset <- fast_food_points_overlayed_belgium[!is.na(fast_food_points_overlayed_belgium$cuisine),] # 4054
fast_food_polygons_final_subset <- fast_food_polygons_overlayed_belgium[!is.na(fast_food_polygons_overlayed_belgium$cuisine),] # 1079

# Combine text and groups

groups_ff <- sapply(fast_food_points_final_subset$cuisine, assign_group, keyword_groups = keyword_groups)
fast_food_points_final_grouped <- data.frame(fast_food_points_final_subset, Group = groups_ff)
table(fast_food_points_final_grouped$Group)

# Combine text and groups

groups_ff_poly <- sapply(fast_food_polygons_final_subset$cuisine, assign_group, keyword_groups = keyword_groups)
fast_food_polygons_final_subset_grouped <- data.frame(fast_food_polygons_final_subset, Group = groups_ff_poly)
table(fast_food_polygons_final_subset_grouped$Group)

################################################################################################################
# Cafe (Overall, 6 Islamic/Muslim Cafes)
################################################################################################################ 

 cafe <- getbb("Belgium") %>% # mind this changes day-by-day as OSM gets updated
   opq() %>%
   add_osm_feature("amenity", "cafe")
 
 str(cafe) 
 
cafe_data <- osmdata_sf(cafe)
cafe_data
 
cafe_points <- cafe_data[["osm_points"]]
 cafe_polygons <- cafe_data[["osm_polygons"]]
 
# write.csv(cafe_points, "cafe_points_2025_06.csv")
# write.csv( cafe_polygons, "cafe_polygons_2025_06.csv")
 
 cafe_points_final <- cafe_points[, c("osm_id", "name", "cuisine", "geometry")]
 cafe_polygons_final <- cafe_polygons[, c("osm_id", "name", "cuisine", "geometry")]

 # Subset by Belgium
 
 # Points
 
 cafe_points_final_upd <- sfc_as_cols(cafe_points_final)
 x_long = as.vector(cafe_points_final_upd$x) # set longitude vector for SpatialPoints format
 y_lat =  as.vector(cafe_points_final_upd$y) # set latitude vector for SpatialPoints format
 xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
 pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # overwrite CRS once again to be sure
 pt.in.poly2_pois <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 
 
 cafe_points_final_upd_df <- as.data.frame(cafe_points_final_upd)
 
 cafe_points_overlayed <- cbind(cafe_points_final_upd, pt.in.poly2_pois)
 
 cafe_points_overlayed_belgium <- cafe_points_overlayed[!is.na(cafe_points_overlayed$CD_MUNTY_REFNIS), ] # rows 10008
 
 # Polygons
 
 cafe_polygons_final$centroids <- st_centroid(cafe_polygons_final$geometry)
 
 cafe_polygons_final_upd <- sfc_as_cols(cafe_polygons_final, cafe_polygons_final$centroids)
 
 x_long = as.vector(cafe_polygons_final_upd$x) # set longitude vector for SpatialPoints format
 y_lat =  as.vector(cafe_polygons_final_upd$y) # set latitude vector for SpatialPoints format
 xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
 pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # set CRS 
 pt.in.poly_c <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 
 
 cafe_polygons_upd_df <- as.data.frame(cafe_polygons_final_upd)
 
 cafe_polygons_overlayed <- cbind(cafe_polygons_upd_df, pt.in.poly_c)
 
 cafe_polygons_overlayed_belgium <- cafe_polygons_overlayed[!is.na(cafe_polygons_overlayed$CD_MUNTY_REFNIS), ] # rows 772
 
 # Subset by Cuisine (not NA)
 
 cafe_points_final_subset <- cafe_points_overlayed_belgium[!is.na(cafe_points_overlayed_belgium$cuisine),] # 624
 cafe_polygons_final_subset <- cafe_polygons_overlayed_belgium[!is.na(cafe_polygons_overlayed_belgium$cuisine),] # 95
 
 # Combine text and groups
 
 groups_c <- sapply(cafe_points_final_subset$cuisine, assign_group, keyword_groups = keyword_groups)
 cafe_points_final_grouped <- data.frame(cafe_points_final_subset, Group = groups_c)
 table(cafe_points_final_grouped$Group)
 
 # Combine text and groups
 
 groups_c_poly <- sapply(cafe_polygons_final_subset$cuisine, assign_group, keyword_groups = keyword_groups)
 cafe_polygons_final_subset_grouped <- data.frame(cafe_polygons_final_subset, Group = groups_c_poly)
 table(cafe_polygons_final_subset_grouped$Group)
 
################################################################################################################ 
# Mosques ( Overall, 144 unique mosques)
################################################################################################################ 

mosque <- getbb("Belgium") %>% # mind this changes day-by-day as OSM gets updated
  opq() %>%
  add_osm_feature("building", "mosque")

str(mosque) 

mosque_data <- osmdata_sf(mosque)
mosque_data

mosque_points <- mosque_data[["osm_points"]]
mosque_polygons <- mosque_data[["osm_polygons"]]

# write.csv(mosque_points, "mosque_points_2025_06.csv")
# write.csv(mosque_polygons, "mosque_polygons_2025_06.csv")

mosque_points_final <- mosque_points[, c("osm_id", "name","religion", "geometry")]
mosque_polygons_final <- mosque_polygons[, c("osm_id", "name", "denomination","religion", "geometry")]

# Subset by Belgium

# Points

mosque_points_final_upd <- sfc_as_cols(mosque_points_final)
x_long = as.vector(mosque_points_final_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(mosque_points_final_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # overwrite CRS once again to be sure
pt.in.poly2_m <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 

mosque_points_final_upd_df <- as.data.frame(mosque_points_final_upd)

mosque_points_overlayed <- cbind(mosque_points_final_upd_df, pt.in.poly2_m)

mosque_points_overlayed_belgium <- mosque_points_overlayed[!is.na(mosque_points_overlayed$CD_MUNTY_REFNIS), ] # rows 14802 (a lot!!!)

leaflet(data = mosque_points_overlayed_belgium) %>%
  addTiles() %>%
  addMarkers(~x, ~y) # OSM ERROR HERE: Polygons are recorded as points 

# Polygons

mosque_polygons_final$centroids <- st_centroid(mosque_polygons_final$geometry)

mosque_polygons_final_upd <- sfc_as_cols(mosque_polygons_final, mosque_polygons_final$centroids)

x_long = as.vector(mosque_polygons_final_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(mosque_polygons_final_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # set CRS 
pt.in.poly_m <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 

mosque_polygons_upd_df <- as.data.frame(mosque_polygons_final_upd)

mosque_polygons_overlayed <- cbind(mosque_polygons_upd_df, pt.in.poly_m)

mosque_polygons_overlayed_belgium <- mosque_polygons_overlayed[!is.na(mosque_polygons_overlayed$CD_MUNTY_REFNIS), ] 

##################################################################################################################
# Keep only unique points: Polygons are stored as stand-alone points in the point data file (Unique POI are double, triple or quadruple counted)
################################################################################################################

# Run this and zoom over
# leaflet(data = mosque_points_overlayed_belgium) %>%
# addTiles() %>%
#   addMarkers(~x, ~y) # OSM ERROR HERE: Polygons are recorded as points 

# Truncate IDs to identify unique POI

mosque_points_overlayed_belgium$osm_id_truncated <- as.character(mosque_points_overlayed_belgium$osm_id)
mosque_points_overlayed_belgium$osm_id_truncated <- substr(mosque_points_overlayed_belgium$osm_id_truncated, 1, 4)
mosque_points_overlayed_belgium <- mosque_points_overlayed_belgium[, c(27,1:26)]

# Subset by unique points (combination of names and coordinates)

mosque_points_overlayed_belgium_a <- mosque_points_overlayed_belgium[!is.na(mosque_points_overlayed_belgium$name), ]
mosque_points_overlayed_belgium_a <- mosque_points_overlayed_belgium_a[c(5,9:17,21),]

# Subset by non-unique points 
mosque_points_overlayed_belgium_b <- mosque_points_overlayed_belgium[is.na(mosque_points_overlayed_belgium$name), ]

leaflet(data = mosque_points_overlayed_belgium_b) %>%
  addTiles() %>%
  addMarkers(~x, ~y) 

mosque_points_overlayed_belgium_b %>%
  group_by(TX_MUNTY_DESCR_NL) %>%
  summarise(unique_count = n_distinct(as.numeric(osm_id_truncated)))

# Pick the first row per group of TX_MUNTY_DESCR_NL and osm_id_truncated (e.g., one pair of longitude and latitude representative of a unique polygon, or unique POI)

mosque_points_overlayed_belgium_b_unique <- mosque_points_overlayed_belgium_b %>%
  group_by(TX_MUNTY_DESCR_NL, osm_id_truncated) %>%
  slice(1) %>%   # pick the first row per group of TX_MUNTY_DESCR_NL and osm_id_truncated
  ungroup()

# Merge two together back

mosque_points_overlayed_belgium_clean <- rbind(mosque_points_overlayed_belgium_a, mosque_points_overlayed_belgium_b_unique)

leaflet(data = mosque_points_overlayed_belgium_clean) %>%
  addTiles() %>%
  addMarkers(~x, ~y) 

# Merge together w/ the mosque polygons

mosques_all <- rbind(mosque_polygons_overlayed_belgium[, -c(3,8)], mosque_points_overlayed_belgium_clean[, -c(1)])

leaflet(data = mosques_all) %>%
  addTiles() %>%
  addMarkers(~x, ~y) # A lot of duplication again

# De-duplicate again: solution by provided ChatGPT, works good

# Remove similar long and lat points within 10-m. 

# Convert mosques_all to an sf object (WGS84 CRS)
mosques_sf <- st_as_sf(mosques_all, coords = c("x", "y"), crs = 4326)

# Transform to a projected CRS for accurate meter-based distances (EPSG:3857)
mosques_proj <- st_transform(mosques_sf, crs = 3857)

# Identify all point pairs within 10 meters
dupes_list <- st_is_within_distance(mosques_proj, mosques_proj, dist = 10)

# Build graph edges from the neighbor list
edges <- do.call(rbind, lapply(seq_along(dupes_list), function(i) {
  neighbors <- dupes_list[[i]]
  neighbors <- neighbors[neighbors != i]  # exclude self
  if (length(neighbors) == 0) return(NULL)
  cbind(rep(i, length(neighbors)), neighbors)
}))

# If there are no duplicates, keep original data
if (is.null(edges)) {
  unique_mosques <- mosques_all
} else {
  # Create graph and find connected components (clusters of near-duplicates)
  g <- graph_from_edgelist(edges, directed = FALSE)
  comp <- components(g)$membership
  
  # Assign group labels to original data
  mosques_proj$dup_group <- NA_integer_
  mosques_proj$dup_group[as.integer(names(comp))] <- comp
  
  # Keep one representative point per duplicate group (or all unique)
  unique_rows <- !duplicated(mosques_proj$dup_group) | is.na(mosques_proj$dup_group)
  mosques_unique <- mosques_proj[unique_rows, ]
  
  # Convert back to original CRS (WGS84) and extract coordinates
  mosques_unique <- st_transform(mosques_unique, crs = 4326)
  coords <- st_coordinates(mosques_unique)
  mosques_clean <- cbind(
    st_drop_geometry(mosques_unique),
    x = coords[, "X"],
    y = coords[, "Y"]
  )
}

# mosques_unique is the pre-processed object without incorrectly documented POIs and duplicates.

################################################################################################################ 
# Geocode Google Maps Addresses: Halal Stores, Islamic Centers, Islamic Schools and Islamic Bookstores
################################################################################################################

# Halal_Stores <- read_excel()
# Islamic_Education_Religion <- read_excel()

################################################################################################################
# Halal_Stores: Geocode addresses via Nominatim, OSM
################################################################################################################

geocoded <- as_tibble(Halal_Stores) %>%
  tidygeocoder::geocode(address = Address, method = "osm")

geocoded_halal_na <- geocoded[is.na(geocoded$lat),]
geocoded_halal <- geocoded[!is.na(geocoded$lat),]

# Standardize addresses for successful geocoding 

geocoded_halal_na$Address <- gsub("\\bChau\\.", "Chaussée", geocoded_halal_na$Address)
geocoded_halal_na$Address <- gsub("\\bAvenue\\b", "Av.", geocoded_halal_na$Address)
geocoded_halal_na$Address <-gsub("\\bBlvd\\b", "Boulevard", geocoded_halal_na$Address)
geocoded_halal_na$Address <- gsub("\\bPl\\.\\b", "Place", geocoded_halal_na$Address)
geocoded_halal_na$Address <- gsub("\\bBd\\b", "Boulevard", geocoded_halal_na$Address)

geocoded_halal_na$Address[c(21, 22, 30, 36, 37)] <- c("Sint-Gummarusstraat 20-22, 2060 Antwerpen", 
                                       "Molenstraat 6, 9160 Lokeren", 
                                       "Boulevard de la Fleur de Lys 20, 1400 Nivelles",
                                       "Rue Vinâve 3, 4030 Liège",
                                       "Ransfortstraat 18, 1080 Sint-Jans-Molenbeek")

geocoded_halal_na$Address[c(2, 11, 24)] <-c("Place Général McAuliffe 5, 6600 Bastogne",
                                            "Av. H. Conscience 143, 1140 Evere",
                                            "Rue des Clairisses 14/3, 7500 Tournai")

geocoded_na <- as_tibble(geocoded_halal_na[,-c(4:5)]) %>%
  tidygeocoder::geocode(address = Address, method = "osm")

# Fill in coordinates manually where non-geocoded

rows_to_fill <- c(1, 2, 11, 20, 23, 32, 33)
na_rows <- rows_to_fill[is.na(geocoded_na$lat[rows_to_fill])]
geocoded_na$lat[na_rows] <- c(49.5667,50.0025, 50.8466, 50.7057, 50.75, 50.4389, 50.4416843)
na_rows2 <- rows_to_fill[is.na(geocoded_na$long[rows_to_fill])]
geocoded_na$long[na_rows2] <-  c(5.5333,5.7792,4.3528,4.7484,3.6,4.6056, 4.8336903)
geocoded_na <- geocoded_na[-34,] # duplicate

# Merge NA and Non-NA dataset

geocoded_halal_stores <-rbind(geocoded_halal,geocoded_na)

leaflet(data = geocoded_halal_stores) %>%
  addTiles() %>%
  addMarkers(~long, ~lat) #

# write.csv(geocoded_halal_stores, "geocoded_halal_stores_2025_06.csv")

################################################################################################################
# Islamic Centers, Islamic Schools and Islamic Bookstores: : Geocode addresses via Nominatim, OSM
################################################################################################################

# Standardize addresses for successful geocoding

Islamic_Education_Religion$Address <- gsub("\\bChau\\.", "Chaussée", Islamic_Education_Religion$Address)
Islamic_Education_Religion$Address <- gsub("\\bAvenue\\b", "Av.", Islamic_Education_Religion$Address)
Islamic_Education_Religion$Address <-gsub("\\bBlvd\\b", "Boulevard", Islamic_Education_Religion$Address)
Islamic_Education_Religion$Address <- gsub("\\bPl\\.\\b", "Place", Islamic_Education_Religion$Address)
Islamic_Education_Religion$Address <- gsub("\\bBd\\b", "Boulevard", Islamic_Education_Religion$Address)

geocoded_edu <- as_tibble(Islamic_Education_Religion) %>%
  tidygeocoder::geocode(address = Address, method = "osm")

# Fill in coordinates manually where non-geocoded

rows_to_fill <- 38
na_rows <- rows_to_fill[is.na(geocoded_edu$lat[rows_to_fill])]
geocoded_edu$lat[na_rows] <- c(50.5333)
na_rows2 <- rows_to_fill[is.na(geocoded_edu$long[rows_to_fill])]
geocoded_edu$long[na_rows2] <-  c(4.75)


leaflet(data = geocoded_edu) %>%
  addTiles() %>%
  addMarkers(~long, ~lat) 

# write.csv(geocoded_edu, "geocoded_islamic_edu_2025_06.csv")

################################################################################################################
# Augment with Admin Geographies
################################################################################################################

x_long = as.vector(geocoded_halal_stores$long) # set longitude vector for SpatialPoints format
y_lat =  as.vector(geocoded_halal_stores$lat) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # overwrite CRS once again to be sure
pt.in.poly2_halal <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 

halal_stores_points_overlayed <- cbind(as.data.frame(geocoded_halal_stores), pt.in.poly2_halal)

x_long = as.vector(geocoded_edu$long) # set longitude vector for SpatialPoints format
y_lat =  as.vector(geocoded_edu$lat) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_MUNTY))) # overwrite CRS once again to be sure
pt.in.poly2_islam_edu <- sp::over(pts, BE_ADMIN_MUNTY, fn = NULL) 

islam_edu_overlayed <- cbind(as.data.frame(geocoded_edu), pt.in.poly2_islam_edu)

################################################################################################################
# Subset by Islamic Cuisine and Merge all
################################################################################################################

vars <- c("osm_id", "name", "cuisine", "x", "y", 
          "CD_MUNTY_REFNIS", "TX_MUNTY_DESCR_NL", "TX_MUNTY_DESCR_FR",
          "CD_DSTR_REFNIS", "TX_ADM_DSTR_DESCR_NL", "TX_ADM_DSTR_DESCR_FR",
          "CD_PROV_REFNIS", "TX_PROV_DESCR_NL", "TX_PROV_DESCR_FR",
          "CD_RGN_REFNIS", "TX_RGN_DESCR_NL", "TX_RGN_DESCR_FR", "Group")

vars2 <- c("osm_id", "name",
          "CD_MUNTY_REFNIS", "TX_MUNTY_DESCR_NL", "TX_MUNTY_DESCR_FR",
          "CD_DSTR_REFNIS", "TX_ADM_DSTR_DESCR_NL", "TX_ADM_DSTR_DESCR_FR",
          "CD_PROV_REFNIS", "TX_PROV_DESCR_NL", "TX_PROV_DESCR_FR",
          "CD_RGN_REFNIS", "TX_RGN_DESCR_NL", "TX_RGN_DESCR_FR")

vars3 <- c("Placename", "Type", "lat", "long",
           "CD_MUNTY_REFNIS", "TX_MUNTY_DESCR_NL", "TX_MUNTY_DESCR_FR",
           "CD_DSTR_REFNIS", "TX_ADM_DSTR_DESCR_NL", "TX_ADM_DSTR_DESCR_FR",
           "CD_PROV_REFNIS", "TX_PROV_DESCR_NL", "TX_PROV_DESCR_FR",
           "CD_RGN_REFNIS", "TX_RGN_DESCR_NL", "TX_RGN_DESCR_FR")

rest_Islamic_1 <- rest_points_final_grouped %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

rest_Islamic_1$Type <- "Restaurant" # !save

rest_Islamic_2 <- rest_polygons_overlayed_belgium_final_grouped %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

rest_Islamic_2$Type <- "Restaurant" # !save

ff_Islamic_1 <- fast_food_points_final_grouped  %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

ff_Islamic_1$Type <- "Fast-Food" # !save

ff_Islamic_2 <- fast_food_polygons_final_subset_grouped %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

ff_Islamic_2$Type <- "Fast-Food" # !save

c_Islamic_1 <- cafe_points_final_grouped   %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

c_Islamic_1$Type <- "Cafe" # !save

c_Islamic_2 <- cafe_polygons_final_subset_grouped %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

c_Islamic_2$Type <- "Cafe" # !save

mosques_final <- mosques_unique %>% # !save
  dplyr::select(all_of(vars2))

halal_stores <- halal_stores_points_overlayed %>% # !save
  dplyr::select(all_of(vars3))

islam_edu_final <- islam_edu_overlayed %>% # !save
  dplyr::select(all_of(vars3))



Islam_Food_Establishments <- rbind(rest_Islamic_1, rest_Islamic_2, # !save
                                   ff_Islamic_1, ff_Islamic_2,
                                   c_Islamic_1, c_Islamic_2)


# write.csv(rest_Islamic_1, "rest_Islamic_1_municipality_2025_06.csv")
# write.csv(rest_Islamic_2, "rest_Islamic_2_municipality_2025_06.csv")
# write.csv(ff_Islamic_1, "ff_Islamic_1_municipality_2025_06.csv")
# write.csv(ff_Islamic_2, "ff_Islamic_2_municipality_2025_06.csv")
# write.csv(c_Islamic_1, "c_Islamic_1_municipality_2025_06.csv")
# write.csv(c_Islamic_2, "c_Islamic_2_municipality_2025_06.csv")
# write.csv(mosques_final, "mosques_final_municipality_2025_06.csv")
# write.csv(halal_stores, "halal_stores_municipality_2025_06.csv")
# write.csv(islam_edu_final, "islam_edu_final_municipality_2025_06.csv")
# write.csv(Islam_Food_Establishments, "Islam_Food_Establishments_municipality_2025_06.csv")

################################################################################################################
# Summarize All by Municipality: Produce Grouped Counts
################################################################################################################

Islam_Food_Establishments_grouped <- Islam_Food_Establishments %>% # !save
  group_by(CD_MUNTY_REFNIS, TX_MUNTY_DESCR_NL, Type)%>% 
  summarise(n_food_establishments_grouped= n())

Islam_Food_Establishments_overall <- Islam_Food_Establishments %>% # !save
  group_by(CD_MUNTY_REFNIS, TX_MUNTY_DESCR_NL)%>% 
  summarise(n_food_establishments = n())

Mosques_mun <- mosques_final %>% # !save
  group_by(CD_MUNTY_REFNIS, TX_MUNTY_DESCR_NL)%>% 
  summarise(mosque = n())
  
Halal_stores_mun <-  halal_stores %>% # !save
  group_by(CD_MUNTY_REFNIS, TX_MUNTY_DESCR_NL)%>% 
  summarise(halal_store = n())
  
Islam_edu_mun <-  islam_edu_final %>% # !save
  group_by(CD_MUNTY_REFNIS, TX_MUNTY_DESCR_NL)%>% 
  summarise(islam_edu= n())

 # write.csv(Islam_Food_Establishments_grouped, "Islam_Food_Establishments_summarized_by_type_and_municipality_2025_06.csv")
 # write.csv(Islam_Food_Establishments_overall, "Islam_Food_Establishments_summarized_by_municipality_2025_06.csv")
 # write.csv(Mosques_mun, "mosques_summarized_by_municipality_2025_06.csv")
 # write.csv(Halal_stores_mun, "halal_stores_summarized_by_municipality_2025_06.csv")
 # write.csv(Islam_edu_mun, "islam_edu_summarized_by_municipality_2025_06.csv")


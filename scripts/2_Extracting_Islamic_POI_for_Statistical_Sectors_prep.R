#############################################################################################################
#############################################################################################################

## ---------------------------
##
## OSM and Google Maps: Extracting Islamic POI for Belgian Statistical Sectors
##
## Author: Dra. Daria Dementeva
##
##
## Email: daria.dementeva@kuleuven.be
##
## ---------------------------
##
##   2024-2025
##
## ---------------------------

## Set Working Directory 

# setwd("...")  

# For Reproducibility

set.seed(05081997)

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
# data("BE_ADMIN_MUNTY")
data("BE_ADMIN_SECTORS")

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

################################################################################################################  
# 1.  Read in Data
################################################################################################################ 

# Paper4_3_6_25.RData
# geocoded_halal_stores_2025_06.csv
# geocoded_islamic_edu_2025_06.csv

################################################################################################################  
# 2.  Overlay onto Statistical Sectors
################################################################################################################

################################################################################################################  
# 2a.  Restaurants
################################################################################################################

# Points

rest_points <- rest_data[["osm_points"]]
rest_polygons <- rest_data[["osm_polygons"]]

rest_points_final <- rest_points[, c("osm_id", "name", "cuisine","diet:halal", "geometry")]
nrow(rest_points_final) # 55453
rest_polygons_final <- rest_polygons[, c("osm_id", "name", "cuisine","diet:halal", "geometry")]
nrow(rest_polygons_final) # 3773

rest_points_upd <- sfc_as_cols(rest_points_final)
x_long = as.vector(rest_points_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(rest_points_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly2_pois <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

rest_points_upd_df <- as.data.frame(rest_points_upd)

rest_points_overlayed <- cbind(rest_points_upd_df, pt.in.poly2_pois)

rest_points_overlayed_belgium <- rest_points_overlayed[!is.na(rest_points_overlayed$CD_REFNIS_SECTOR), ] # rows 36767

# Polygons

rest_polygons_final$centroids <- st_centroid(rest_polygons_final$geometry)

rest_polygons_upd <- sfc_as_cols(rest_polygons_final, rest_polygons_final$centroids)

x_long = as.vector(rest_polygons_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(rest_polygons_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # set CRS 
pt.in.poly_rest <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

rest_polygons_upd_df <- as.data.frame(rest_polygons_upd)

rest_polygons_overlayed <- cbind(rest_polygons_upd_df, pt.in.poly_rest)

rest_polygons_overlayed_belgium <- rest_polygons_overlayed[!is.na(rest_polygons_overlayed$CD_REFNIS_SECTOR), ] # rows 2605

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
# 2b.  Fast Food
################################################################################################################

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
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly2_pois <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

fast_food_points_final_upd_df <- as.data.frame(fast_food_points_final_upd)

fast_food_points_overlayed <- cbind(fast_food_points_final_upd_df, pt.in.poly2_pois)

fast_food_points_overlayed_belgium <- fast_food_points_overlayed[!is.na(fast_food_points_overlayed$CD_REFNIS_SECTOR), ] # rows 14802

# Polygons

fast_food_polygons_final$centroids <- st_centroid(fast_food_polygons_final$geometry)

fast_food_polygons_final_upd <- sfc_as_cols(fast_food_polygons_final, fast_food_polygons_final$centroids)

x_long = as.vector(fast_food_polygons_final_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(fast_food_polygons_final_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # set CRS 
pt.in.poly_ff <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

fast_food_polygons_upd_df <- as.data.frame(fast_food_polygons_final_upd)

fast_food_polygons_overlayed <- cbind(fast_food_polygons_upd_df, pt.in.poly_ff)

fast_food_polygons_overlayed_belgium <- fast_food_polygons_overlayed[!is.na(fast_food_polygons_overlayed$CD_REFNIS_SECTOR), ] # rows 1304

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
# 2c. Cafe (Overall, 6 Islamic/Muslim Cafes)
################################################################################################################ 

cafe_points <- cafe_data[["osm_points"]]
cafe_polygons <- cafe_data[["osm_polygons"]]

cafe_points_final <- cafe_points[, c("osm_id", "name", "cuisine", "geometry")]
cafe_polygons_final <- cafe_polygons[, c("osm_id", "name", "cuisine", "geometry")]

# Subset by Belgium

# Points

cafe_points_final_upd <- sfc_as_cols(cafe_points_final)
x_long = as.vector(cafe_points_final_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(cafe_points_final_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly2_pois <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

cafe_points_final_upd_df <- as.data.frame(cafe_points_final_upd)

cafe_points_overlayed <- cbind(cafe_points_final_upd, pt.in.poly2_pois)

cafe_points_overlayed_belgium <- cafe_points_overlayed[!is.na(cafe_points_overlayed$CD_REFNIS_SECTOR), ] # rows 10008

# Polygons

cafe_polygons_final$centroids <- st_centroid(cafe_polygons_final$geometry)

cafe_polygons_final_upd <- sfc_as_cols(cafe_polygons_final, cafe_polygons_final$centroids)

x_long = as.vector(cafe_polygons_final_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(cafe_polygons_final_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # set CRS 
pt.in.poly_c <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

cafe_polygons_upd_df <- as.data.frame(cafe_polygons_final_upd)

cafe_polygons_overlayed <- cbind(cafe_polygons_upd_df, pt.in.poly_c)

cafe_polygons_overlayed_belgium <- cafe_polygons_overlayed[!is.na(cafe_polygons_overlayed$CD_REFNIS_SECTOR), ] # rows 772

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
# 2d. Mosques
################################################################################################################ 

data_pofw <- st_read("/Users/dariadementeva/Desktop/for_powf_be_latest.shp",
                     layer = 'gis_osm_pofw_a_free_1')

mosques_all_osm <- data_pofw[data_pofw$fclass %in% c("muslim", "muslim_sunni"), ]

mosques_all_osm$centroids <- st_centroid(mosques_all_osm$geometry)

mosques_all_osm_final_upd <- sfc_as_cols(mosques_all_osm, mosques_all_osm$centroids)

x_long = as.vector(mosques_all_osm_final_upd$x) # set longitude vector for SpatialPoints format
y_lat =  as.vector(mosques_all_osm_final_upd$y) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # set CRS 
pt.in.poly_c <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

mosques_all_upd_df <- as.data.frame(mosques_all_osm_final_upd)

mosques_overlayed <- cbind(mosques_all_upd_df , pt.in.poly_c)


################################################################################################################
# 2e. Halal Stores
################################################################################################################ 

# geocoded_halal_stores_2025_06 <- read_csv("Dropbox/Мой Mac (MacBook Pro — Daria)/Desktop/PhD/Paper4/Data_Files/Geocoded_GoogleMaps/geocoded_halal_stores_2025_06.csv")

geocoded_halal_stores<- geocoded_halal_stores_2025_06 

x_long = as.vector(geocoded_halal_stores$long) # set longitude vector for SpatialPoints format
y_lat =  as.vector(geocoded_halal_stores$lat) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly2_halal <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

halal_stores_points_overlayed <- cbind(as.data.frame(geocoded_halal_stores), pt.in.poly2_halal)

################################################################################################################
# 2e. Islamic Education
################################################################################################################ 

# geocoded_islamic_edu_2025_06 <- read_csv("Dropbox/Мой Mac (MacBook Pro — Daria)/Desktop/PhD/Paper4/Data_Files/Geocoded_GoogleMaps/geocoded_islamic_edu_2025_06.csv")

geocoded_edu <- geocoded_islamic_edu_2025_06

x_long = as.vector(geocoded_edu$long) # set longitude vector for SpatialPoints format
y_lat =  as.vector(geocoded_edu$lat) # set latitude vector for SpatialPoints format
xy = cbind(x_long,y_lat) # create a separate matrix/data frame for further processing
pts = SpatialPoints(xy, proj4string = CRS(proj4string(BE_ADMIN_SECTORS))) # overwrite CRS once again to be sure
pt.in.poly2_islam_edu <- sp::over(pts, BE_ADMIN_SECTORS, fn = NULL) 

islam_edu_overlayed <- cbind(as.data.frame(geocoded_edu), pt.in.poly2_islam_edu)


################################################################################################################
# 3. Subset by Islamic Cuisine and Merge all
################################################################################################################

vars <- c("osm_id", "name", "cuisine", "x", "y", 
          "CD_REFNIS_SECTOR", "CD_SECTOR", "TX_SECTOR_DESCR_NL", "TX_SECTOR_DESCR_FR",
          "CD_MUNTY_REFNIS", "TX_MUNTY_DESCR_NL", "TX_MUNTY_DESCR_FR",
          "CD_DSTR_REFNIS", "TX_ADM_DSTR_DESCR_NL", "TX_ADM_DSTR_DESCR_FR",
          "CD_PROV_REFNIS", "TX_PROV_DESCR_NL", "TX_PROV_DESCR_FR",
          "CD_RGN_REFNIS", "TX_RGN_DESCR_NL", "TX_RGN_DESCR_FR", "Group")

vars2 <- c("osm_id", "name",
           "CD_REFNIS_SECTOR", "CD_SECTOR", "TX_SECTOR_DESCR_NL", "TX_SECTOR_DESCR_FR",
           "CD_MUNTY_REFNIS", "TX_MUNTY_DESCR_NL", "TX_MUNTY_DESCR_FR",
           "CD_DSTR_REFNIS", "TX_ADM_DSTR_DESCR_NL", "TX_ADM_DSTR_DESCR_FR",
           "CD_PROV_REFNIS", "TX_PROV_DESCR_NL", "TX_PROV_DESCR_FR",
           "CD_RGN_REFNIS", "TX_RGN_DESCR_NL", "TX_RGN_DESCR_FR")

vars3 <- c("Placename", "Type", "lat", "long",
           "CD_REFNIS_SECTOR", "CD_SECTOR", "TX_SECTOR_DESCR_NL", "TX_SECTOR_DESCR_FR",
           "CD_MUNTY_REFNIS", "TX_MUNTY_DESCR_NL", "TX_MUNTY_DESCR_FR",
           "CD_DSTR_REFNIS", "TX_ADM_DSTR_DESCR_NL", "TX_ADM_DSTR_DESCR_FR",
           "CD_PROV_REFNIS", "TX_PROV_DESCR_NL", "TX_PROV_DESCR_FR",
           "CD_RGN_REFNIS", "TX_RGN_DESCR_NL", "TX_RGN_DESCR_FR")

rest_Islamic_1 <- rest_points_final_grouped %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

rest_Islamic_1$Type <- "Restaurant"  

rest_Islamic_2 <- rest_polygons_overlayed_belgium_final_grouped %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

rest_Islamic_2$Type <- "Restaurant"  

ff_Islamic_1 <- fast_food_points_final_grouped  %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

ff_Islamic_1$Type <- "Fast-Food"  

ff_Islamic_2 <- fast_food_polygons_final_subset_grouped %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

ff_Islamic_2$Type <- "Fast-Food"  

c_Islamic_1 <- cafe_points_final_grouped   %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

c_Islamic_1$Type <- "Cafe" 

c_Islamic_2 <- cafe_polygons_final_subset_grouped %>% 
  dplyr::filter(Group == "Islamic") %>%
  dplyr::select(all_of(vars))

c_Islamic_2$Type <- "Cafe" 

Islam_Food_Establishments <- rbind(rest_Islamic_1, rest_Islamic_2, 
                                   ff_Islamic_1, ff_Islamic_2,
                                   c_Islamic_1, c_Islamic_2)

mosques_final <- mosques_overlayed %>%  
  dplyr::select(all_of(vars2))

halal_stores <- halal_stores_points_overlayed %>%  
  dplyr::select(all_of(vars3))

islam_edu_final <- islam_edu_overlayed %>%  
  dplyr::select(all_of(vars3))


################################################################################################################
# Summarize All by SS: Produce Grouped Counts
################################################################################################################

Islam_Food_Establishments_grouped <- Islam_Food_Establishments %>% # 
  group_by(CD_REFNIS_SECTOR, TX_SECTOR_DESCR_NL, Type)%>% 
  summarise(n_food_establishments_grouped= n())

Islam_Food_Establishments_overall <- Islam_Food_Establishments %>% # 
  group_by(CD_REFNIS_SECTOR, TX_SECTOR_DESCR_NL)%>% 
  summarise(n_food_establishments = n())

Mosques_ss <- mosques_final %>% #
  group_by(CD_REFNIS_SECTOR, TX_SECTOR_DESCR_NL)%>% 
  summarise(mosque = n())

Halal_stores_ss <-  halal_stores %>% # 
  group_by(CD_REFNIS_SECTOR, TX_SECTOR_DESCR_NL)%>% 
  summarise(halal_store = n())

Islam_edu_ss <-  islam_edu_final %>% # 
  group_by(CD_REFNIS_SECTOR, TX_SECTOR_DESCR_NL)%>% 
  summarise(islam_edu= n())


write.csv(Islam_Food_Establishments_grouped, "Islam_Food_Establishments_summarized_by_type_and_ss_2025_06.csv")
write.csv(Islam_Food_Establishments_overall, "Islam_Food_Establishments_summarized_by_ss_2025_06.csv")
write.csv(Mosques_mun, "mosques_summarized_by_ss_2025_06.csv")
write.csv(Halal_stores_mun, "halal_stores_summarized_by_ss_2025_06.csv")
write.csv(Islam_edu_mun, "islam_edu_summarized_by_ss_2025_06.csv")


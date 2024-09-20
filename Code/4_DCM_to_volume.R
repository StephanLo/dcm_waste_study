## DCM script

library(terra)
source("Code/0_helper_functions.R")


#DCM has change map, date of pixel change map and quality/accuracy layers.

## --------------------------------Input--------------------------------- ##

## the site data
site_locations <- vect("Data/Landfill_locations/final_sites.gpkg", layer = "buffer")
DCM_productnames <- vect("data/aux_spatial_data/DCM_grid.gpkg")


## DCM data location
data_folder <- "DCM/download_folder"  #

## all the names.
names_aois <- site_locations$name

#_____________________________________________________________________________#

for(i in 1:100){
  aoi <- site_locations[i,]


# ----------------------------getting files --------------------------------- #
intersecting_dcm_products <- terra::intersect(aoi, DCM_productnames)
product_name <- intersecting_dcm_products$DCM_name

parent_1 <- paste0(data_folder, product_name,"_FIRST1622_V01_C")
parent_2 <- paste0(data_folder, product_name,"_LAST1622_V01_C")

# DCM is change from EDEM until the date in date_ras. 
path_dcm_1 <- list.files(path = paste0(parent_1,"/DCM"),
                         full.names = TRUE,
                         pattern = ".tif$")

path_dcm_2 <- list.files(path = paste0(parent_2,"/DCM"),
                         full.names = TRUE,
                         pattern = ".tif$")

#Acquisition Dates (DATE): this layer gives the acquisition date of the used
#scene for each pixel. The Date is given in integer format YYYYMMDD.
path_date_1 <- list.files(path = paste0(parent_1,"/DCM_AUXFILES"),
                          full.names = TRUE,
                          pattern = "DATE")
path_date_2 <- list.files(path = paste0(parent_2,"/DCM_AUXFILES"),
                          full.names = TRUE,
                          pattern = "DATE")


#Change Indication Mask (CIM): shows where the DCM processor found changes
#between the mosaic of the DEM scenes from the new acquisitions and the
#reference DEM (TanDEM-X 30m EDEM). It is meant to provide information on
#possible terrain changes and their reliability based on the local properties of
#the TanDEM-X 30m EDEM and the new DEM data and the HAI. Note that the CIM does
#not replace a thorough temporal height change analysis.
path_cim_1 <- list.files(path = paste0(parent_1,"/DCM_AUXFILES"),
                         full.names = TRUE,
                         pattern = "CIM")
path_cim_2 <- list.files(path = paste0(parent_2,"/DCM_AUXFILES"),
                         full.names = TRUE,
                         pattern = "CIM")

#Height Accuracy Indication (HAI): approximates the resulting height error of
#the combination of the Height Error Map values (HEM) of the new DEM data and
#the reference TanDEM-X 30m EDEM assuming both height errors (HEMs) follow a
#Gaussian distribution.
path_hai_1 <- list.files(path = paste0(parent_1,"/DCM_AUXFILES"),
                         full.names = TRUE,
                         pattern = "HAI")
path_hai_2 <- list.files(path = paste0(parent_2,"/DCM_AUXFILES"),
                         full.names = TRUE,
                         pattern = "HAI")

#----------------------------------loading files ------------------------------#
# in the case where a site covers more than one tile, it is necessary to read the 
# individual tiles into a spatialrasterCollection (sprc) and then mosaic the 
# rasters together
 
# dcm
dcm_1_collection <- lapply(path_dcm_1, FUN = rast) |> sprc()
dcm_2_collection <- lapply(path_dcm_2, FUN = rast) |> sprc()

dcm_1_full <- mosaic(dcm_1_collection)
dcm_2_full <- mosaic(dcm_2_collection)

# date
date_1_collection <- lapply(path_date_1, FUN = rast) |> sprc()
date_2_collection <- lapply(path_date_2, FUN = rast) |> sprc()

date_1_full <- mosaic(date_1_collection)
date_2_full <- mosaic(date_2_collection)
# HAI
hai_1_collection <- lapply(path_hai_1, FUN = rast) |> sprc()
hai_2_collection <- lapply(path_hai_2, FUN = rast) |> sprc()

hai_1_full <- mosaic(hai_1_collection)
hai_2_full <- mosaic(hai_2_collection)


## Load them: 
dcm_full <- c(dcm_1_full,dcm_2_full) 
date_full <- c(date_1_full,date_2_full)
hai_full <- c(hai_1_full,hai_2_full)

## Getting to rate / i.e., considering the time of observation.

fun_n_days <- function(base_date, x_date){
  date_0 <- as.Date(as.character(base_date), format = "%Y %m %d")
  date_1 <- as.Date(as.character(x_date), format = "%Y %m %d")
  difference_days <- date_1 - date_0 # time in days between 2 dates
  return(as.numeric(difference_days)) # get it as a number
}

# 1 crop to AOI
dcm_aoi <- crop(dcm_full,aoi)
time_aoi <- crop(date_full, aoi)
hai_aoi <- crop(hai_full,aoi)

# 2 work out date diff
days_aoi <- lapp(time_aoi, fun_n_days)


# 3 Get velocity of height change by dividing DCM by date diff
# change between second and first observation.
dcm_change <- dcm_aoi[[2]] - dcm_aoi[[1]]

velocity_aoi <- dcm_change/days_aoi

#3 Save
outrast <- c(velocity_aoi,time_aoi, hai_aoi,days_aoi,dcm_aoi[[1]], dcm_aoi[[2]])
names(outrast) <- c("velocity_m_day", "first_dates", "last_dates", "reliability_hai_first", "reliability_hai_last", "n_days","dcm_1", "dcm_2")
plot(outrast)
output_name <- paste0(aoi$ID,"_site_data.tif")

writeRaster(outrast, filename = file.path("Processing","sites", output_name), overwrite = T)
}



library(terra)
library(sf)
library(dplyr)
source("Code/0_helper_functions.R")


# make a 1 degree world grid:
world_grid <- rast(nrows = 180,
                   ncols = 360,
                   xmin=-180, xmax=180, 
                   ymin=-90, ymax=90,
                   vals = paste("cell",1:(180*360),sep = "_"),
                   crs = "epsg:4326")

poly_grid <- as.polygons(world_grid) |> st_as_sf()

grid_centroids <- st_centroid(poly_grid) 


poly_grid$DCM_name <-  sapply(grid_centroids$geometry, FUN = productname_function)

## Get the URLs of the DCM
all_URLs <-  sapply(grid_centroids$geometry, FUN = DCM_URL_function)

# this is a dimention (2, 64800) matrix.   
poly_grid$first_DCM_URL <- all_URLs[1,]
poly_grid$last_DCM_URL <- all_URLs[2,]


st_write(poly_grid, "Data/aux_spatial_data/DCM_grid.gpkg")

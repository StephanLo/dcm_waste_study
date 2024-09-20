## Rate of volume change (added?) in landfill site, if 2 DEMs supplied
# update v4: include pixel level change threshold based on error in pixel.



library(openxlsx)
library(terra)
library(data.table)
source("Code/0_helper_functions.R")
library(sf)

#DCM has change map, date of pixel change map and quality/accuracy layers.

## --------------------------------  Input  --------------------------------- ##
path_sites <- list.files(path = file.path("Processing", "sites"),
                         full.names = TRUE,
                         pattern = ".tif")

## the site data
path_aoi <- "data/final_sites.gpkg"
## EPSG code dataframe, needed for finding correct local projection.
epsg_codes <- read.xlsx("Data/EPSG/UTM_epsg.xlsx")

####### Optional Parameters: ##################################

# percentile outlier removal (winsorizing https://en.wikipedia.org/wiki/Winsorizing)
winsor_filter <- FALSE
winsor_thresh <- c(0.01, 0.99)  # percentiles (this leads to drop of 2% of data)

# median filter (exp. FALSE default)
median_filter <- FALSE
# temporal mask
time_mask <- TRUE
days_thresh <- 2  # days between observations

## HAI filter:
# This filter masks pixels where the DCM change is smaller than the estimated error in the pixel.
hai_filter <- T



##############################################################


locations <- vect(path_aoi) 
is.valid(locations)

# dataframe of sites and the volume rate in the site, 
# We iterate over sites and append results to this df.
sites_summary <- data.frame()


## Loop through all the products.
for (site in path_sites){

  print(paste("now running:", site))
  ## read DEM change velocity
  site_rast <- rast(site)

  # get AOI
  #Target sites
  site_ext <- ext(site_rast) # get spatial extent of raster
  target_site <- terra::intersect(locations,site_ext) #intersect database of landfill locations with the DCM extent 
  print(paste("which has AOI:", target_site$name))
  if(nrow(target_site) >1){
    print("there were more than one aois reported. choosing first.")
    target_site <- target_site[1]
  }
  
  ## Choose correct UTM zone ##
  aoi_middle <- centroids(target_site) |> crds() # get coordinate of centroid
  utm_string <- get_UTMname(aoi_middle[1], aoi_middle[2]) # Get the UTM zone name using our custom function
  epsg_number <- epsg_codes$CODE[grepl(pattern = utm_string,  # search epsg database for the matching epsg code
                                       x = epsg_codes$NAME, 
                                       fixed = TRUE)]
  epsg_string <- paste0("epsg:",epsg_number)
  print(paste("projection:", epsg_string))
  print(paste("The country:", target_site$NAME_EN))
  # Project the raster and the vector.
  utm_rast <- project(site_rast, epsg_string)
  utm_aoi  <- project(target_site, epsg_string)  
  dates_unmasked <- utm_rast[[c("first_dates","last_dates","n_days")]]
  
  ## for winsor filter:
  if(winsor_filter){
    winsor_bounds <- global(utm_rast$velocity_m_day, 
                            fun=quantile, 
                            probs = winsor_thresh, 
                            na.rm = T) |> as.numeric()
    
    mask_matrix <- matrix(data = c(-Inf,winsor_bounds[1], NA,
                                   winsor_bounds[2], Inf, NA),
                          ncol = 3,
                          byrow = TRUE)
    winsor_mask <- terra::classify(utm_rast$velocity_m_day, rcl = mask_matrix)
    utm_rast <- mask(utm_rast, mask = winsor_mask)
  } 
  
  ## Median filter for raster:
  if(median_filter){
    utm_rast <- focal(x = utm_rast,
                      w = 3, #filter windows size
                      fun = "median",
                      na.policy = "omit", # cells that have NA values in orig stay NA
                      na.rm = TRUE #dont count NA cels in median calculation
                      )
  }

  # mask on dates (if days interval between 2 measurements are less than threshold, mask the pixel since the volume estimate not reliable?)
  if(time_mask){
    mask_matrix <- matrix(data = c(-1, days_thresh, NA),
                          ncol = 3)
    
    time_mask_raster <- classify(utm_rast$n_days, rcl = mask_matrix)
    utm_rast  <- mask(utm_rast, mask = time_mask_raster)
  }
  
  ## Get combined error of two observations, using rule of quadrature (HAI)
  combined_hai <- rule_quadrature(utm_rast[[c("reliability_hai_first", "reliability_hai_last")]])

  res_meter <- res(utm_rast)
  pixel_area <- res_meter[1]*res_meter[2] 
  
  ## error propagation section ## 
  volume_error <- combined_hai*pixel_area  ## error in volume
  volume_rate_error <- volume_error/utm_rast$n_days
  aoi_error <- mask(volume_rate_error, mask = utm_aoi)
  aoi_total_error <- global(aoi_error, rule_quadrature, na.rm = TRUE) |> as.numeric() # report
  
  
  ## volume (cubic meter) per day in pixels:
  volume_rate <- utm_rast$velocity_m_day*pixel_area
  real_change <-lapp(c(volume_rate, volume_rate_error), fun = pixel_filter)

  aoi_real_change <- mask(real_change, utm_aoi)
  aoi_volume_rate <- mask(volume_rate, utm_aoi)
 
  # workout total volume change in ROI:
  non_na_raster_expanse <- expanse(aoi_volume_rate)
  non_na_raster_expanse <- non_na_raster_expanse[2]
  
  real_change_expanse <- expanse(aoi_volume_rate)
  real_change_expanse <- real_change_expanse[2]
  
  aoi_expanse <- expanse(utm_aoi) |> as.numeric()
  used_percentage <- 100*(non_na_raster_expanse/aoi_expanse)
  
  tot_vol_change <- global(aoi_volume_rate, "sum", na.rm = TRUE) |> as.numeric() # report
  tot_real_change <- global(aoi_real_change, "sum", na.rm = TRUE) |> as.numeric() # report
  
  ## timing data. section ##
  timing_aoi <-mask(dates_unmasked, mask = utm_aoi)
  my_sum_func <- function(x){ list(vec_min = min(x, na.rm = T),
                                   vec_max = max(x, na.rm = T), 
                                   vec_mode = Modes(x))}
  
  
  timing_summary <- global(timing_aoi, my_sum_func)
  
  data_row <- c(utm_aoi$ID,
                utm_aoi$name,
                tot_vol_change,
                tot_real_change,
                aoi_total_error,
                non_na_raster_expanse,
                real_change_expanse,
                aoi_expanse,
                used_percentage,
                timing_summary$vec_min,
                timing_summary$vec_max,
                timing_summary$vec_mode)
  
  names(data_row) <- c("ID",
                       "name",
                       "dcm_daily",
                       "dcm_error_filtered_daily",
                       "absolute_daily_error_vol_rate",
                       "considered_area",
                       "error_filtered_area",
                       "total_site_area",
                       "perc_aoi_used",
                       c("min_firstd", "min_lastd","min_nd"),
                       c("max_firstd", "max_lastd","max_nd"),
                       c("most_firstd", "most_lastd","most_nd")
                       )
  sites_summary <- rbind(sites_summary, data_row)
  

  
  # save results:
  veloc_path <- file.path("processing", 
                          "site_images",
                           paste0(utm_aoi$ID, "_volume_daily"))
  png(filename = paste0(veloc_path,".png") )
  plot(volume_rate, main = utm_aoi$ID)
  plot(utm_aoi, add = TRUE)
  dev.off()
  writeRaster(volume_rate, paste0(veloc_path,".tif"), overwrite = TRUE)
  writeRaster(real_change, paste0(veloc_path,"_filtered", ".tif"), overwrite = TRUE)  
  }

write.xlsx(sites_summary, file.path("Processing", "site_stats.xlsx"))

spatial_dat <- merge(locations,sites_summary)
writeVector(spatial_dat,
            file.path("Processing", "misc", "site_stats.gpkg"),
            layer = "aois")

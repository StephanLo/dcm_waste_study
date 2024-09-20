## Results for DCM evaluation study.

library(openxlsx)
library(terra)
library(data.table)
library(ggplot2)
library(openxlsx)
library(tmap)
library(sf)
library(dplyr)
library(patchwork)
source("Code/helper_functions.R")


## Graphic setup ----
theme_set(theme_bw())

## Possible color palette:
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# To use for fills, add
#scale_fill_manual(values=cbPalette)

# To use for line and point colors, add
#scale_colour_manual(values=cbPalette)

## data ----
df_spatial <- st_read("processing/misc/site_stats.gpkg")
df <- df_spatial |> as.data.table()

## this dataset contains date with 00 and 32 for dates. Need to change them to 31 and 01
df$most_firstd <- sub(pattern = "32$",
                replacement = 31,
                x = df$most_firstd)

df$most_lastd <- sub(pattern = "32$",
                      replacement = 31,
                      x = df$most_lastd)

df$most_firstd <- sub(pattern = "00$",
                      replacement = 01,
                      x = df$most_firstd)

df$most_lastd <- sub(pattern = "00$",
                     replacement = 01,
                     x = df$most_lastd)
# Then turn date string into date object

df[,c("first_time", "last_time") := list(as.Date(as.character(most_firstd), format = "%Y %m %d"),
                                                as.Date(as.character(most_lastd), format = "%Y %m %d"))]






#Graph 1: This graph shows the distribution of temporal gap between first and last DCM ----
time_g1 <- ggplot(df, aes(x = most_nd)) + 
  geom_histogram(fill = cbPalette[2], alpha = 1, color = "black",
                 binwidth = 50) + 
  labs(x = bquote("Interval between" ~ DCM[1]~ "and" ~ DCM[2] ~ "(days)"),
       y = "Landfill site count") +
  scale_x_continuous(breaks=seq(0,1200,200))

## graph accompanying information:
## number of zero days.
df[most_nd == 0, .N]
# except for the zero day rows, what is the distribution of the data?
df[most_nd != 0, quantile(most_nd)]


#Graph 2: Shows the actual time period of the data coverage in the 100 example sites. ----
df_ordered <- df[order(most_firstd)]
df_ordered$timeid <- 1:100

time_g2 <- ggplot(data = df_ordered) + 
  geom_segment(aes(x = first_time, xend = last_time, y = timeid, yend = timeid), 
               linewidth = 1,
               alpha = 1,
               color = cbbPalette[2]) +
  labs(x = "Time",
       y = bquote("Landfill sites (arranged by" ~ DCM[1] ~ "time)")) + 
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

time_g2 + time_g1 + plot_annotation(tag_levels = "a",
                                    tag_prefix = "(",
                                    tag_suffix = ")")

#ggsave("processing/figures/resources_time.jpg", height = 4, width = 7)


## Graph 3: Height accuracy: ---- 
# shows estimated height accuracy in landfill sites
# which AOIs are below 50 days
df[most_nd < 50, ID]


## exclude values more than 500:
df[most_nd > 50, .N]

df_timef <- df[most_nd > 50,]
## Get the quantile of the error.
df[,quantile(absolute_daily_error_vol_rate, probs = c(0.5,0.75, 0.9, 0.95) )]
df_timef[,quantile(absolute_daily_error_vol_rate,probs = c(0.5,0.75, 0.9, 0.95) )]

error_g1 <- ggplot(df, aes(x = most_nd, 
                           y = absolute_daily_error_vol_rate)) + 
  geom_point(alpha = 1, size = 1) +
  #ylim(0,500)+
  scale_x_continuous(breaks=seq(0,1200,200))+
  geom_vline(xintercept = 50, color = cbPalette[6],linetype = "dashed" )+
  geom_label(x = 150, y =4000, label = "50 days", colour = cbPalette[6] )+
  labs(y = bquote("Volumetric change error"~ (m^3 %.% day^{-1})),
       x = bquote("Interval between" ~ DCM[1]~ "and" ~ DCM[2] ~ "(days)")) + 
  scale_y_continuous(breaks = seq(0,6000,1000) ) +
  theme(axis.title = element_text(size = 10))

error_g2 <- ggplot(df_timef, aes(x = absolute_daily_error_vol_rate)) + 
  geom_histogram(fill = cbPalette[6], alpha = 1, color = "black",
                 binwidth = 20) + 
  labs(x = bquote("Volumetric change error"~ (m^3 %.% day^{-1})),
       y = bquote("Landfill site count  " ~(Delta[t] >50))) +
  scale_x_continuous(breaks=seq(0,500,40)) +
  theme(axis.title = element_text(size = 10))



error_g1 + error_g2 + plot_annotation(tag_levels = "a",
                                    tag_prefix = "(",
                                    tag_suffix = ")") 

ggsave("processing/figures/resources_error.jpg", height = 4, width = 7)


# reported

HK_reports <- read.xlsx("data/in_situ/DCM_evaluation_reported.xlsx", sheet = "Hong Kong")

library(tidyr)
WENT_stats <- HK_reports |> filter(Site== "WENT") |> 
  select(c(4,5,6))|> 
  pivot_wider(names_from = category,
              values_from = quantity_tpd) |> arrange(year)

write.csv(WENT_stats, "processing/misc/WENT_breakdown.csv")



# for the reported statistics, we can work out daily tonnage change like:
aoi_reported <- read.xlsx("Data/waste_statistics/DCM_evaluation_reported.xlsx",
                          sheet = "AOI81")
ndays <- as.Date("2020-01-31") -  as.Date("2018-09-01")

## total in study period
aoi81_tonnes <- sum(aoi_reported$amount)
# average daily:
aoi81_tpd <- aoi81_tonnes/as.numeric(ndays) 

## For AOI 85:
aoi_dcm <- rast("Processing/sites_DCM/figures/volume_daily_AOI85_orig.tif") 
aoi85_interests <- vect("Processing/DCM_evaluation_study/aoi85_interests.shp")|> project("epsg:32649")
plot(aoi_dcm)
plot(aoi85_interests, add = T)

aoi85_active_volumechange <- terra::zonal(aoi_dcm, aoi85_interests, fun = "sum")
# the total volemtric change in the aoi is:
aoi_85_tot_change <- sum(aoi85_active_volumechange$velocity_m_day)
# 6305.636
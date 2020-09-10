# water_supply_timing()

# compare glacier ice melt timing with precipitation and snowmelt timing

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tibble)

##############################################################################################################
# glacier ice melt
glMelt.m = read.csv("results/glMelt/ERA_hist_basin_glMelt_km3_1980_2009_monthly.csv")

# rain
rain.m = read.csv("results/rainFall/ERA_hist_basin_rainFall_km3_1980_2009_monthly.csv")

# snowmelt
snow.m = read.csv("results/snowFall/ERA_hist_basin_snowFall_km3_1980_2009_monthly.csv")

basins = glMelt.m$Basin

for(b in 1:length(basins)){
  b.gl = glMelt.m[b,2:ncol(glMelt.m)]
  b.rn = rain.m[b,2:ncol(rain.m)]
  b.sw = snow.m[b,2:ncol(snow.m)]
  
  b.other = as.numeric(b.rn) + as.numeric(b.sw) # rain + snowmelt
  
  # rain
  r2= signif(summary(lm(as.numeric(b.gl) ~ as.numeric(b.other)))$r.squared,2)
  dat = as.data.frame(cbind(as.numeric(b.other), as.numeric(b.gl)))
  colnames(dat) = c("other", "ice")
  
  basin.plot = 
    ggplot(dat, aes(x=other, y=ice)) +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm) +   # Add linear regression line 
    labs(x = expression(paste("Rainfall + Snowmelt (km"^3~"month"^-1~")")), 
         y = expression(paste("Glacier Ice Melt (km"^3~"month"^-1~")")), 
         title = paste(as.character(basins[b]), "R2 =", r2)) +
    theme_classic(base_size = 12) 
  
  ggsave(filename = paste("ERA_hist_", basins[b], "_basin_SnowPlusRain_vs_glMelt_regression_monthly.png", sep=""),
         plot = basin.plot,
         device = "png",
         path = "figures/Water_source_regression",
         scale = 1, width = 8, height = 4, units = c("in"),
         dpi = 300)
}

##############################################################################################################

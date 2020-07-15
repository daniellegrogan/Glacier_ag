# subbasin_aggregation()

# aggregate WBM results by subbasin

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(maptools)
library(maps)
library(abind)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)


#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

# shapefile for spatial agg
subbasins = raster("/net/nfs/zero/home/WBM_TrANS/data/watershed_regions/HiMAT_full_210_Subset/HiMAT_full_210_Subset_regions.asc")
subbasins.poly = rasterToPolygons(subbasins, dissolve = T)


# spatial aggregation
vars = c("irrigationGross", "GrossIrr_mm_pgi",  "runoff", "runoff_mm_pgi")                                                             # other useful vars
lapply(vars, FUN = function(var){create_dir(file.path("results/subbasin", var))})

mod = "ERA_hist"
path.out  = "results/subbasin"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)
years = seq(1980, 2009)  # climatology historical time series

yearly.agg = lapply(vars, function(var) extract_ts(raster.path = file.path(path.base, "yearly", var), 
                                                   shp = subbasins.poly, 
                                                   years, 
                                                   var, 
                                                   row.nm = as.character(subbasins.poly$HiMAT_full_210_Subset_regions),
                                                   out.nm = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep=""),
                                                   check.file = 0))

yc.agg = lapply(vars, function(var) yearly_to_yc(data.y = read.csv(paste(path.out, "/", var, "/", mod, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")),
                                                 years = seq(1980,2009),
                                                 out.nm = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_",  min(years), "_", max(years), "_yc.csv", sep="")))

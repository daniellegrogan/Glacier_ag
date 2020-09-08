##### RESULTS IN GLACIER_ICE_MELT FOLDER WERE WRONG (DON'T MATCH WITH ROUNCE ET AL 2020). USE GLMELT VARIABLE INSTEAD

# glacier_melt()  

# Spatial aggregation of PyGEM model output: calculate glacier melt in km3/month for each basin
# project: NASA HiMAT
# Danielle S Grogan

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

##################################################################################################################################
# spatial aggregation: monthly time series from netcdf files
glacier_agg = function(gcm, rcp, path.base, var, shp, shp.names){
  gcm = as.character(sub(" ", "", gcm))
  rcp = as.character(sub(" ", "", rcp))
  out.nm      = paste("results/Glacier_ice_melt/", gcm, "_", rcp, "_glacier_", var, "_basins_monthly.csv", sep = "")
  
  if(!file.exists(out.nm)){
    raster.path = file.path(path.base, paste(gcm, "_", rcp, "_c2_ba1_100sets_2000_2100_m.nc", sep=""))
    b = raster::brick(raster.path, varname = var)*1e-9  # 1e-9 to convert from m3 to km3
    a = raster::extract(b, shp, fun = sum,  na.rm = T, sp = F)
    rownames(a) = shp.names
    write.csv(a, out.nm)
  }
  print(out.nm)
  removeTmpFiles(h=6) # remove temporary files older than 6 hours
}
##################################################################################################################################
# temporal aggregation: monthly to yearly time series
glacier_melt_y = function(mod, rcp){
  hist.yrs = seq(1980, 2016)
  fut.yrs  = seq(2000, 2099)
  
  icemelt.m = read.csv(paste("results/Glacier_ice_melt/", mod, "_", rcp, "_glacier_", var, "_basins_monthly.csv", sep = ""))
  out.nm = paste("results/Glacier_ice_melt/", mod, "_", rcp, "_glacier_", var, "_basins_yearly.csv", sep = "")
  if(mod == "ERA_hist"){
    years = hist.yrs
  }else{
    years = fut.yrs
  }
  
  basin.names = icemelt.m[,1]
  
  yrs = as.numeric(substr(names(icemelt.m), start = 2, stop = 5))
  icemelt.m = as.data.frame(subset(icemelt.m, select = c(yrs %in% years)))
  icemelt.m = apply(icemelt.m, c(2), FUN = function(x) as.numeric(unlist(x)))
  
  yrs = subset(yrs, yrs %in% years)
  icemelt.y = t(aggregate(t(icemelt.m), by=list(yrs), sum))[2:16,]
  icemelt.y = cbind(as.character(basin.names), icemelt.y)
  colnames(icemelt.y) = c("Basin", years)
  write.table(icemelt.y, out.nm, sep=",")
}
##################################################################################################################################

basins = readOGR("data/basins_hma", "basins_hma") # for spatial aggregation

path.base = "/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset"
shp       = basins
shp.names = basins$name

##################################################################################################################################
### 1. Ice melt
var       = 'melt'

## 1a. for 3-RCP set of models
mods.3    = read.csv("data/CMIP5_models3.csv")  # models with 3 RCP scenarios
gcms.3    = mods.3$GCM
rcps.3    = colnames(mods.3)[2:ncol(mods.3)]

mod.matrix = mapply(rep, gcms.3, length(rcps.3))

# spatial aggregation of glacier melt
mapply(function(x,y) glacier_agg(x, y, path.base, var, shp, shp.names), mod.matrix, rcps.3)
mapply(function(x,y) glacier_melt_y(x, y), mod.matrix, rcps.3)

## 1b. for 4-RCP set of models
mods.4    = read.csv("data/CMIP5_models4.csv")  # models with 3 RCP scenarios
gcms.4    = mods.4$GCM
rcps.4    = colnames(mods.4)[2:ncol(mods.4)]

mod.matrix = mapply(rep, gcms.4, length(rcps.4))
mod.matrix = sub(" ", "", mod.matrix)


# spatial aggregation of glacier melt
mapply(function(x,y) glacier_agg(x, y, path.base, var, shp, shp.names), mod.matrix, rcps.4)
mapply(function(x,y) glacier_melt_y(x, y), mod.matrix, rcps.4)

# single model, rcp
glacier_agg(gcm = "GFDL-CM3", rcp = "rcp26", path.base, var, shp, shp.names)
glacier_melt_y(gcm = "GFDL-CM3", rcp = "rcp26")

## 1c. for ERA_hist
out.nm = file.path("results/Glacier_ice_melt", paste("ERA_hist_glacier", var, "basins_m.csv", sep = "_"))

if(!file.exists(out.nm)){
  raster.path = file.path(path.base, "ERA-Interim_c2_ba1_100sets_1980_2017_m.nc")
  b = raster::brick(raster.path, varname = var)*1e-9  # 1e-9 to convert from m3 to km3
  a = raster::extract(b, shp, fun = sum,  na.rm = T, sp = F)
  rownames(a) = shp.names
  write.csv(a, out.nm)
}
glacier_melt_y(mod = "ERA_hist", rcp = NA)
##################################################################################################################################
### 2. Total glacier runoff
var       = 'runoff'

## 2a. for 3-RCP set of models
mods.3    = read.csv("data/CMIP5_models3.csv")  # models with 3 RCP scenarios
gcms.3    = mods.3$GCM
rcps.3    = colnames(mods.3)[2:ncol(mods.3)]

mod.matrix = mapply(rep, gcms.3, length(rcps.3))

# spatial aggregation of glacier melt
mapply(function(x,y) glacier_agg(x, y, path.base, var, shp, shp.names), mod.matrix, rcps.3)


## 2b. for 4-RCP set of models
var       = 'runoff'
mods.4    = read.csv("data/CMIP5_models4.csv")  # models with 3 RCP scenarios
gcms.4    = mods.4$GCM
rcps.4    = colnames(mods.4)[2:ncol(mods.4)]

mod.matrix = mapply(rep, gcms.4, length(rcps.4))

# spatial aggregation of glacier melt
mapply(function(x,y) glacier_agg(x, y, path.base, var, shp, shp.names), mod.matrix, rcps.4)


## 2c. for ERA_hist
out.nm = file.path("results/Glacier_ice_melt", paste("ERA_hist_glacier_", var, "_basins_monthly.csv", sep = ""))

if(!file.exists(out.nm)){
  raster.path = file.path(path.base, "ERA-Interim_c2_ba1_100sets_1980_2017_m.nc")
  b = raster::brick(raster.path, varname = var)*1e-9  # 1e-9 to convert from m3 to km3
  a = raster::extract(b, shp, fun = sum,  na.rm = T, sp = F)
  rownames(a) = shp.names
  write.csv(a, out.nm)
}
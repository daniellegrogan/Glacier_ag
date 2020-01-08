# glacier_melt()  # CHECK FILE NAMING ISSUE

# Spatial aggregation of PyGEM model output: calculate glacier melt in km3/month for each basin
# project: NASA HiMAT
# Danielle S Grogan

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgdal)
library(rgeos)

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

##################################################################################################################################
glacier_agg = function(gcm, rcp, path.base, var, shp, shp.names){
  gcm = as.character(sub(" ", "", gcm))
  rcp = as.character(sub(" ", "", rcp))
  out.nm      = file.path("results", gcm, rcp, paste(gcm, rcp, "glacier", var, "basins_m.csv", sep = "_"))
  
  if(!file.exists(out.nm)){
    raster.path = file.path(path.base, paste(gcm, "_", rcp, "_c2_ba1_100sets_2000_2100_m.nc", sep=""))
    b = raster::brick(raster.path, varname = var)*1e-9  # 1e-9 to convert from m3 to km3
    a = raster::extract(b, shp, fun = sum,  na.rm = T, sp = F)
    rownames(a) = shp.names
    write.csv(a, out.nm)
  }
  print(out.nm)
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

# create output directories if they don't already exist
mapply(function(x,y) create_dir(file.path("results", x, y)), mod.matrix, rcps.3)

# spatial aggregation of glacier melt
mapply(function(x,y) glacier_agg(x, y, path.base, var, shp, shp.names), mod.matrix, rcps.3)


## 1b. for 4-RCP set of models
mods.4    = read.csv("data/CMIP5_models4.csv")  # models with 3 RCP scenarios
gcms.4    = mods.4$GCM
rcps.4    = colnames(mods.4)[2:ncol(mods.4)]

mod.matrix = mapply(rep, gcms.4, length(rcps.4))
mod.matrix = sub(" ", "", mod.matrix)

# create output directories if they don't already exist
mapply(function(x,y) create_dir(file.path("results", x, y)), mod.matrix, rcps.4)

# spatial aggregation of glacier melt
mapply(function(x,y) glacier_agg(x, y, path.base, var, shp, shp.names), mod.matrix, rcps.4)



##################################################################################################################################
### 2. Total glacier runoff
var       = 'runoff'

## 2a. for 3-RCP set of models
mods.3    = read.csv("data/CMIP5_models3.csv")  # models with 3 RCP scenarios
gcms.3    = mods.3$GCM
rcps.3    = colnames(mods.3)[2:ncol(mods.3)]

mod.matrix = mapply(rep, gcms.3, length(rcps.3))

# create output directories if they don't already exist
mapply(function(x,y) create_dir(file.path("results", x, y)), mod.matrix, rcps.3)

# spatial aggregation of glacier melt
mapply(function(x,y) glacier_agg(x, y, path.base, var, shp, shp.names), mod.matrix, rcps.3)


## 2b. for 4-RCP set of models
var       = 'runoff'
mods.4    = read.csv("data/CMIP5_models4.csv")  # models with 3 RCP scenarios
gcms.4    = mods.4$GCM
rcps.4    = colnames(mods.4)[2:ncol(mods.4)]

mod.matrix = mapply(rep, gcms.4, length(rcps.4))

# create output directories if they don't already exist
mapply(function(x,y) create_dir(file.path("results", x, y)), mod.matrix, rcps.4)

# spatial aggregation of glacier melt
mapply(function(x,y) glacier_agg(x, y, path.base, var, shp, shp.names), mod.matrix, rcps.4)

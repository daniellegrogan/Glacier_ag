# water_sources()  

# Spatial aggregation of WBM model output: calculate water sources in km3/month for each basin
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

# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)

##################################################################################################################################
water_agg = function(gcm, rcp, path.base, var, shp, shp.names){
  
  if(gcm == "ERA_hist"){
    out.nm      = paste(file.path("results", var), paste("/ERA_hist_basin", var, "km3_monthly.csv", sep = "_"), sep="")
    raster.path = file.path(path.base, paste("ERA_hist/monthly/", var, sep=""))
  }else{
    gcm = as.character(sub(" ", "", gcm))
    rcp = as.character(sub(" ", "", rcp))
    out.nm      = paste(file.path("results", var), paste("/", gcm, "_", rcp, "_basin_", var, "_km3_monthly.csv", sep = ""), sep="")
    raster.path = file.path(path.base, gcm, rcp, "monthly")
  }
  
  if(!file.exists(out.nm)){
    
    hist.yrs  = seq(2000, 2005) 
    fut.yrs = seq(2006, 2099)
    ERA.yrs = seq(1980, 2016)
    if(gcm == "ERA_hist"){
      years = ERA.yrs
    }else if(rcp == "historical"){
      years = hist.yrs
    }else{
      years = fut.yrs
    }
      
    month.data = read.csv("data/days_in_months.csv")
    data.agg = extract_ts(raster.path, shp, years, var)*month.data$days   # x month.data$days to convert from ave mm/day to mm/month
    
    month.cols =  seq(from = as.Date(paste(min(years), "-01-01", sep="")), 
                      to   = as.Date(paste(max(years), "-12-01", sep="")), 
                      by   = "month")
    colnames(data.agg) = as.character(month.cols)
    rownames(data.agg) = c(as.character(shp.names), "all_basins")
    
    # aggregate to yearly
    year.cols = as.numeric(substr(colnames(data.agg), start = 1, stop = 4))
    data.yr = data.frame(matrix(nr=nrow(data.agg), nc=length(years)))
    rownames(data.yr) = rownames(data.agg)
    colnames(data.yr) = unique(year.cols)
    for(i in 1:length(years)){
      data.sub = subset(data.agg, select = c(year.cols == years[i]))
      data.yr[,i] = rowSums(data.sub)
    }
    out.nm.yr = sub("monthly", "yearly", c(out.nm))

    write.csv(data.agg, out.nm)
    write.csv(data.yr, out.nm.yr)
  }
  print(out.nm)
}
##################################################################################################################################

basins = readOGR("data/basins_hma", "basins_hma") # for spatial aggregation

path.base = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12"
shp       = basins
shp.names = basins$name

##################################################################################################################################
### Variables to aggregate
vars  = c('runoff', 'irrRunoff', 'snowMelt', 'precip', 'irrigationGrwt', 'irrigationExtra', 'glMelt', 'baseflow_mm_pgi', 'etIrrCrops', 'soilMoist_mm_pgi')

# create output directories if they don't already exist
lapply(vars, FUN = function(x) create_dir(file.path("results", x)))
lapply(vars, FUN = function(x) water_agg(gcm = 'ERA_hist', 
                                         rcp = "NA", 
                                         path.base, 
                                         var = x, 
                                         shp = basins, 
                                         shp.names = basins$name))

gcms = c("CCSM4", "MIROC5")
for(g in gcms){
  lapply(vars, FUN = function(x) water_agg(gcm = g, 
                                           rcp = "historical", 
                                           path.base, 
                                           var = x, 
                                           shp = basins, 
                                           shp.names = basins$name))
}


# Set up matrices of inputs for GCMs x RCPs
## 3-RCP set of models
mods.3       = read.csv("data/CMIP5_models3.csv")  # models with 3 RCP scenarios
gcms.3       = mods.3$GCM
rcps.3       = colnames(mods.3)[2:ncol(mods.3)]
rcps.3       = c("historical", rcps.3)
mod.matrix.3 = mapply(rep, gcms.3, length(rcps.3))

## 4-RCP set of models
mods.4       = read.csv("data/CMIP5_models4.csv")  # models with 3 RCP scenarios
gcms.4       = mods.4$GCM
rcps.4       = colnames(mods.4)[2:ncol(mods.4)]
rcps.4       = c("historical", rcps.4)
mod.matrix.4 = mapply(rep, gcms.4, length(rcps.4))


for(v in 1:length(vars)){
  mapply(function(x,y) water_agg(x, y, path.base, vars[v], shp, shp.names), mod.matrix.3, rcps.3)
  mapply(function(x,y) water_agg(x, y, path.base, vars[v], shp, shp.names), mod.matrix.4, rcps.4)
  print(vars[v])
}

# one gcm at a time
lapply(vars, FUN = function(x) water_agg(gcm = "CCSM4", rcp = "rcp45", path.base, x, shp, shp.names))



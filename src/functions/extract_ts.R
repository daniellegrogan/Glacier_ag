# extract_ts()
# wrapper on spatial_aggregation()

# project: NASA HiMAT
# Danielle S Grogan

library(raster)
library(rgdal)
library(rgeos)

extract_ts = function(raster.path, # path to wbm output
                      shp,         # shapefile for spatial aggregation
                      var = NA,    # only needed if wbm output is monthly; variable name to load
                      years = NA,    # sequence of years. If NA, all years are used
                      s=1,         # sum = 1 (set to 0 for average spatial aggregation)
                      cell.area=1, 
                      weight=T, 
                      poly.out=F){
  region = gUnaryUnion(shp, id = rep(1, length(shp)))
  
  if(grepl("yearly", c(raster.path))){
    
    if(sum(is.na(years)) == 1){
      file.list = list.files(path = path, full.names = T)
    }else{
      file.list.full = list.files(path = raster.path, full.names = T)
      file.yrs = substr(file.list.full, start = nchar(file.list.full)-6, stop= nchar(file.list.full)-3)
      file.list = file.list.full[as.numeric(file.yrs) %in% years]
    }
    brk = do.call(stack,
                  lapply(file.list, 
                         raster::brick))
    
  }else if(grepl("monthly", c(raster.path))){
    
    if(sum(is.na(years)) == 1){
      file.list = list.files(path = raster.path, full.names = T)
    }else{
      file.list.full = list.files(path = raster.path, full.names = T)
      file.list = file.list.full[sapply(years, FUN = function(x) grep(pattern=x, file.list.full))]
    }
    
    brk = do.call(stack,
                  lapply(file.list, 
                         raster::brick, 
                         varname = var))
  }
  
  s1 = spatial_aggregation(brk, shp,    s, cell.area, weight, poly.out)
  s2 = spatial_aggregation(brk, region, s, cell.area, weight, poly.out)
  
  out = rbind(s1,s2)
}


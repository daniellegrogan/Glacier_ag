# extract_ts()
# wrapper on spatial_aggregation()

# project: NASA HiMAT
# Danielle S Grogan

extract_ts = function(raster.path, # path to wbm output
                      shp,         # shapefile for spatial aggregation
                      var = NA,    # only needed if wbm output is monthly; variable name to load
                      s=1,         # sum = 1 (set to 0 for average spatial aggregation)
                      cell.area=1, 
                      weight=T, 
                      poly.out=F){
  region = gUnaryUnion(shp, id = rep(1, length(shp)))
  
  if(grepl("yearly", c(raster.path))){
    brk = do.call(brick,
                  lapply(list.files(path      = raster.path,
                                    full.names=T,
                                    pattern   = "wbm"),
                         raster))
    
  }else if(grepl("monthly", c(raster.path))){
    
    brk = do.call(stack,
                  lapply(list.files(path      = raster.path,
                                    full.names=T,
                                    pattern   = "wbm"),
                         brick, varname = var))
  }
  
  s1 = spatial_aggregation(brk, shp, s, cell.area, weight, poly.out)
  s2 = spatial_aggregation(brk, region, s, cell.area, weight, poly.out)
  
  out = rbind(s1,s2)
}


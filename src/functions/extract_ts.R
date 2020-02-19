# extract_ts()
# wrapper on spatial_aggregation()

# project: NASA HiMAT
# Danielle S Grogan

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

extract_ts = function(raster.path, # path to wbm output
                      shp,         # shapefile for spatial aggregation
                      years,       # sequence of years
                      var = NA,    # only needed if wbm output is monthly; variable name to load
                      s=1,         # sum = 1 (set to 0 for average spatial aggregation)
                      cell.area = 1, 
                      weight = T, 
                      poly.out = F,
                      row.nm = NA,  # characters; typically the basin names
                      out.nm = NA,  # out.nm = character string; if provided, results are written to file
                      check.file = 1 ){  # 1 or 0; if 1, exits if file exists.  Requires and out.nm (not NA)
  

  if(file.exists(out.nm) & check.file == 1){  # if the file exists, and check.file = 1, then just read in the file.
    out = read.csv(out.nm)
    print(paste(out.nm, "already exists"))
    
  }else{                                      # otherwise, do the aggregation
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
      brk = 365*brk   # convert average mm/day over the year to mm/year
      
      
    }else if(grepl("monthly", c(raster.path))){
      
      if(sum(is.na(years)) == 1){
        file.list = list.files(path = raster.path, full.names = T)
      }else{
        file.list.full = list.files(path = raster.path, full.names = T)
        file.yrs = substr(file.list.full, start = nchar(file.list.full)-6, stop= nchar(file.list.full)-3)
        file.list = file.list.full[as.numeric(file.yrs) %in% years]
      }
      
      brk = do.call(stack,
                    lapply(file.list, 
                           raster::brick, 
                           varname = var))
      
      # x days-per-month to convert from ave/month to total per month
      month.data = read.csv("data/days_in_months.csv")
      brk = month.data$days*brk
      
    }
    
    s1 = spatial_aggregation(brk, shp,    s, cell.area, weight, poly.out)
    s2 = spatial_aggregation(brk, region, s, cell.area, weight, poly.out)
    
    row.names = c(row.nm, "all_basins")
    month.cols =  seq(from = as.Date(paste(min(years), "-01-01", sep="")), 
                      to   = as.Date(paste(max(years), "-12-01", sep="")), 
                      by   = "month")
    
    out = rbind(s1,s2)
    out = cbind(row.names, out)
    colnames(out) = c("Basin", as.character(month.cols))
    
    if(is.na(out.nm) == F){
      # make a date sequence for the column names. assume full years (Jan through Dec)
      write.csv(out, out.nm, row.names = F)
      print(paste(out.nm, "written to file"))
    }
  }

  
  out
}


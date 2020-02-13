# agg_contribution()
# Calculate contribution of tracked water components to irrigation

# project: NASA HiMAT
# Danielle S Grogan

library(raster)
library(rgdal)
library(rgeos)

agg_contribution = function(path,          # path to wbm output
                            basins,        # shapefile of basins over which to aggregate
                            vars,          # vector of two character strings: should be two complementary variables for component tracking, e.g., GrossIrr and GrossIrr_pg
                            years,         # vector of years over which to process, e.g., seq(2000, 2005)
                            percent.nm,    # character string: name for percent output, e.g., "GrossIrr_pg_percent"
                            out.nm.pre     # character string: location for output with file prefix, e.g., "results/ERA_hist/ERA_hist_basin_"
                            ){     
  
  if(grepl("monthly", c(path))){
    
    if(grepl("ERA_hist", path)){
      basin.agg = lapply(vars, function(var) extract_ts(raster.path = file.path(path, var), shp = basins, years, var))
    }else{
      basin.agg = lapply(vars, function(var) extract_ts(raster.path = path, shp = basins, years, var))
    }

    # for monthly
    # x days-per-month to convert from ave/month to total per month
    month.data = read.csv("data/days_in_months.csv")
    
    basin.array = month.data$days*array(as.numeric(unlist(basin.agg)), dim=c(nrow(basin.agg[[1]]), ncol(basin.agg[[1]]), length(vars)))
    gross_irr_pg_percent = 100*(basin.array[,,2]/basin.array[,,1])
    basin.array = abind(basin.array, gross_irr_pg_percent, along=3)
    
    # make a date sequence for the column names. assume full years (Jan through Dec)
    month.cols =  seq(from = as.Date(paste(min(years), "-01-01", sep="")), 
                      to   = as.Date(paste(max(years), "-12-01", sep="")), 
                      by   = "month")
    
    # write full time series to file
    for(i in 1:dim(basin.array)[3]){
      if(i < 3){
        out.nm.i = paste(out.nm.pre, sub("mm", "km3", vars[i]), "_", min(years), "_", max(years), "_monthly.csv", sep="")
      }else{
        out.nm.i = paste(out.nm.pre, sub("mm", "percent", vars[2]), "_", min(years), "_", max(years), "_monthly.csv", sep="")
      }
      var.out = basin.array[,,i]
      rownames(var.out) = c(as.character(basins$name), "all_basins")
      colnames(var.out) = as.character(month.cols)
      write.csv(var.out, out.nm.i)
    }
    
    # calculate mean and stdev
    n.years = length(years)
    m = seq(1,12*n.years)
    month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    month.summary.mean  = data.frame(matrix(nr=dim(basin.array)[1], nc=dim(basin.array)[3]*12))
    month.summary.stdev = data.frame(matrix(nr=dim(basin.array)[1], nc=dim(basin.array)[3]*12))
    
    m.names = lapply(X = month.names, FUN = rep, dim(basin.array)[3])
    var.names = c(vars, percent.nm)
    colnames(month.summary.mean)  = paste(unlist(m.names), var.names, sep="_")
    colnames(month.summary.mean) = sub("mm", "km3", colnames(month.summary.mean))   # replace "mm" with "km3" in column names because extract_ts() takes depth and outputs volume
    colnames(month.summary.stdev) = paste(unlist(m.names), paste(var.names, "stdev", sep="_"), sep="_")
    colnames(month.summary.stdev) = sub("mm", "km3", colnames(month.summary.stdev))   # replace "mm" with "km3" in column names because extract_ts() takes depth and outputs volume
    
    m = seq(1:ncol(basin.array))
    
    for(month in 1:12){
      month.data = basin.array[,which(m%%12 == month),]
      month.summary.mean[((3*month)-2):(3*month)]  = apply(month.data, c(1,3), mean)
      month.summary.stdev[((3*month)-2):(3*month)] = apply(month.data, c(1,3), sd)
    }
    
    out = cbind(month.summary.mean, month.summary.stdev)
    rownames(out) = c(as.character(basins$name), "all_basins")
    
  }else if(grepl("yearly", c(path))){
    raster.paths = file.path(path, vars)
    basin.agg = lapply(raster.paths, extract_ts, basins, years)
    
    # x365 to convert from ave/year to total per year
    basin.array = 365*array(as.numeric(unlist(basin.agg)), dim=c(length(basins)+1, ncol(basin.agg[[1]]), length(vars)))
    var_percent = 100*(basin.array[,,2]/basin.array[,,1])
    basin.array = abind(basin.array, var_percent, along=3)
    
    # write full time series to file
    for(i in 1:dim(basin.array)[3]){
      if(i < 3){
        out.nm.i = paste(out.nm.pre, sub("mm", "km3", vars[i]), "_", min(years), "_", max(years), "_yearly.csv", sep="")
      }else{
        out.nm.i = paste(out.nm.pre, sub("mm", "percent", vars[2]), "_", min(years), "_", max(years), "_yearly.csv", sep="")
      }
      var.out = basin.array[,,i]
      rownames(var.out) = c(as.character(basins$name), "all_basins")
      colnames(var.out) = years
      write.csv(var.out, out.nm.i)
    }
    
    # calculate mean and stdev
    mean.var = apply(basin.array, c(1,3), mean)
    mean.var = as.data.frame(mean.var)
    colnames(mean.var) = c(vars, percent.nm)
    colnames(mean.var) = sub("mm", "km3", colnames(mean.var)) # replace "mm" with "km3" in column names because extract_ts() takes depth and outputs volume
    rownames(mean.var) = c(as.character(basins$name), "all_basins")
    
    stdev.var = apply(basin.array, c(1,3), sd)
    stdev.var = as.data.frame(stdev.var)
    colnames(stdev.var) = paste(c(vars, percent.nm), "stdev", sep="_")
    rownames(stdev.var) = c(as.character(basins$name), "all_basins")
    
    out = cbind(mean.var, stdev.var)
  }
  
  # output mean and stdev (needed in other function calls)
  out   
}

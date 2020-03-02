# glacier_test()
# Generate input data for test simulations
# project: NASA HiMAT
# Danielle S Grogan

library(raster)
library(rgdal)

#######################################################################################################################################
format_brick_dates = function(infile){
  # format layer names as dates, use to subset input file
  date.str = (sub("X", "", names(infile)))
  date.str = (sub(".00.00.00", "", date.str))
  date.str = (sub(".01.00.00", "", date.str))
  date.layers = as.Date(date.str, format = "%Y.%m.%d")
  date.layers
}
#######################################################################################################################################
prepend_year_zeros = function(infile){
  # nb: infile must start at the beginning of a year (may need to subset input before using function)
  
  # format layer names as dates
  date.layers = format_brick_dates(infile)
  year.layers = as.numeric(substr(start = 1, stop = 4, date.layers))
  
  # subset to one year
  if(sum(year.layers == min(year.layers)) == 12){ # if T, then the file is monthly.  Else, yearly
    new.dat = subset(infile, 1:12)  # use input data as a template
    values(new.dat) = 0
    names(new.dat) = sub(min(year.layers), "1979", names(subset(infile, 1:12)))  # replace year names
  }else{
    new.dat = subset(infile, 1)  # use input data as a template
    values(new.dat) = 0
    names(new.dat) = sub(min(year.layers), "1979", names(subset(infile, 1)))  # replace year names
  }
  out = stack(new.dat, infile)
  out
}
#######################################################################################################################################
test_2_format = function(infile){
  # nb: infile must start at the beginning of a year (may need to subset input before using function)
  
  # format layer names as dates
  date.layers = format_brick_dates(infile)
  year.layers = as.numeric(substr(start = 1, stop = 4, date.layers))
  
  # subset to one year
  if(sum(year.layers == min(year.layers)) == 12){ # if T, then the file is monthly.  Else, yearly
    
    # calculate a monthly climatology for the first layer (year 1979)
    n.yrs = length(unique(year.layers))
    m.clim = stackApply(infile, indices = rep(seq(1:12), n.yrs), fun = mean)
    names(m.clim) = sub(min(year.layers), "1979", names(subset(infile, 1:12)))  # replace year names
    
    # make a time series of 0s for the remaining layers
    new.dat = infile  # use input data as a template
    values(new.dat) = 0
    names(new.dat)  = names(infile)  # replace year names (they get overwritten when resetting the values)
    out = stack(m.clim, new.dat)
    
  }else{
    
    # calculate a yearly climatology for the first layer (year 1979)
    y.clim = calc(infile, mean)
    names(y.clim) = sub(min(year.layers), "1979", names(subset(infile, 1)))  # replace year names
    
    # make a time series of 0s for the remaining layers
    new.dat = infile  # use input data as a template
    values(new.dat) = 0
    names(new.dat)  = names(infile)  # replace year names (they get overwritten when resetting the values)
    out = stack(y.clim, new.dat)
  }
  out
}
#######################################################################################################################################

# Outputs:
# 1. glMelt (m)
# 2. glIceMelt (m)
# 3. glArea (y)
# 4. glVolume (y)

vars = c("runoff", "melt", "area_frac", "volume")
path = "/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset/"
file.base = "ERA-Interim_c2_ba1_100sets_1980_2017_"

#######################################################################################################################################
# Test 1: 0 glacier melt during spin-up year 1970, and glacier melt input through the historical period (1980 - 2016)

for(v in vars){
  if(v == "runoff" | v == "melt"){
    ts = "m"
    time.unit = "monthly"
    unit = "m3/month"
  }else if(v == "area_frac" | v == "volume"){
    ts = "y"
    time.unit = "yearly"
    if(v == "area_frac"){
      unit = "fraction"
    }else if(v == "volume"){
      unit = "km3"
    }
  }

  infile  = brick(paste(path, file.base, ts, ".nc", sep=""), varname = v)
  
  # format layer names as dates, use to subset input file
  date.layers = format_brick_dates(infile)
  year.layers = as.numeric(substr(start = 1, stop = 4, date.layers))
  infile.sub  = subset(infile, which(year.layers > 1979))
  
  outfile = prepend_year_zeros(infile.sub)
  out.nm = paste(path, "TEST_1_1979_2016_", v, "_", ts, ".nc", sep="")
  writeRaster(outfile, 
              out.nm, 
              "CDF", 
              overwrite = TRUE,
              varname   = v,
              varunit   = unit,
              xname     = "lon",
              yname     = "lat", 
              zname     = "time", 
              zunit     = time.unit,
              bylayer   = FALSE,
              NAflag    = -9999)
}


#######################################################################################################################################
# Test 2: glacier melt during spin-up year 1970, and 0 glacier melt through the historical period (1980 - 2016)

for(v in vars){
  if(v == "runoff" | v == "melt"){
    ts = "m"
    time.unit = "monthly"
    unit = "m3/month"
  }else if(v == "area_frac" | v == "volume"){
    ts = "y"
    time.unit = "yearly"
    if(v == "area_frac"){
      unit = "fraction"
    }else if(v == "volume"){
      unit = "km3"
    }
  }
  
  infile  = brick(paste(path, file.base, ts, ".nc", sep=""), varname = v)
  
  # format layer names as dates, use to subset input file
  date.layers = format_brick_dates(infile)
  year.layers = as.numeric(substr(start = 1, stop = 4, date.layers))
  infile  = subset(infile, which(year.layers > 1979))
  
  outfile = test_2_format(infile)
  out.nm = paste(path, "TEST_2_1979_2016_", v, "_", ts, ".nc", sep="")
  writeRaster(outfile, 
              out.nm, 
              "CDF", 
              overwrite = TRUE,
              varname   = v,
              varunit   = unit,
              xname     = "lon",
              yname     = "lat", 
              zname     = "time", 
              zunit     = time.unit,
              bylayer   = FALSE,
              NAflag    = -9999)
}

#######################################################################################################################################
# SCRATCH

icemelt = read.csv("results/Glacier_ice_melt/GFDL-CM3_rcp26_glacier_melt_basins_monthly.csv")

basins = icemelt$X
for(b in 1:15){
  plot(as.numeric(icemelt[b,2:ncol(icemelt)]), type='l', main = basins[b])
}




#
snowmelt.m = read.csv("results/snowMelt/ERA_hist_basin_snowMelt_km3_1980_2009_monthly.csv")
snowmelt.y = read.csv("results/snowMelt/ERA_hist_basin_snowMelt_km3_1980_2009_yearly.csv")

for(b in 1:15){
  plot(as.numeric(snowmelt.y[b,3:ncol(snowmelt.y)]), type='l', main = basins[b])
}

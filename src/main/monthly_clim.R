# monthly_clim()

# calculate monthly climatologies from monthly time series

library(RCurl)  # enables sourcing R code from github
source("src/functions/monthly_to_mc.R")

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

################################################################################################
monthly_clim = function(mod, rcp){
  ### Water sources
  if(mod == "ERA_hist"){
    m.char = mod
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  
  vars = c("precip", "snowMelt", "snowFall")
  for(v in vars){
    data.m = read.csv(paste("results/", v, "/", m.char, "_basin_", v, "_km3_monthly.csv", sep=""))
    data.mc = as.data.frame(cbind(data.m[,1], monthly_to_mc(data.m[,2:ncol(data.m)])))
    colnames(data.mc)[1] = "Basin"
    out.nm = paste("results/", v, "/", m.char, "_basin_", v, "_km3_mc.csv", sep="")
    write.csv(data.mc, out.nm, row.names=F)
  }
  
  # glacier ice melt
  data.m = read.csv(paste("results/Glacier_ice_melt/", m.char, "_glacier_melt_basins_monthly.csv", sep=""))
  data.mc = as.data.frame(cbind(data.m[,1], monthly_to_mc(data.m[,2:ncol(data.m)])))
  colnames(data.mc)[1] = "Basin"
  out.nm = paste("results/Glacier_ice_melt/", m.char, "_glacier_melt_basins_mc.csv", sep="")
  write.csv(data.mc, out.nm, row.names=F)
  
  # calculate rain (liquid precip) 
  precip   = read.csv(paste("results/precip/", m.char, "_basin_precip_km3_monthly.csv", sep=""))
  snowFall = read.csv(paste("results/snowFall/", m.char, "_basin_snowFall_km3_monthly.csv", sep=""))
  
  rain.m = precip[,2:ncol(precip)] - snowFall[,2:ncol(snowFall)]
  rain.mc = as.data.frame(cbind(precip[,1], monthly_to_mc(rain.m)))
  colnames(rain.mc)[1] = "Basin"
  rain.m.out = as.data.frame(cbind(rain.mc[,1], rain.m))
  
  #create_dir("results/rainFall")
  write.csv(rain.m.out, paste("results/rainFall/", m.char, "_basin_rainFall_km3_monthly.csv", sep=""), row.names=F)
  write.csv(rain.mc, paste("results/rainFall/", m.char, "_basin_rainFall_km3_mc.csv", sep=""), row.names=F)
  
}

################################################################################################

# MAIN #

mod = "ERA_hist"
monthly_clim(mod, rcp="NA")


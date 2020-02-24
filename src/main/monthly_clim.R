# monthly_clim()   # ARCHIVE?

# calculate monthly climatologies from monthly time series

library(RCurl)  # enables sourcing R code from github
source("src/functions/monthly_to_mc.R")

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

################################################################################################
monthly_clim = function(mod, rcp, vars, years = NA){
  ### Water sources
  if(mod == "ERA_hist"){
    m.char = mod
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  
  yr.str = paste(min(years), max(years), sep="_")
  
  for(v in vars){
    if(v == "glacier_ice_melt"){
      # glacier ice melt
      data.m = read.csv(paste("results/Glacier_ice_melt/", m.char, "_glacier_melt_basins_monthly.csv", sep=""))
      yr = as.numeric(substr(colnames(data.m), start=2, stop=5))
      if(sum(is.na(years)) == F){
        data.m = subset(data.m, select=c(yr %in% years))
        data.mc = as.data.frame(cbind(data.m[,1], monthly_to_mc(data.m)))
        out.nm = paste("results/Glacier_ice_melt/", m.char, "_glacier_melt_basins_", min(years), "_", max(years), "_mc.csv", sep="")
      }else{
        data.mc = as.data.frame(cbind(data.m[,1], monthly_to_mc(data.m[,2:ncol(data.m)])))
        out.nm = paste("results/Glacier_ice_melt/", m.char, "_glacier_melt_basins_", min(yr, na.rm=T), "_", max(yr, na.rm=T), "_mc.csv", sep="")
      }
      colnames(data.mc)[1] = "Basin"
      write.csv(data.mc, out.nm, row.names=F)
      
    }else if(v == "rainFall"){
      # calculate rain (liquid precip) 
      precip   = read.csv(paste("results/precip/", m.char, "_basin_precip_km3_monthly.csv", sep=""))
      snowFall = read.csv(paste("results/snowFall/", m.char, "_basin_snowFall_km3_monthly.csv", sep=""))
      
      rain.m = precip[,2:ncol(precip)] - snowFall[,2:ncol(snowFall)]
      yr = as.numeric(substr(colnames(rain.m), start=2, stop=5))
      
      if(sum(is.na(years)) == F){
        rain.m = subset(rain.m, select=c(yr %in% years))
        rain.mc = as.data.frame(cbind(precip[,1], monthly_to_mc(rain.m)))
        out.nm.mc = paste("results/rainFall/", m.char, "_basin_rainFall_km3_", min(years), "_", max(years), "_mc.csv", sep="")
        out.nm.monthly = paste("results/rainFall/", m.char, "_basin_rainFall_km3_", min(years), "_", max(years), "_monthly.csv", sep="")
        
      }else{
        rain.mc = as.data.frame(cbind(precip[,1], monthly_to_mc(rain.m)))
        out.nm.mc = paste("results/rainFall/", m.char, "_basin_rainFall_km3_", min(yr, na.rm=T), "_", max(yr, na.rm=T), "_mc.csv", sep="")
        out.nm.monthly = paste("results/rainFall/", m.char, "_basin_rainFall_km3_", min(yr, na.rm=T), "_", max(yr, na.rm=T), "_monthly.csv", sep="")
      }
      
      colnames(rain.mc)[1] = "Basin"
      rain.m.out = as.data.frame(cbind(rain.mc[,1], rain.m))
      
      #create_dir("results/rainFall")
      write.csv(rain.m.out, out.nm.monthly, row.names=F)
      write.csv(rain.mc, out.nm.mc, row.names=F)
      
    }else{
      data.m = read.csv(paste("results/", v, "/", m.char, "_basin_", v, "_km3_monthly.csv", sep=""))
      yr = as.numeric(substr(colnames(data.m), start=2, stop=5))
      
      if(sum(is.na(years)) == F){
        data.m = subset(data.m, select=c(yr %in% years))
        data.mc = as.data.frame(cbind(data.m[,1], monthly_to_mc(data.m)))
        out.nm = paste("results/", v, "/", m.char, "_basin_", v, "_km3_", min(years), "_", max(years), "_mc.csv", sep="")
      }else{
        data.mc = as.data.frame(cbind(data.m[,1], monthly_to_mc(data.m[,2:ncol(data.m)])))
        out.nm = paste("results/", v, "/", m.char, "_basin_", v, "_km3_", min(yr, na.rm=T), "_", max(yr, na.rm=T), "_mc.csv", sep="")
      }
      colnames(data.mc)[1] = "Basin"
      write.csv(data.mc, out.nm, row.names=F)
    }
   
  }
  
}

################################################################################################

# MAIN #
vars = c("precip", "snowMelt", "snowFall", "rainFall", "glacier_ice_melt")

mod = "ERA_hist"
years = seq(1980, 2009)
monthly_clim(mod, rcp="NA", years)


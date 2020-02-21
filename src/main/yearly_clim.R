# yearly_clim()

# calculate yearly climatolgies for climate normals


### R Libraries
library(RCurl)  # enables sourcing R code from github

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)

#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

vars = c('precip', 'irrigationGross', 'Glacier_ice_melt')  # other useful vars

############ ERA HISTORICAL ################
mod = "ERA_hist"
path.out  = "results"
years = seq(1980, 2009)  # climatology historical time series
yr.str = paste(min(years), max(years), sep="_")

for(v in vars){
  if(v == "Glacier_ice_melt"){
    data.y = read.csv(paste("results/", v, "/", mod, "_glacier_melt_basins_yearly.csv", sep=""))
    out.nm = paste("results/", v, "/", mod, "_glacier_melt_basins_", yr.str, "_yc.csv", sep="")
  }else if(v == "precip"){
    data.y = read.csv(paste("results/", v, "/", mod, "_basin_", v, "_km3_yearly.csv", sep=""))
    out.nm = paste("results/", v, "/", mod, "_basin_", v, "_km3_", yr.str, "_yc.csv", sep="")
  }else if(v == "irrigationGross"){
    data.y = read.csv(paste("results/", v, "/", mod, "_basin_", v, "_km3_", yr.str, "_yearly.csv", sep=""))
    out.nm = paste("results/", v, "/", mod, "_basin_", v, "_km3_", yr.str, "_yc.csv", sep="")
  }
  yearly_to_yc(data.y, 
               years, 
               out.nm)
}


############ GCM Future ################
mods  = c("CCSM4", "MIROC5")
rcp = c("rcp45", "rcp85")
path.out  = "results"

# mid-century climatology
years = seq(2040, 2069) 
yr.str = paste(min(years), max(years), sep="_")

for(m in mods){
  for(r in rcp){
    if(v == "Glacier_ice_melt"){
      data.y = read.csv(paste("results/", v, "/", m, "_", r, "_glacier_melt_basins_yearly.csv", sep=""))
      out.nm = paste("results/", v, "/", m, "_", r, "_glacier_melt_basins_", yr.str, "_yc.csv", sep="")
    }else if(v == "irrigationGross"){
      data.y = read.csv(paste("results/", v, "/", m, "_", r, "_basin_", v, "_km3_2006_2099_yearly.csv", sep=""))
      out.nm = paste("results/", v, "/", m, "_", r, "_basin_", v, "_km3_", yr.str, "_yc.csv", sep="")
    }else if(v == "precip"){
      data.y = read.csv(paste("results/", v, "/", m, "_", r, "_basin_", v, "_km3_yearly.csv", sep=""))
      out.nm = paste("results/", v, "/", m, "_", r, "_basin_", v, "_km3_", yr.str, "_yc.csv", sep="")
    }
    yearly_to_yc(data.y, 
                 years, 
                 out.nm)
    
  }
}


# late-century climatology
years = seq(2070, 2099) 
yr.str = paste(min(years), max(years), sep="_")

for(v in vars){
  
  for(m in mods){
    for(r in rcp){
      if(v == "Glacier_ice_melt"){
        data.y = read.csv(paste("results/", v, "/", m, "_", r, "_glacier_melt_basins_yearly.csv", sep=""))
        out.nm = paste("results/", v, "/", m, "_", r, "_glacier_melt_basins_", yr.str, "_yc.csv", sep="")
      }else if(v == "irrigationGross"){
        data.y = read.csv(paste("results/", v, "/", m, "_", r, "_basin_", v, "_km3_2006_2099_yearly.csv", sep=""))
        out.nm = paste("results/", v, "/", m, "_", r, "_basin_", v, "_km3_", yr.str, "_yc.csv", sep="")
      }else if(v == "precip"){
        data.y = read.csv(paste("results/", v, "/", m, "_", r, "_basin_", v, "_km3_yearly.csv", sep=""))
        out.nm = paste("results/", v, "/", m, "_", r, "_basin_", v, "_km3_", yr.str, "_yc.csv", sep="")
      }
      yearly_to_yc(data.y, 
                   years, 
                   out.nm)
      
    }
  }
  
}

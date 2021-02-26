# spinup climatology run analysis

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(zoo)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

### Source functions from within this project:
source("src/functions/extract_ts.R")

#####################################################################################################################################

# paths
path.base = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2021-02/HiMAT_clim_1981_2010/"  #NB: year 2010 got cut off - don't use
path.out = "results/spinup_clim/"
path.base = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2021-02/HiMAT_wbm_core_noCrops/"  #NB: year 2010 got cut off - don't use


# shapefiles for spatial agg
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
subbasins.poly = readOGR(dsn = "data/subbasin_poly/", layer = "subbasin_poly")

# variables to aggregate
vars.stor = c("resStorage",
              "resStorage_m3_pgi",
              "endoStrg",
              "endoStrg_m3_pgi",
              "surfRffStorage",
              "ricePaddyStrg",
              "irrRffStorage",
              "runoffStg_mm_pgi",
              "soilMoist",
              "soilMoist_mm_pgi",
              "grdWater",
              "grndWater_mm_pgi")

vars.flux = c("evapotrans",	    
              "IrrEvap_mm_pgi",
              "endoEvap",
              "endoStrg_m3_pgi",
              "etIrrCrops",
              "etIrrCrops_mm_pgi",
              "openWaterEvap",
              "openWaterEvap_m3_pgi",
              "runoff",
              "runoff_mm_pgi",
              "irrRunoff",
              "baseflow", 
              "baseflow_mm_pgi",
              "irrigationGross",
              "GrossIrr_mm_pgi",
              "irrigationExtra",
              "irrigationFlow",
              "IrrFlow_mm_pgi",
              "irrigationGrwt", 
              "IrrGrwt_mm_pgi",
              "addedWater",
              "IrrPercRice_mm_pgi",
              "irrPercIneff",	      
              "IrrPercIneff_mm_pgi"
)

# make folders for output
lapply(vars.stor, FUN = function(var){create_dir(file.path("results/spinup_clim", var))})

# Daily
# aggregate storage vars by basin
basin.agg = lapply(vars.stor, function(var) extract_ts(raster.path = file.path(path.base, "daily"), 
                                                       shp = basins, 
                                                       years = seq(1981, 2010), 
                                                       var, 
                                                       row.nm = as.character(basins$name),
                                                       out.nm = paste(path.out, var, "/basin_", var, "_km3_daily.csv", sep=""),
                                                       check.file = 0))

# aggregate storage vars by subbasin
subbasin.agg = lapply(vars.stor, function(var) extract_ts(raster.path = file.path(path.base, "daily"), 
                                                       shp = subbasins.poly, 
                                                       years = seq(1981, 2010), 
                                                       var, 
                                                       row.nm = as.character(subbasins.poly$HMAT__2),
                                                       out.nm = paste(path.out, "/", var, "/subbasin_", var, "_km3_daily.csv", sep=""),
                                                       check.file = 0))


# YEARLY
# aggregate storage vars by basin

# grdWater
var = "grdWater"
grdWater_yearly = 
  extract_ts(raster.path = file.path(path.base, "yearly", "grdWater"), 
             shp = basins, 
             years = seq(1981, 1992), 
             var = "grdWater", 
             row.nm = as.character(basins$name),
             out.nm = paste(path.out, var, "/pristine_basin_", var, "_km3_yearly.csv", sep=""),
             check.file = 0)

ymin = min(as.numeric(grdWater_yearly[,2:ncol(grdWater_yearly)]))
ymax = max(as.numeric(grdWater_yearly[,2:ncol(grdWater_yearly)]))

for(i in 1:16){
  #png(paste("figures/spinup_clim_figures/grdWater_avekm3Yr_", grdWater_yearly[i,1], ".png", sep=""))
  plot(as.numeric(grdWater_yearly[i,2:ncol(grdWater_yearly)]), type='l',
       main=paste(grdWater_yearly[i,1], "ave grdWater"), ylab = c("km3/yr"), xlab = c("Years from start"))
  points(as.numeric(grdWater_yearly[i,2:ncol(grdWater_yearly)]))
  #dev.off()
}

for(i in 1:16){
  if(i==1){
    plot(as.numeric(grdWater_yearly[i,2:29]), type='l', ylim = c(ymin, ymax))
  }
  lines(as.numeric(grdWater_yearly[i,2:29]), col=i)
}

# percent change
per.chg = function(vec.x){
  p.vec = mat.or.vec(nr=length(vec.x)-1, nc=1)
  for(i in 1:length(vec.x)-1){
    p.vec[i] = 100*(vec.x[i+1] - vec.x[i])/vec.x[i]
  }
  p.vec
}

for(i in 1:16){
  p.change.1 = per.chg(as.numeric(grdWater_yearly[i, 2:ncol(grdWater_yearly)]))
  if(i == 1){
    plot(p.change.1, type='l', ylim=c(-1.5,1.5), main = paste(grdWater_yearly[i,1], "% chg grdWater"))
  }else{  
    lines(p.change.1, col=i)
  }
  abline(h=0, col='black', lty=2)
}

for(i in 1:16){
  p.change.1 = per.chg(as.numeric(grdWater_yearly[i, 2:ncol(grdWater_yearly)]))
  
  #png(paste("figures/spinup_clim_figures/grdWater_percent_chg_", grdWater_yearly[i,1], ".png", sep=""))
  plot(p.change.1, type='l', ylim=c(-1.5,1.5), 
       main=paste(grdWater_yearly[i,1], "grdWater"), ylab = c("percent change"), xlab = c("Years from start"))
  abline(h=0, col='grey')
  #abline(lm(p.change.1~seq(1:27)), col='red', lty=2)
  #lines(rollmean(p.change.1, 5) ~ seq(5,26), col='blue', lwd=2)
  #legend("topleft", legend=c("% change from previous year", "5-year rolling mean % chg"), 
   #      lty=c(1,1), col=c('black', 'blue'), lwd=c(1,2))
  #dev.off()
}


# soilMoist
var="soilMoist"
soilMoist_yearly = 
  extract_ts(raster.path = file.path(path.base, "yearly", "soilMoist"), 
             shp = basins, 
             years = seq(1981, 2009), 
             var = "soilMoist", 
             row.nm = as.character(basins$name),
             out.nm = paste(path.out, var, "/basin_", var, "_km3_yearly.csv", sep=""),
             check.file = 0)

for(i in 1:16){
  png(paste("figures/spinup_clim_figures/soilMoist_avekm3Yr_", soilMoist_yearly[i,1], ".png", sep=""))
  plot(as.numeric(soilMoist_yearly[i,2:29]), type='l',
       main=paste(soilMoist_yearly[i,1], "ave soilMoist"), ylab = c("km3/yr"), xlab = c("Years from start"))
  dev.off()
}

ymin = min(as.numeric(soilMoist_yearly[,2:29]))
ymax = max(as.numeric(soilMoist_yearly[,2:29]))

for(i in 1:16){
  if(i==1){
    plot(as.numeric(soilMoist_yearly[i,2:29]), type='l', ylim = c(ymin, ymax))
  }
  lines(as.numeric(soilMoist_yearly[i,2:29]), col=i)
}

# percent change
per.chg = function(vec.x){
  p.vec = mat.or.vec(nr=length(vec.x)-1, nc=1)
  for(i in 1:length(vec.x)-1){
    p.vec[i] = 100*(vec.x[i+1] - vec.x[i])/vec.x[i]
  }
  p.vec
}

for(i in 1:16){
  p.change.1 = per.chg(as.numeric(soilMoist_yearly[i, 2:30]))
  if(i == 1){
    plot(p.change.1, type='l', ylim=c(-1.5,1.5))
  }else{  
    lines(p.change.1, col=i)
  }
  abline(h=0, col='black', lty=2)
}

for(i in 1:16){
  p.change.1 = per.chg(as.numeric(soilMoist_yearly[i, 3:30]))
  
  png(paste("figures/spinup_clim_figures/soilMoist_percent_chg_", soilMoist_yearly[i,1], ".png", sep=""))
  plot(p.change.1, type='l', ylim=c(-1.5,1.5), 
       main=paste(soilMoist_yearly[i,1], "soilMoist"), ylab = c("percent change"), xlab = c("Years from start"))
  abline(h=0, col='grey')
  #abline(lm(p.change.1~seq(1:27)), col='red', lty=2)
  lines(rollmean(p.change.1, 5) ~ seq(5,27), col='blue', lwd=2)
  legend("topleft", legend=c("% change from previous year", "5-year rolling mean % chg"), 
         lty=c(1,1), col=c('black', 'blue'), lwd=c(1,2))
  dev.off()
}


# grndWater_mm_pgi
var="grndWater_mm_pgi"
grndWater_pgi_yearly = 
  extract_ts(raster.path = file.path(path.base, "yearly", "grndWater_mm_pgi"), 
             shp = basins, 
             years = seq(1981, 2009), 
             var = "grndWater_mm_pgi", 
             row.nm = as.character(basins$name),
             out.nm = paste(path.out, var, "/basin_", var, "_km3_yearly.csv", sep=""),
             check.file = 0)

for(i in 1:16){
  png(paste("figures/spinup_clim_figures/grndWater_pgi_avekm3Yr_", grndWater_pgi_yearly[i,1], ".png", sep=""))
  plot(as.numeric(grndWater_pgi_yearly[i,2:29]), type='l',
       main=paste(grndWater_pgi_yearly[i,1], "ave grndWater_pgi"), ylab = c("km3/yr"), xlab = c("Years from start"))
  dev.off()
}

ymin = min(as.numeric(grndWater_pgi_yearly[,2:29]))
ymax = max(as.numeric(grndWater_pgi_yearly[,2:29]))

for(i in 1:16){
  if(i==1){
    plot(as.numeric(grndWater_pgi_yearly[i,2:29]), type='l', ylim = c(ymin, ymax))
  }
  lines(as.numeric(grndWater_pgi_yearly[i,2:29]), col=i)
}

# percent change
for(i in 1:16){
  p.change.1 = per.chg(as.numeric(grndWater_pgi_yearly[i, 3:30]))
  
  png(paste("figures/spinup_clim_figures/grndWater_pgi_percent_chg_", grndWater_pgi_yearly[i,1], ".png", sep=""))
  plot(p.change.1, type='l', ylim=c(-1.5,1.5), 
       main=paste(grndWater_pgi_yearly[i,1], "grndWater_pgi"), ylab = c("percent change"), xlab = c("Years from start"))
  abline(h=0, col='grey')
  #abline(lm(p.change.1~seq(1:27)), col='red', lty=2)
  lines(rollmean(p.change.1, 5) ~ seq(5,27), col='blue', lwd=2)
  legend("topleft", legend=c("% change from previous year", "5-year rolling mean % chg"), 
         lty=c(1,1), col=c('black', 'blue'), lwd=c(1,2))
  dev.off()
}


######################################################################################################################################

######################################################################################################################################
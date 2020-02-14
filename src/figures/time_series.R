# time_series()
### Figure for Glacier-Ag paper

# project: NASA HiMAT
# Danielle S Grogan

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rasterVis)
library(rgdal)
library(rgeos)
library(maptools)
library(maps)
library(abind)
library(RColorBrewer)


#######################################################################################################################################
### Time series plots

# plot time series for each basin
plot.dir = "figures/"
mod = "ERA_hist"
icemelt.y_hist = read.csv(file.path("results", mod.hist, paste(mod.hist, "_glacier_melt_basins_y.csv", sep="")), sep="")
basin.names =  as.character(icemelt.y_hist$Basin)

Year = seq(1980, 2016)
for(i in 1:nrow(icemelt.y)){
  
  y = as.numeric(icemelt.y_hist[i,2:ncol(icemelt.y_hist)])
  
  png(paste(plot.dir, "GlacierIceMelt_annual_ts/", mod.hist, "_", mod.fut, "_", rcp, "_glacierIceMelt_ts_", as.character(basin.names[i]), ".png", sep=""))
  plot(Year, y, type='l', ylab = "Melt (km3)", xlim = c(1980, 2099))
  
  lin.fit = summary(lm(as.numeric(icemelt.y_hist[i,2:ncol(icemelt.y_hist)]) ~ Year))
  if(sum(as.numeric(icemelt.y_hist[i,2:ncol(icemelt.y_hist)])) > 0){  # NB: Luni_ext has no melt
    
    if(lin.fit$coefficients[,4][2] < 0.01){
      abline(lin.fit, col='blue', lty=2)
      mtext(paste("Slope =", signif(lin.fit$coefficients[,1][2], 2)), side=3, adj = 1)
    }else{
      abline(lin.fit, col='grey', lty=2)
    }
    
  }
  
  dev.off()
}


#######################################################################################################################################

icemelt.y_hist = read.csv(file.path("results", mod.hist, paste(mod.hist, "_glacier_melt_basins_y.csv", sep="")), sep="")
icemelt.y_fut  = read.csv(file.path("results", mod.fut, rcp, paste(mod.fut, "_", rcp, "_glacier_melt_basins_y.csv", sep="")))

Year.h = seq(1980, 2016)
Year.f = seq(2000, 2099)

for(i in 1:nrow(icemelt.y_hist)){
  
  y.hist = as.numeric(icemelt.y_hist[i,2:ncol(icemelt.y_hist)])
  y.fut  = as.numeric(icemelt.y_fut[i,2:ncol(icemelt.y_fut)])
  
  png(paste(plot.dir, "GlacierIceMelt_annual_ts/glacierIceMelt_ts_", as.character(basin.names[i]), ".png", sep=""))
  plot(Year.h,  y.hist, type='l', ylab = "Melt (km3)", xlab = "Year",
       xlim = c(1980, 2099),
       ylim = c(min(c(y.hist, y.fut)), max(c(y.hist, y.fut))) )
  lines(Year.f, y.fut, col = 'darkorange')
  legend("topright", 
         lty = c(1,1), 
         col=c('black', 'darkorange'), 
         legend = c("Historical", "CanESM2 RCP 8.5"), 
         bty = 'n')
  dev.off()
}

#######################################################################################################################################
# yearly irrigation gross

irrGross.y_hist = read.csv(file.path("results", mod.hist, paste(mod.hist, "_basin_irrigationGross_1980_2016_yearly.csv", sep="")), sep=",")
irrGross.y_fut  = read.csv(file.path("results", mod.fut, rcp, paste(mod.fut, "_basin_irrigationGross_2006_2099_yearly.csv", sep="")), sep=",")

Year.h = seq(1980, 2016)
Year.f = seq(2006, 2099)

for(i in 1:nrow(icemelt.y_hist)){
  
  y.hist = as.numeric(irrGross.y_hist[i,2:ncol(irrGross.y_hist)])
  y.fut  = as.numeric(irrGross.y_fut[i,2:ncol(irrGross.y_fut)])
  
  png(paste(plot.dir, "GrossIrrigation_annual_ts/grossIrrigation_annual_ts_", as.character(basin.names[i]), ".png", sep=""))
  plot(Year.h,  y.hist, type='l', ylab = "Gross Irrigation (km3)", xlab = "Year",
       xlim = c(1980, 2099),
       ylim = c(min(c(y.hist, y.fut)), max(c(y.hist, y.fut))) )
  lines(Year.f, y.fut, col = 'darkorange')
  legend("topleft", 
         lty = c(1,1), 
         col=c('black', 'darkorange'), 
         legend = c("Historical", "CanESM2 RCP 8.5"), 
         bty = 'n')
  dev.off()
}

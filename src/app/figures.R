### Plots for Glacier-Ag paper

# project: NASA HiMAT
# Danielle S Grogan

library(raster)
library(rasterVis)
library(rgdal)
library(rgeos)
library(maptools)
library(maps)
library(abind)
library(RColorBrewer)

############################################################################################################
figure_3part = function(coastline, 
                        coastline.shaddow,
                        xl = c(59, 120), 
                        yl = c(9, 49), 
                        xticks,
                        yticks,
                        x.labs,
                        y.labs,
                        glacier.icemelt, 
                        GrossIrr.pgi, 
                        discharge.pgi, 
                        basins,
                        out.dir, 
                        out.nm){
  
  glacier.icemelt   = mask(glacier.icemelt, basins)
  GrossIrr.pgi      = mask(GrossIrr.pgi, basins)
  discharge.pgi     = mask(discharge.pgi, basins)
  
  png(paste(map.dir, out.nm, ".png", sep=""), 
      height=6, width=7, units = 'in', res=300)
  par(mar=c(7, 4, 0.5, 0.5))
  plot(coastline.shadow,  xlim = xl, ylim = yl, border='grey90', lwd=4)
  plot(coastline,         xlim = xl, ylim = yl, border='grey70', col='white',  lwd=1, add=T)
  plot(glacier.icemelt,   add=T, col = p1,     legend=F,          box=F, axes=T, las=1)
  plot(discharge.pgi,     add=T, col = blues2, legend=F, bty='n', box=F, axes=F)
  plot(GrossIrr.pgi,      add=T, col = g1,     legend=F, bty='n', box=F, axes=F)
  plot(basins, xlim = xl, ylim = yl, add=T,  lwd=0.8, border='grey15')
  
  # legend: NB - need to move legend to better position for publication. below plots?
  plot(glacier.icemelt,    add=T, col = p1, box=F, axes=T, las=1,
       legend.only=T, legend.width=0.4, horizontal=T, 
       smallplot=c(0.02, 0.28, 0.07, 0.09),
       axis.args=list(cex.axis=0.8),
       legend.args=list(text='Glacier ice melt (mm/year)', side=3, font=1, line=0.05, cex=0.8))
  
  plot(discharge.pgi, add=T, col = blues2, box=F, axes=T, las=1,
       legend.only=T, legend.width=0.4, horizontal=T, 
       smallplot=c(0.32, 0.62, 0.07, 0.09),
       axis.args=list(cex.axis=0.8),
       legend.args=list(text='Glacier ice melt in discharge (m3/s)', side=3, font=1, line=0.05, cex=0.8))
  
  plot(GrossIrr.pgi, add=T, col = g1, box=F, axes=T, las=1,
       legend.only=T, legend.width=0.4, horizontal=T, 
       smallplot=c(0.67, 0.97, 0.07, 0.09),
       axis.args=list(cex.axis=0.8),
       legend.args=list(text='Glacier ice melt in irrigation (mm/year)', side=3, font=1, line=0.05, cex=0.8))
  
  axis(side = 1,
       at = xticks,
       labels = x.labs)
  axis(side = 2, las=1,
       at = yticks,
       labels = y.labs)
  
  dev.off()
}
############################################################################################################

map.dir = "figures/"

### set up plot appearance

# spatial extent
xl = c(59, 120)
yl = c(9, 49)

# axis labels
xticks=seq(from=55, to=125, by=10)
yticks=seq(from=10, to=50, by=10)
x.labs <- parse(text = paste(xticks, "*degree ~ E", sep = ""))
y.labs <- parse(text = paste(yticks, "*degree ~ N", sep = ""))

# color scales 
purples<-colorRampPalette(c(brewer.pal(n=9, name='Purples')))(100)
p1 = purples[30:100]

greens<-colorRampPalette(c(brewer.pal(n=9,name='Greens')))(100)
g1 = greens[20:100]

blues2<-colorRampPalette(c("cadetblue2", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4"))(100)


# shapefiles for mapping
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
coastline = readOGR("data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basins))
coastline.shadow = shift(coastline, x= 0.15, y=-0.08)

# input
mod = "ERA_hist"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)

### calculate yearly climatology of glacier ice melt in mm ##################################################################################
# move to separate function later
glacier.icemelt.m = brick("/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset/ERA-Interim_c2_ba1_100sets_1980_2017_m.nc", 
                          varname = 'melt')
glacier.icemelt.y = stackApply(glacier.icemelt.m, 
                               indices = mapply(function(x,y) rep(x,y), x = 1:(nlayers(glacier.icemelt.m)/12), y = 12),
                               fun = sum)
glacier.icemelt.area = brick("/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset/ERA-Interim_c2_ba1_100sets_1980_2017_y.nc",
                             varname = 'area') # unit: km2
m3_to_km3 = 1e-9
km_to_mm = 1e6
glacier.icemelt.mm = km_to_mm*(glacier.icemelt.y*m3_to_km3 / glacier.icemelt.area)
glacier.icemelt.yc = calc(glacier.icemelt.mm, fun = mean, na.rm=T) # in mm/year

GrossIrr.pgi.yc  = raster(file.path(path.base, "climatology", "wbm_GrossIrr_pgi_yc.nc"))*365  # x365 to convert from ave to total
discharge.pgi.yc = raster(file.path(path.base, "climatology", "wbm_discharge_m3s_pgi_yc.nc"))
discharge.pgi.yc[discharge.pgi.yc < 0.05] = c(NA) # lower limit on discharge; else it covers all areas and other display items

out.nm = "ERA_hist_yc_pgi_map"

figure_3part(coastline, 
             coastline.shaddow,
             xl = c(59, 120), 
             yl = c(9, 49), 
             xticks,
             yticks,
             x.labs,
             y.labs,
             glacier.icemelt.yc, 
             GrossIrr.pgi.yc, 
             discharge.pgi.yc, 
             basins,
             out.dir = map.dir, 
             out.nm)


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

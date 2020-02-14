# map_3part()
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
  
  discharge.pgi[discharge.pgi < 1] = c(NA) # lower limit on discharge; else it covers all areas and other display items
  GrossIrr.pgi[GrossIrr.pgi < 0.01] = c(NA) # lower limit on discharge; else it covers all areas and other display items
  
  
  png(paste(map.dir, out.nm, ".png", sep=""), 
      height=6, width=7, units = 'in', res=300)
  par(mar=c(7, 4, 0.5, 0.5))
  plot(coastline.shadow,  xlim = xl, ylim = yl, border='grey90', lwd=4)
  plot(coastline,         xlim = xl, ylim = yl, border='grey70', col='white',  lwd=1, add=T)
  plot(glacier.icemelt,   add=T, col = p1,     legend=F,          box=F, axes=T, las=1)
  plot(GrossIrr.pgi,      add=T, col = g1,     legend=F, bty='n', box=F, axes=F)
  plot(discharge.pgi,     add=T, col = blues2, legend=F, bty='n', box=F, axes=F)
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

GrossIrr.pgi.yc  = raster(file.path(path.base, "climatology", "wbm_GrossIrr_mm_pgi_yc.nc"))*365  # x365 to convert from ave to total
discharge.pgi.yc = raster(file.path(path.base, "climatology", "wbm_discharge_m3s_pgi_yc.nc"))
discharge.pgi.yc[discharge.pgi.yc < 0.1] = c(NA) # lower limit on discharge; else it covers all areas and other display items

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


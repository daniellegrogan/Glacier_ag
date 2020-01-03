# glacier_ag()

# Main script for Glacier contributions to agriculture paper
# project: NASA HiMAT
# Danielle S Grogan

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(maps)
library(abind)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

# mouth_ts()
mouth_ts.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/mouth_ts.R", ssl.verifypeer=F)
eval(parse(text=mouth_ts.script))

# wbm_model_mean()
wbm_model_mean.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_model_mean.R", ssl.verifypeer=F)
eval(parse(text=wbm_model_mean.script))

# raster_time_ave()
raster_time_ave.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/raster_time_ave.R", ssl.verifypeer=F)
eval(parse(text=raster_time_ave.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)

#######################################################################################################################################
### MAIN ####
#######################################################################################################################################


# Inputs: same for all models, historical, rcps, etc: 
basin.shape = readOGR("data/basins_hma", "basins_hma")
netwk.path = "/net/nfs/zero/data3/WBM_TrANS/data"

# inputs needed to identify basin mouths
basin.ID = raster(file.path(netwk.path, "HiMAT_full_210_IDs_Subset.asc"))
up.area  = raster(file.path(netwk.path,"flowdirection210_upstrArea.asc"))

# find IDs for exorheic basins
ex.basins = basin.shape[basin.shape$name == "Ganges" |
                          basin.shape$name == "Mekong" |
                          basin.shape$name == "Irawaddy" |
                          basin.shape$name == "Luni_ext" |
                          basin.shape$name == "Indus" |
                          basin.shape$name == "Brahmaputra" |
                          basin.shape$name == "Salween" |
                          basin.shape$name == "Yangtze" |
                          basin.shape$name == "Yellow",]


mod = "ERA_hist"
basins = basin.shape
years = seq(2000, 2001)  # for testing

### File paths ###
path.base = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12"
path.out  = "results/"
path.yr   = file.path(path.base, mod, "yearly")
path.mo   = file.path(path.base, mod, "monthly")
path.c = file.path(path.base, mod, "climatology")
map.dir = "figures/"

### Contribution of water components to agriculture  ###
# NB  _pgi := glacier ice melt. 
#     _pgn := glacier non-ice runoff (e.g., rain on the glacier)

### Glacier water ice melt
# 1. annual
vars = c("irrigationGross",
         "GrossIrr_mm_pgi")
percent.nm = "GrossIrr_percent_pgi"
basin.agg.pgi = agg_contribution(path.yr, basins, vars, years, percent.nm) 
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrGross_pgi_yearly.csv", sep="")
write.table(basin.agg.pg, 
            out.nm,
            sep=",")

# 2. monthly (NB: this step takes a long time)
vars = c("irrigationGross",
         'GrossIrr_mm_pgi')
percent.nm = "GrossIrr_percent_pgi"
basin.agg.pgi.m = agg_contribution(path.mo, basins, vars, years, percent.nm) 
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrGross_pgi_monthly.csv", sep="")
write.table(basin.agg.pgi.m, 
            out.nm,
            sep=",")


# 3a. Calculate month of max km3 irr_pgi
irr_pgi_max_month = max_month(var.m = basin.agg.pgi.m,
                              var   = "pgi",
                              unit  = "km3")
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrGross_pgi_km3_month_max.csv", sep="")
write.table(irr_pgi_max_month, 
            out.nm,
            quote = F,
            sep=",")

# 3b. Calculate month of max % irr_pg
irr_pgi_max_month.percent = max_month(var.m = basin.agg.pgi.m,
                                      var   = "pgi",
                                      unit  = "percent")
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrGross_pgi_percent_month_max.csv", sep="")
write.table(irr_pgi_max_month, 
            out.nm,
            quote = F,
            sep=",")



# 4. monthly SW_pgi 
vars = c("irrigationGross",
         'IrrFlow_mm_pgi')
percent.nm = "GrossIrr_percent_SWpgi"
basin.agg.SW.pgi.m = agg_contribution(path.mo, basins, vars, years, percent.nm) 
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrGross_SWpgi_monthly.csv", sep="")
write.table(basin.agg.SW.pgi.m, 
            out.nm,
            sep=",")

# 4a. Calculate month of max km3 SW_pgi
SW_pgi_max_month = max_month(var.m = basin.agg.SW.pgi.m,
                              var   = "pgi",
                              unit  = "km3")
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrFlow_pgi_km3_month_max.csv", sep="")
write.table(SW_pgi_max_month, 
            out.nm,
            quote = F,
            sep=",")

# 4b. Calculate month of max % SW_pgi
SW_pgi_max_month.percent = max_month(var.m = basin.agg.SW.pgi.m,
                                      var   = "SWpgi",
                                      unit  = "percent")
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrFlow_pgi_percent_month_max.csv", sep="")
write.table(SW_pgi_max_month.percent, 
            out.nm,
            quote = F,
            sep=",")

# 5. monthly GW_pg 
vars = c("irrigationGross",
         'IrrGrwt_mm_pgi')
percent.nm = "GrossIrr_percent_GWpgi"
basin.agg.GW.pgi.m = agg_contribution(path.mo, basins, vars, years, percent.nm) 
out.nm = paste(path.out, mod, "/",  mod, "_basin_IrrGross_GWpgi_monthly.csv", sep="")
write.table(basin.agg.GW.pgi.m, 
            out.nm,
            sep=",")

# 5a. Calculate month of max km3 GW_pgi
GW_pgi_max_month = max_month(var.m = basin.agg.GW.pgi.m,
                             var   = "pgi",
                             unit  = "km3")
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrGW_GWpgi_km3_month_max.csv", sep="")
write.table(GW_pgi_max_month, 
            out.nm,
            quote = F,
            sep=",")

# 5b. Calculate month of max % SW_pgi
GW_pgi_max_month.percent = max_month(var.m = basin.agg.GW.pgi.m,
                                     var   = "GWpgi",
                                     unit  = "percent")
out.nm = paste(path.out, mod, "/", mod, "_basin_IrrGW_GWpgi_percent_month_max.csv", sep="")
write.table(GW_pgi_max_month.percent, 
            out.nm,
            quote = F,
            sep=",")

# Move plots to separate script?
### Map spatial extent of glacier runoff use for irr
# 
# # 1. all glacier runoff in irr in August
# irr.pg.all = subset(brick(file.path(path.c, "wbm_irrigationGross_mc.nc")), 8)
# irr.pg.all[irr.pg.all == 0] <-c(NA)
# irr.pg.all = mask(irr.pg.all, basins)
# out.nm = "ERA_hist_grossIrr_pg_yc.png"
# 
# xl = c(59, 120)
# yl = c(9, 49)
# 
# cols = colorRampPalette(c(brewer.pal(n=9, name='YlGn')))(100)
# 
# png(file.path(map.dir, out.nm), 
#     height=6, width=7, units = 'in', res=300)
# par(mar=c(3, 3.2,0,0), xpd=TRUE)
# 
# plot(coastline.shadow, xlim = xl, ylim = yl, border='grey90', lwd=4)
# plot(coastline,        xlim = xl, ylim = yl, border='grey70', col='white',  lwd=1, add=T)
# plot(irr.pg.all,   add=T, col = cols,     legend=F,          box=F, axes=T, las=1)
# #plot(glacier.shp,  add=T, col = adjustcolor(col = "grey", alpha.f = 0.5), legend=F, bty='n', box=F, axes=F)
# plot(basins, xlim = xl, ylim = yl, add=T,  lwd=0.8)
# 
# # legend: NB - need to move legend to better position for publication. below plots?
# plot(irr.pg.all,    add=T, col = cols, box=F, axes=T, las=1,
#      legend.only=T, legend.width=0.4, horizontal=T,
#      axis.args=list(cex.axis=1),
#      legend.args=list(text='Glacier runoff in Irrigation (mm/year)',
#                       side=3, font=1, line=0.1, cex=1))
# dev.off()

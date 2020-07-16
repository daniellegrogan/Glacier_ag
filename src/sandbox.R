# sandbox, check problems

library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(rasterVis)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)

######################################################################################################################################################
# 2020-06-17  evaluating historical results only

######################################################################################################################################################
# sub-basin regions
subbasins = raster("/net/nfs/zero/home/WBM_TrANS/data/watershed_regions/HiMAT_full_210_Subset/HiMAT_full_210_Subset_regions.asc")
plot(subbasins)

subbasins.poly = rasterToPolygons(subbasins, dissolve = T)
indus = basins[basins$name=="Indus",]
subbasins.indus = mask(subbasins, indus)
subbasins.indus.poly = rasterToPolygons(subbasins.indus, dissolve = T)
plot(subbasins.indus.poly)
######################################################################################################################################################
#

path = "results/grid_climatology"
var = "discharge_m3s_pgi"

q_base = brick(file.path(path, "discharge/ERA_hist_discharge_1980_2009_mc.nc"))
q_m3s_pgi = brick(file.path(path, var, paste("ERA_hist_", var, "_1980_2009_mc.nc", sep="")))

q_pgi_frac = q_m3s_pgi/q_base
names(q_pgi_frac) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
coastline = readOGR("data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basins))

png("figures/Historical/dicharge_pgi_fraction_1980_2009_mc.png", width = 1500, height=1000, res=130)
plot(q_pgi_frac)
plot(basins, add=T)
dev.off()

q_pgi_frac = mask(q_pgi_frac, basins)

png("figures/Historical/dicharge_pgi_fraction_1980_2009_mc.png", width = 1500, height=1000, res=130)
plt <- levelplot(q_pgi_frac, col.regions=colorRampPalette(brewer.pal(9,"YlGnBu"))(100))
plt + latticeExtra::layer(sp.lines(basins, col="black", lwd=0.5)) + latticeExtra::layer(sp.lines(coastline, col="black", lwd=0.5))
dev.off() 


irr_base = brick(file.path(path, "irrigationGross/ERA_hist_irrigationGross_1980_2009_mc.nc"))
irr_mm_pgi = brick(file.path(path, "GrossIrr_mm_pgi/ERA_hist_GrossIrr_mm_pgi_1980_2009_mc.nc"))

irr_pgi_frac = irr_mm_pgi/irr_base
names(irr_pgi_frac) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# png("figures/Historical/GrossIrr_pgi_fraction_1980_2009_mc.png", width = 1500, height=1000, res=130)
# plot(irr_pgi_frac)
# dev.off()

irr_pgi_frac[irr_base == 0] = NA
irr_pgi_frac = mask(irr_pgi_frac, basins)

png("figures/Historical/GrossIrr_pgi_fraction_1980_2009_mc.png", width = 1500, height=1000, res=130)
plt <- levelplot(irr_pgi_frac, col.regions=colorRampPalette(brewer.pal(9,"YlGnBu"))(100))
plt + latticeExtra::layer(sp.lines(basins, col="black", lwd=0.5)) + latticeExtra::layer(sp.lines(coastline, col="black", lwd=0.5))
dev.off() 

# scatter plot: for each grid cell with irrigation, plot gross irr vs fraction pgi 
irr_pgi_frac.list = getValues(irr_pgi_frac)
irr_base.list = getValues(irr_base)

png("figures/Historical/Scatter_IrrPgi_vs_IrrGross_1980_2009_mc.png", width = 1000, height=1000, res=130)
plot(irr_pgi_frac.list ~ irr_base.list, pch=19, cex=0.5, 
     main = "Monthly mean grid cell values",
     xlab = "Gross irrigation (mm/day)", ylab = "Fraction Glacier Ice Melt in Irr")
dev.off()

# hexbin plots
library(hexbin)
library(RColorBrewer)

irr_base.vec = as.vector(irr_base.list)
irr_pgi.vec = as.vector(irr_pgi_frac.list)

irr = cbind(irr_base.vec, irr_pgi.vec)
irr = subset(irr, irr[,1] != 0)
irr = subset(irr, !is.na(irr[,1]))
irr = subset(irr, !is.na(irr[,2]))

x = irr[,1]
y = irr[,2]
bin<-hexbin(x, y, xbins=80)
my_colors=colorRampPalette(rev(brewer.pal(11,'Spectral')))
png("figures/Historical/Hexbin_IrrPgi_vs_IrrGross_1980_2009_mc.png", width = 1000, height=1000, res=130)
gplot.hexbin(bin,  colorcut = colorcut, 
             xlab = "Gross irrigation (mm/day)", 
             ylab = "Fraction Glacier Ice Melt in Irrigation",
             main = "Glacier Ice Melt in Irrigation: grid cells, all basins")
dev.off()

basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over

for(b in 1:15){
  irr_base_basin = extract(irr_base, basins[b,])[[1]]
  irr_pgi_frac_basin = extract(irr_pgi_frac, basins[b,])[[1]]
  
  irr_base.vec = as.vector(irr_base_basin)
  irr_pgi.vec = as.vector(irr_pgi_frac_basin)
  
  irr = cbind(irr_base.vec, irr_pgi.vec)
  irr = subset(irr, irr[,1] != 0)
  irr = subset(irr, !is.na(irr[,1]))
  irr = subset(irr, !is.na(irr[,2]))
  
  x = irr[,1]
  y = irr[,2]
  bin<-hexbin(x, y, xbins=80)
  colorcut = c(0, 1/1000, 1/100, 1/10, 1)
  png(paste("figures/Historical/", basins$name[b], "_Hexbin_IrrPgi_vs_IrrGross_1980_2009_mc.png", sep=""), 
      width = 1200, height=1000, res=130)
  gplot.hexbin(bin,  colorcut = colorcut, 
               xlab = "Gross irrigation (mm/day)", 
               ylab = "Fraction Glacier Ice Melt in Irrigation",
               main = paste(basins$name[b],"monthly mean grid cell values"))
  dev.off()
}

# hexbin by basin


######################################################################################################################################################
mod.hist = "ERA_hist"
irrGross_pgi_y_stats  = read.csv(file.path("results", mod.hist, paste(mod.hist, "_basin_GrossIrr_pgi_1980_2016_yearly_stats.csv", sep="")), sep=",")
irrFlow_pgi_y_stats   = read.csv(file.path("results", mod.hist, paste(mod.hist, "_basin_IrrFlow_pgi_1980_2016_yearly_stats.csv", sep="")), sep=",")
irrGrwt_pgi_y_stats  = read.csv(file.path("results", mod.hist, paste(mod.hist, "_basin_IrrGrwt_pgi_1980_2016_yearly_stats.csv", sep="")), sep=",")


irrGross_pgi_y = read.csv("results/ERA_hist/ERA_hist_basin_GrossIrr_km3_pgi_1980_2016_yearly.csv")
irrFlow_pgi_y  = read.csv("results/ERA_hist/ERA_hist_basin_IrrFlow_km3_pgi_1980_2016_yearly.csv")
irrGrwt_pgi_y  = read.csv("results/ERA_hist/ERA_hist_basin_IrrGrwt_km3_pgi_1980_2016_yearly.csv")

# test 1980
irrGross_pgi = irrGross_pgi_y[,1:2]
irrFlow_pgi  = irrFlow_pgi_y[,1:2]
irrGrwt_pgi  = irrGrwt_pgi_y[,1:2]

test.sum = irrFlow_pgi[,2] + irrGrwt_pgi[,2]

diff = irrGross_pgi[,2] - test.sum
diff.percent = 100*(diff/irrGross_pgi[,2])



# grid cell level
irr.gross.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgi/wbm_1980.nc")
irr.flow.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pgi/wbm_1980.nc")
irr.grwt.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pgi/wbm_1980.nc")

test.sum = irr.flow.pgi*3645 + irr.grwt.pgi*365
test.diff = irr.gross.pgi*365 - test.sum
test.diff.percent = 100*(test.diff/irr.gross.pgi)

plot(test.diff * (test.diff > 50))



irr.gross = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGross/wbm_1980.nc")
irr.flow = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationFlow/wbm_1980.nc")
irr.grwt = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGrwt/wbm_1980.nc")
irr.ugw  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationExtra/wbm_1980.nc")

test.diff2 = irr.gross - (irr.flow + irr.grwt + irr.ugw)


# test sum of components

# yearly
irr.flow.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pgi/wbm_1980.nc")
irr.flow.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pgn/wbm_1980.nc")
irr.flow.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pr/wbm_1980.nc")
irr.flow.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_ps/wbm_1980.nc")
irr.flow.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pu/wbm_1980.nc")

irr.flow = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationFlow/wbm_1980.nc")

test.flow = irr.flow*365 - (irr.flow.pgi + irr.flow.pgn + irr.flow.pr + irr.flow.ps + irr.flow.pu)*365


irr.gw.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pgi/wbm_1980.nc")
irr.gw.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pgn/wbm_1980.nc")
irr.gw.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pr/wbm_1980.nc")
irr.gw.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_ps/wbm_1980.nc")
irr.gw.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pu/wbm_1980.nc")

irr.gw = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGrwt/wbm_1980.nc")

test.gw = irr.gw*365 - (irr.gw.pgi + irr.gw.pgn + irr.gw.pr + irr.gw.ps + irr.gw.pu)*365
test[test==0]=NA
plot(test>50)


irr.gross.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgi/wbm_1980.nc")
irr.gross.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgn/wbm_1980.nc")
irr.gross.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pr/wbm_1980.nc")
irr.gross.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_ps/wbm_1980.nc")
irr.gross.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pu/wbm_1980.nc")

irr.gross = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGross/wbm_1980.nc")

test.gross = irr.gross*365 - (irr.gross.pgi + irr.gross.pgn + irr.gross.pr + irr.gross.ps + irr.gross.pu)*365
test[test==0]=NA
plot(test)


# frac
irr.gross.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgi/wbm_1980.nc")
irr.gross.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgn/wbm_1980.nc")
irr.gross.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pr/wbm_1980.nc")
irr.gross.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_ps/wbm_1980.nc")
irr.gross.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pu/wbm_1980.nc")

sum.frac = irr.gross.pgi + irr.gross.pgn + irr.gross.pr + irr.gross.ps + irr.gross.pu

# daily




# test fraction
irr.gross = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGross/wbm_1980.nc")
irr.flow = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationFlow/wbm_1980.nc")
irr.grwt = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGrwt/wbm_1980.nc")
irr.ugw  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationExtra/wbm_1980.nc")

irr.gross.pgi.frac = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgi/wbm_1980.nc")

irr.gross.pgi.mm = irr.gross * irr.gross.pgi.frac



test = irr.gross.pgi.mm - irr.gross.pgi

test.diff2 = irr.gross.pgi.mm - (irr.flow.pgi + irr.grwt.pgi)




##################

# compare reported mm vs calculated mm

irr.gross = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGross/wbm_1980.nc")

# pgi
irr.gross.mm.pgi  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgi/wbm_1980.nc")
irr.gros.frac.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgi/wbm_1980.nc")

irr.gross.mm.pgi.calc = irr.gros.frac.pgi*irr.gross
diff.pgi = irr.gross.mm.pgi.calc - irr.gross.mm.pgi

# pgn
irr.gross.mm.pgn  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgn/wbm_1980.nc")
irr.gros.frac.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgn/wbm_1980.nc")

irr.gross.mm.pgn.calc = irr.gros.frac.pgn*irr.gross
diff.pgn = irr.gross.mm.pgn.calc - irr.gross.mm.pgn

# ps
irr.gross.mm.ps  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_ps/wbm_1980.nc")
irr.gros.frac.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_ps/wbm_1980.nc")

irr.gross.mm.ps.calc = irr.gros.frac.ps*irr.gross
diff.ps = irr.gross.mm.ps.calc - irr.gross.mm.ps

# pr
irr.gross.mm.pr  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pr/wbm_1980.nc")
irr.gros.frac.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pr/wbm_1980.nc")

irr.gross.mm.pr.calc = irr.gros.frac.pr*irr.gross
diff.pr = irr.gross.mm.pr.calc - irr.gross.mm.pr

# pu
irr.gross.mm.pu  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pu/wbm_1980.nc")
irr.gros.frac.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pu/wbm_1980.nc")

irr.gross.mm.pu.calc = irr.gros.frac.pu*irr.gross
diff.pu = irr.gross.mm.pu.calc - irr.gross.mm.pu

### check fraction and mm sums
frac.sum = irr.gros.frac.pgi + irr.gros.frac.pgn + irr.gros.frac.pr + irr.gros.frac.ps + irr.gros.frac.pu


### monthly? will load Jan
irr.gross = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname = "irrigationGross")

# pgi
irr.gross.mm.pgi  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_pgi")
irr.gros.frac.pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_pgi")

irr.gross.mm.pgi.calc = irr.gros.frac.pgi*irr.gross
diff.pgi = irr.gross.mm.pgi.calc - irr.gross.mm.pgi
diff.pgi[diff.pgi==0]=NA
plot(diff.pgi * abs(diff.pgi>1))

# pgn
irr.gross.mm.pgn  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_pgn")
irr.gros.frac.pgn = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_pgn")

irr.gross.mm.pgn.calc = irr.gros.frac.pgn*irr.gross
diff.pgn = irr.gross.mm.pgn.calc - irr.gross.mm.pgn
diff.pgn[diff.pgn==0]=NA
plot(diff.pgn * abs(diff.pgn>1))

# ps
irr.gross.mm.ps  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_ps")
irr.gros.frac.ps = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_ps")

irr.gross.mm.ps.calc = irr.gros.frac.ps*irr.gross
diff.ps = irr.gross.mm.ps.calc - irr.gross.mm.ps
diff.ps[diff.ps==0]=NA
plot(diff.ps * (abs(diff.ps)>1))

# pr
irr.gross.mm.pr  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_pr")
irr.gros.frac.pr = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_pr")

irr.gross.mm.pr.calc = irr.gros.frac.pr*irr.gross
diff.pr = irr.gross.mm.pr.calc - irr.gross.mm.pr
diff.pr[diff.pr==0]=NA
plot(diff.pr * (abs(diff.pr)>1))

# pu
irr.gross.mm.pu  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_pu")
irr.gros.frac.pu = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_pu")

irr.gross.mm.pu.calc = irr.gros.frac.pu*irr.gross
diff.pu = irr.gross.mm.pu.calc - irr.gross.mm.pu
diff.pu[diff.pu==0]=NA
plot(diff.pu * (abs(diff.pu)>1))

### check fraction and mm sums
frac.sum = irr.gros.frac.pgi + irr.gros.frac.pgn + irr.gros.frac.pr + irr.gros.frac.ps + irr.gros.frac.pu

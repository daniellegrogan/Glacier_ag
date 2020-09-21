# agriculture_data()
# summarize crop area, production, and yield by subbasin

# project: NASA HiMAT
# Danielle S Grogan

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(RColorBrewer)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))


#######################################################################################################################################
subbasin_gaez = function(p){
  
    file.list = list.files(p, pattern = ".tif", full.names = T)
    
    # load all individual crop data
    data.brk = do.call(stack,
                       lapply(file.list, 
                              raster)
    )
    # sum data
    data.sum = stackApply(data.brk, indices=rep(1, length(file.list)), fun = sum)
    
    # spatial aggregation to subbasins
    sp.agg = extract(data.sum, subbasins.poly, fun=sum, na.rm=T, sp=T)
    sp.agg
}
#######################################################################################################################################
subbasin_50p = function(sp.agg){
  
  # Table: sort in descending order, identify percentage of total by subbasin
  data.subbasin = as.data.frame(sp.agg@data)
  data.subbasin = data.subbasin[order(-data.subbasin$index_1),] 
  
  cs = cumsum(data.subbasin$index_1)
  cs.frac = cs/sum(data.subbasin$index_1)
  data.subbasin = cbind(data.subbasin, cs.frac)
  
  # identify 50% total harvested area
  cs.p50 = which(cs.frac <= 0.5)
  subbasins.cs.p50 = data.subbasin$HMAT__2[cs.p50]
  
  # identify shapefiles with 50% total harvested area
  sp.agg.p50 = sp.agg[sp.agg$HMAT__2 %in% subbasins.cs.p50,]
  sp.agg.p50
}

#######################################################################################################################################
# shapefiles for spatial agg and plotting
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
subbasins.poly = readOGR(dsn = "data/subbasin_poly/", layer = "subbasin_poly")
coastline = readOGR("data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basins))

# other plotting items
basin.layer = list("sp.polygons", basins, col = "grey", first=FALSE)
coastline.layer = list("sp.polygons", coastline, col = "darkgrey", lwd=1.5, first=TRUE)
green.pal = colorRampPalette(brewer.pal(n = 7, name = "Greens"))(20)
green.pal = c('white', green.pal)
#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

#### Harvested Area (1000 HA)
p = "/net/nfs/squam/raid/data/GLEAM/HarvardDataverse_GAEZ+_2015/GAEZ+_2015_crop_harvest_area"
sp.agg = subbasin_gaez(p)
writeOGR(sp.agg, dsn = "data/subbasin_poly_HarvArea50/", layer = "subbasin_poly_HarvArea50", driver = "ESRI Shapefile")

# plot
png("figures/Historical/Harvested_area_SUBBASIN_1000ha.png", width = 1500, height=1000, res=130)
spplot(sp.agg, zcol=2, sp.layout = list(basin.layer, coastline.layer), col.regions = green.pal, col = "transparent", as.table=TRUE)
dev.off()

# 50% harvested area
sp.agg.p50 = subbasin_50p(sp.agg)
writeOGR(sp.agg.p50, dsn = "data/subbasin_poly_HarvArea50p/", layer = "subbasin_poly_HarvArea50p", driver = "ESRI Shapefile")

# plot
png("figures/Historical/Harvested_area_SUBBASIN_1000ha_50percent.png", width = 1500, height=1000, res=130)
spplot(sp.agg.p50, zcol=2, sp.layout = list(basin.layer, coastline.layer), 
       col.regions = green.pal, col = "transparent", as.table=TRUE, 
       xlim=c(57.4, 120.6), ylim=c(9.9, 49.4))
dev.off()



#### Productivity (1000 tonnes)
p = "/net/nfs/squam/raid/data/GLEAM/HarvardDataverse_GAEZ+_2015/GAEZ+_2015_crop_production/"
sp.agg = subbasin_gaez(p)
writeOGR(sp.agg, dsn = "data/subbasin_poly_Production/", layer = "subbasin_poly_Production", driver = "ESRI Shapefile")

# plot
png("figures/Historical/subbasin_poly_Production_SUBBASIN_1000tonnes.png", width = 1500, height=1000, res=130)
spplot(sp.agg, zcol=2, sp.layout = list(basin.layer, coastline.layer), col.regions = green.pal, col = "transparent", as.table=TRUE)
dev.off()

# 50% harvested area
sp.agg.p50 = subbasin_50p(sp.agg)
writeOGR(sp.agg.p50, dsn = "data/subbasin_poly_Production50p/", layer = "subbasin_poly_Production50p", driver = "ESRI Shapefile")

# plot
png("figures/Historical/Production_SUBBASIN_tonnes_50percent.png", width = 1500, height=1000, res=130)
spplot(sp.agg.p50, zcol=2, sp.layout = list(basin.layer, coastline.layer), 
       col.regions = green.pal, col = "transparent", as.table=TRUE, 
       xlim=c(57.4, 120.6), ylim=c(9.9, 49.4))
dev.off()


#### Crop Yield (tonnes per kha)
p = "/net/nfs/squam/raid/data/GLEAM/HarvardDataverse_GAEZ+_2015/GAEZ+_2015_crop_yield/"
file.list = list.files(p, pattern = ".tif", full.names = T)

# load all individual crop data
data.brk = do.call(stack,
                   lapply(file.list, 
                          raster)
)
# sum data
data.sum = stackApply(data.brk, indices=rep(1, length(file.list)), fun = sum)

# spatial aggregation to subbasins
sp.agg = extract(data.sum, subbasins.poly, fun=sum, na.rm=T, sp=T)

# plot
green.pal = colorRampPalette(brewer.pal(n = 7, name = "Greens"))(20)
green.pal = c('white', green.pal)

png("figures/Historical/Crop_yield_SUBBASIN_1000tonnes_per_kha.png", width = 1500, height=1000, res=130)
spplot(sp.agg, zcol=2, sp.layout = list(basin.layer, coastline.layer), col.regions = green.pal, col = "transparent", as.table=TRUE)
dev.off()

# Table: sort in descending order, identify percentage of total by subbasin
data.subbasin = as.data.frame(sp.agg@data)
data.subbasin = data.subbasin[order(-data.subbasin$index_1),] 

cs = cumsum(data.subbasin$index_1)
cs.frac = cs/sum(data.subbasin$index_1)
data.subbasin = cbind(data.subbasin, cs.frac)

# identify 50% total production
cs.p50 = which(cs.frac <= 0.5)
subbasins.cs.p50 = data.subbasin$HMAT__2[cs.p50]

# identify shapefiles with 50% total production
sp.agg.p50 = sp.agg[sp.agg$HMAT__2 %in% subbasins.cs.p50,]
writeOGR(sp.agg.p50, dsn = "data/subbasin_poly_Yield50p/", layer = "subbasin_poly_Yield50p", driver = "ESRI Shapefile")

green.pal = colorRampPalette(brewer.pal(n = 7, name = "Greens"))(20)
png("figures/Historical/Crop_yield_SUBBASIN_1000tonnes_per_kha_50percent.png", width = 1500, height=1000, res=130)
spplot(sp.agg.p50, zcol=2, sp.layout = list(basin.layer, coastline.layer), 
       col.regions = green.pal, col = "transparent", as.table=TRUE, 
       xlim=c(57.4, 120.6), ylim=c(9.9, 49.4))
dev.off()

##########################################################################################################################################

# calculate irrigation vars for key subbasins
sub_harv_50 = readOGR(dsn = "data/subbasin_poly_HarvArea50p/", layer = "subbasin_poly_HarvArea50p")
sub_prod_50 = readOGR(dsn = "data/subbasin_poly_Production50p/", layer = "subbasin_poly_Production50p")

png("figures/Historical/Prod50grey_Harv50red_subbasins.png", width = 1500, height=1000, res=130)
plot(basins)
plot(sub_prod_50, col=adjustcolor("darkgrey", alpha.f = 0.99), add=T)
plot(sub_harv_50, col=adjustcolor("red", alpha.f = 0.3), add=T)
plot(coastline, add=T)
plot(basins, border='darkgrey', add=T)
dev.off()

# combine the two shapefiles
harv_prod_50 = union(sub_harv_50, sub_prod_50)
plot(harv_prod_50)

# spatial aggregation
vars = c("irrigationGross", "GrossIrr_mm_pgi",  "runoff", "runoff_mm_pgi") 
lapply(vars, FUN = function(var){create_dir(file.path("results/p50_subbasins", var))})

mod = "ERA_hist"
path.out  = "results/p50_subbasins"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)
years = seq(1980, 2009)  # climatology historical time series

# monthly climatology
month.data = read.csv("data/days_in_months.csv")

for(var in vars){
  # check if file exists
  check.file = paste(path.out, "/", var, "/", mod, "_p50subbasins_", var, "_km3_mc.csv", sep="")
  if(!exists(check.file)){
    sp.agg = spatial_aggregation(raster.data = brick(paste(path.base, "/climatology/wbm_", var, "_mc.nc", sep=""))*month.data$days,
                                 shapefile = harv_prod_50)
    colnames(sp.agg@data)[2:13] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    out.nm = paste(path.out, "/", var, "/", mod, "_p50subbasins_", var, "_km3_mc.csv", sep="")
    write.csv(sp.agg@data, out.nm, row.names = F)
  }
  print(var)
}

# fraction irrGross as pgi
irrGross = read.csv("results/p50_subbasins/irrigationGross/ERA_hist_p50subbasins_irrigationGross_km3_mc.csv")
irr_pgi  = read.csv("results/p50_subbasins/GrossIrr_mm_pgi/ERA_hist_p50subbasins_GrossIrr_mm_pgi_km3_mc.csv")

#frac = irr_pgi[,2:] / irrGross

##########################################################################################################################################

# plot cumulative productivity and GrossIrr_pgi
prod = readOGR(dsn = "data/subbasin_poly_Production/", layer = "subbasin_poly_Production")

# spatial aggregation: GrossIrr_pgi as km3/yr
var = c("GrossIrr_mm_pgi") 
mod = "ERA_hist"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)
sp.agg.pgi = spatial_aggregation(raster.data = brick(paste(path.base, "/climatology/wbm_", var, "_yc.nc", sep=""))*365,
                             shapefile = prod)

# spatial aggregation: irrigation Gross
var = c("irrigationGross") 
mod = "ERA_hist"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)
sp.agg.irrGross = spatial_aggregation(raster.data = brick(paste(path.base, "/climatology/wbm_", var, "_yc.nc", sep=""))*365,
                             shapefile = prod)

# GrossIrr_pgi as fraction of irrigationGross
sp.agg.pgi_frac = sp.agg.pgi$layer/sp.agg.irrGross$layer

# combine all data in one shapefile
sp.agg = sp.agg.pgi
colnames(sp.agg@data) = c("Subbasin_index", "Production", "IrrGross_km3_pgi")
sp.agg$IrrGross = sp.agg.irrGross$layer
sp.agg$pgi_frac = sp.agg.pgi_frac

# Table: sort in descending order by Crop Production, identify percentage of total Production by subbasin
data.subbasin = as.data.frame(sp.agg@data)
data.subbasin = data.subbasin[order(-data.subbasin$Production),] 

cs.prod = cumsum(data.subbasin$Production)
cs.prod.frac = cs.prod/sum(data.subbasin$Production)

cs.pgi = cumsum(data.subbasin$IrrGross_km3_pgi)
cs.pgi.frac = cs.pgi/sum(data.subbasin$IrrGross_km3_pgi)

data.subbasin = cbind(data.subbasin, cs.prod.frac, cs.pgi.frac)
cs.p50 = which(cs.prod.frac <= 0.5)
cs.p75 = which(cs.prod.frac <= 0.75)
#cs.p90 = which(cs.prod.frac <= 0.9)
cs.p95 = which(cs.prod.frac <= 0.95)


######## plot

# cumulative sum of Crop Production and GrossIrr_pgi
png("figures/Subbasin_cumulative_CropProd_GrossIrrPgi.png", width = 1200, height=1200, res=200)
plot(data.subbasin$cs.prod.frac, type='l', yaxt='n', ylab = "Cumulative Fraction", xlab = "Number of Sub-basins")
axis(2, at = c(0.25, 0.5, 0.75, 0.95, 1.0), las=2)
lines(data.subbasin$cs.pgi.frac ~ seq(1:nrow(data.subbasin)), col='cadetblue3')
legend("bottomright", legend = c("Crop production", "Glacier ice melt in gross irrigation"), 
       lty=c(1,1), col=c("black", "cadetblue3"), bty='n')

clip(-30,cs.p95[max(cs.p95)], 0, 0.95)
abline(h=0.95, lty=4, col='darkgrey')
abline(v=cs.p95[max(cs.p95)], lty=4, col='darkgrey')

clip(-30,cs.p75[max(cs.p75)], 0, 0.75)
abline(h=0.75, lty=3, col='darkgrey')
abline(v=cs.p75[max(cs.p75)], lty=3, col='darkgrey')

clip(-30,cs.p50[max(cs.p50)], 0, 0.5)
abline(h=0.5, lty=2, col='grey')
abline(v=cs.p50[max(cs.p50)], lty=2, col='grey', )
dev.off()


# scatter plot
subbasins.prod.p50 = data.subbasin[cs.p50,]
subbasins.prod.p75 = data.subbasin[cs.p75,]
subbasins.prod.p95 = data.subbasin[cs.p95,]

png("figures/Subbasin_CropProd_vs_GrossIrrPgi.png", width = 1200, height=1200, res=200)
plot((100*data.subbasin$pgi_frac) ~ data.subbasin$Production, pch=19, col='grey75', cex=0.25,
     xlab = c("Crop Production (1000 tonnes)"), ylab = c("Glacier Ice Melt in Irrigation (%)"))
points((100*subbasins.prod.p95$pgi_frac) ~ subbasins.prod.p95$Production, pch=19, col='grey70', cex=0.5)
points((100*subbasins.prod.p75$pgi_frac) ~ subbasins.prod.p75$Production, pch=19, col='grey40', cex=0.75)
points((100*subbasins.prod.p50$pgi_frac) ~ subbasins.prod.p50$Production, pch=19, col='black')
legend("topright", legend=c("50% Total Production", "75% Total Production", "95% Total Production", "100% Total Production"),
       pch=c(19,19,19,19), pt.cex=c(1, 0.75, 0.5,0.25), col=c("black", "grey40", "grey70", "grey75"))
dev.off()

# calculate some statics to report in paper
max(subbasins.prod.p50$pgi_frac)
max(subbasins.prod.p75$pgi_frac)
max(subbasins.prod.p95$pgi_frac)

sum(data.subbasin$pgi_frac > 0.1, na.rm=T)


# sub-basin monthly stats
grossIrr_pgi_km3 = read.csv("results/subbasin/GrossIrr_mm_pgi/ERA_hist_subbasin_GrossIrr_mm_pgi_km3_mc.csv")
irrGross_km3 = read.csv("results/subbasin/irrigationGross/ERA_hist_subbasin_irrigationGross_km3_mc.csv")
grossIrr_pgi_frac = grossIrr_pgi_km3[,2:13]/irrGross_km3[,2:13]

max.mo = apply(grossIrr_pgi_frac, c(1), FUN=max, na.rm=T)
max.mo[max.mo=='-Inf'] = NA
sum(max.mo > 0.1, na.rm=T)
sum(max.mo > 0.2, na.rm=T)

max(max.mo, na.rm=T)
max.mo.sub = subset(max.mo, max.mo > 0.1, na.rm=T)
png("figures/Historical/Subbasin_MaxMonth_GrossIrr_pgi_fraction_historgram.png", width = 1500, height=1200, res=200)
hist(max.mo.sub, main="", ylim=c(0,70), xaxt='n', labels=T,
     xlab = c("Maximum Monthly Glacier Ice Melt Contribution to Sub-basin Irrigation (fraction)"),
     ylab = c("Number of Sub-basins"))
axis(1, at = seq(0.1, 1.0, by=0.1), las=1)
dev.off()

subbasins.max.mo = prod
prod$max_mo = max.mo

green.pal = colorRampPalette(brewer.pal(n = 7, name = "Greens"))(20)
png("figures/Historical/Subbasin_MaxMonth_GrossIrr_pgi_fraction.png", width = 1500, height=1000, res=130)
spplot(prod, zcol="max_mo", sp.layout = list(basin.layer, coastline.layer), 
       col.regions = green.pal, col = "transparent", as.table=TRUE, 
       xlim=c(57.4, 120.6), ylim=c(9.9, 49.4))
dev.off()

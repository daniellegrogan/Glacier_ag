# subbasin_aggregation()

# aggregate WBM results by subbasin

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(maptools)
library(maps)
library(abind)
library(RColorBrewer)

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


#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

# shapefiles for spatial agg
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over

subbasins = raster("/net/nfs/zero/home/WBM_TrANS/data/watershed_regions/HiMAT_full_210_Subset/HiMAT_full_210_Subset_regions.asc")
subbasins = mask(subbasins, basins)
subbasins.poly = rasterToPolygons(subbasins, dissolve = T)
crs(subbasins.poly) = crs(basins)

coastline = readOGR("data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basins))

# spatial aggregation
vars = c("irrigationGross", "GrossIrr_mm_pgi",  "runoff", "runoff_mm_pgi")                                                             # other useful vars
lapply(vars, FUN = function(var){create_dir(file.path("results/subbasin", var))})

mod = "ERA_hist"
path.out  = "results/subbasin"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)
years = seq(1980, 2009)  # climatology historical time series

yearly.agg = lapply(vars, function(var) extract_ts(raster.path = file.path(path.base, "yearly", var), 
                                                   shp = subbasins.poly, 
                                                   years, 
                                                   var, 
                                                   row.nm = as.character(subbasins.poly$HiMAT_full_210_Subset_regions),
                                                   out.nm = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep=""),
                                                   check.file = 0))

# test with loop
for(var in vars){
  extract_ts(raster.path = file.path(path.base, "yearly", var), 
             shp = subbasins.poly, 
             years, 
             var, 
             row.nm = as.character(subbasins.poly$HiMAT_full_210_Subset_regions),
             out.nm = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep=""),
             check.file = 0)
  print(var)
}

yc.agg = lapply(vars, function(var) yearly_to_yc(data.y = read.csv(paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")),
                                                 years = seq(1980,2009),
                                                 out.nm = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_",  min(years), "_", max(years), "_yc.csv", sep="")))


# yearly climatology
for(var in vars){
  sp.agg = spatial_aggregation(raster.data = raster(paste(path.base, "/climatology/wbm_", var, "_yc.nc", sep=""))*365,
                      shapefile = subbasins.poly)
  colnames(sp.agg@data)[2] = paste(var, "_km3", sep="")
  out.nm = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_yc.csv", sep="")
  write.csv(sp.agg@data, out.nm, row.names = F)
  print(var)
}

basin.layer = list("sp.polygons", basins, col = "lightgrey", first=FALSE)
coastline.layer = list("sp.polygons", coastline, col = "darkgrey", lwd=1.5, first=TRUE)
spplot(sp.agg, "irrigationGross_km3", sp.layout = list(basin.layer, coastline.layer), col = "transparent")


# monthly climatology
month.data = read.csv("data/days_in_months.csv")

for(var in vars){
  sp.agg = spatial_aggregation(raster.data = brick(paste(path.base, "/climatology/wbm_", var, "_mc.nc", sep=""))*month.data$days,
                               shapefile = subbasins.poly)
  colnames(sp.agg@data)[2:13] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  out.nm = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_mc.csv", sep="")
  write.csv(sp.agg@data, out.nm, row.names = F)
  print(var)
}
irrGross = sp.agg

basin.layer = list("sp.polygons", basins, col = "grey", first=FALSE)
coastline.layer = list("sp.polygons", coastline, col = "darkgrey", lwd=1.5, first=TRUE)
red.pal = colorRampPalette(brewer.pal(n = 7, name = "OrRd"))(20)

png("figures/Historical/irrGross_SUBBASINS_1980_2009_mc.png", width = 1500, height=1000, res=130)
spplot(irrGross, zcol=seq(2,13), sp.layout = list(basin.layer, coastline.layer), col.regions = red.pal, col = "transparent", as.table=TRUE)
dev.off()

# spplot(sp.agg, zcol=2, sp.layout = list(basin.layer, coastline.layer), col.regions = red.pal, col = "transparent", as.table=TRUE)

irr.pgi = spatial_aggregation(raster.data = brick(file.path(path.base, "climatology/wbm_GrossIrr_mm_pgi_mc.nc"))*month.data$days,
                           shapefile = subbasins.poly)
colnames(sp.agg@data)[2:13] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
png("figures/Historical/irrGross_pgi_km3_SUBBASINS_1980_2009_mc.png", width = 1500, height=1000, res=130)
spplot(irr.pgi, zcol=seq(2,13), sp.layout = list(basin.layer, coastline.layer), col.regions = red.pal, col = "transparent", as.table=TRUE)
dev.off()


irr.pgi.frac = irr.pgi@data[,2:13]/irrGross@data[,2:13]
sp.ob = irrGross
sp.ob@data[,2:13] = irr.pgi.frac

png("figures/Historical/irrGross_pgi_fraction_SUBBASINS_1980_2009_mc.png", width = 1500, height=1000, res=130)
spplot(sp.ob, zcol=seq(2,13), sp.layout = list(basin.layer, coastline.layer), col.regions = red.pal, col = "transparent", as.table=TRUE)
dev.off()

sp.ob@data[,2:13] > 0.5
#spplot(sp.ob, zcol=8,         sp.layout = list(basin.layer, coastline.layer), col.regions = red.pal, col = "transparent", as.table=TRUE)


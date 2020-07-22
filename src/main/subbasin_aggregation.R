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

# subbasins = raster("/net/nfs/zero/home/WBM_TrANS/data/watershed_regions/HiMAT_full_210_Subset/HiMAT_full_210_Subset_regions.asc")
# subbasins = mask(subbasins, basins)
# subbasins.poly = rasterToPolygons(subbasins, dissolve = T)
# crs(subbasins.poly) = crs(basins)
# writeOGR(subbasins.poly, dsn = "data/subbasin_poly/", layer = "subbasin_poly", driver = "ESRI Shapefile")

subbasins.poly = readOGR(dsn = "data/subbasin_poly/", layer = "subbasin_poly")

indus = basins[basins$name == "Indus",]
sub.indus = crop(subbasins.poly, indus)

coastline = readOGR("data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basins))

# spatial aggregation
vars = c("irrigationGross", "GrossIrr_mm_pgi",  "runoff", "runoff_mm_pgi") 
vars = c("GrossIrr_mm_pgn", "runoff_mm_pgn", "GrossIrr_mm_pu", "runoff_mm_pu") 
vars = c("GrossIrr_mm_ps", "runoff_mm_ps", "snowMelt", "snowFall", "precip")      # other useful vars
lapply(vars, FUN = function(var){create_dir(file.path("results/subbasin", var))})

mod = "ERA_hist"
path.out  = "results/subbasin"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)
years = seq(1980, 2009)  # climatology historical time series

# monthly climatology
month.data = read.csv("data/days_in_months.csv")

for(var in vars){
  # check if file exists
  check.file = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_mc.csv", sep="")
  if(!exists(check.file)){
    sp.agg = spatial_aggregation(raster.data = brick(paste(path.base, "/climatology/wbm_", var, "_mc.nc", sep=""))*month.data$days,
                                 shapefile = subbasins.poly)
    colnames(sp.agg@data)[2:13] = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    out.nm = paste(path.out, "/", var, "/", mod, "_subbasin_", var, "_km3_mc.csv", sep="")
    write.csv(sp.agg@data, out.nm, row.names = F)
  }
  print(var)
}

################################################################################################################################################
month_max_table = function(base.var, 
                           p.var,
                           path, 
                           mod){
  
  # load data
  base.dat = read.csv(paste(path, "/", base.var, "/", mod, "_subbasin_", base.var, "_km3_mc.csv", sep=""))
  p.dat    = read.csv(paste(path, "/", p.var   , "/", mod, "_subbasin_", p.var   , "_km3_mc.csv", sep=""))
  
  # subset to subbasins with > 0 of base.var
  p.dat    = subset(p.dat, rowSums(base.dat[,2:ncol(base.dat)]) > 0)
  base.dat = subset(base.dat, rowSums(base.dat[,2:ncol(base.dat)]) > 0)
  
  month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # month of max p.var km3
  max.p.km3 = apply(p.dat[,2:ncol(p.dat)], c(1), max, na.rm=T)
  max.month = (p.dat == max.p.km3)
  max.month.id = apply(max.month, MARGIN = c(1), FUN = function(x) which(x == T))
  max.month.names = month.names[unlist(max.month.id)]
  
  # text formatting
  max.p.km3 = signif(max.p.km3,2)
  
  # month of max p.var %
  p.percent = 100*(p.dat / base.dat)
  max.p.percent = apply(p.percent[,2:ncol(p.percent)], c(1), max, na.rm=T)
  
  # plotting aside
  b = c(1,seq(5,100, by=5))
  png("figures/Historical/irrGross_pgi_max_month_percent_histogram_1980_2009_mc.png", width = 1500, height=1000, res=130)
  hist(max.p.percent, breaks = 15, labels=T, 
       main = "% Glacier Melt in Irr Water in Month of Max: 391 Subbasins",
       xlab = "Maximum monthly percent")
  dev.off()
  
  max.p.5 = subset(max.p.percent, max.p.percent <= 5)
  png("figures/Historical/irrGross_pgi_max_month_percent_le5_histogram_1980_2009_mc.png", width = 1500, height=1000, res=130)
  hist(max.p.5, breaks = 5, labels=T, 
       main = "% Glacier Melt in Irr Water in Month of Max: 258 Subbasins",
       xlab = "Maximum monthly percent")
  dev.off()
  
  max.p.1 = subset(max.p.percent, max.p.percent <= 1)
  png("figures/Historical/irrGross_pgi_max_month_percent_le1_histogram_1980_2009_mc.png", width = 1500, height=1000, res=130)
  hist(max.p.1, breaks = 10, labels=T, 
       main = "% Glacier Melt in Irr Water in Month of Max: 217 Subbasins",
       xlab = "Maximum monthly percent")
  dev.off()
  
  max.month.percent = (p.percent == max.p.percent)
  max.month.id.percent = apply(max.month.percent, MARGIN = c(1), FUN = function(x) which(x == T))
  max.month.percent.names = month.names[unlist(max.month.id.percent)]
  
  # text formatting
  max.p.percent = signif(max.p.percent,2)
  max.p.percent[max.p.percent < 0.1] = "<0.1"
  
  max.month.out = as.data.frame(cbind(p.dat$HiMAT_full_210_Subset_regions, 
                                      max.month.names,
                                      max.p.km3,
                                      max.month.percent.names,
                                      max.p.percent
                                      )
                                )
  colnames(max.month.out) = c("Subbasin", "Month_of_Max_km3", "Max_km3", "Month_of_Max_%", "Max_%")
  
}


base.var = "irrigationGross"
p.var = "GrossIrr_mm_pgi"
path = "results/subbasin/"
mod = "ERA_hist"


################################################################################################################################################


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


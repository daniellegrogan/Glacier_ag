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



#### Productivity (tonnes)
p = "/net/nfs/squam/raid/data/GLEAM/HarvardDataverse_GAEZ+_2015/GAEZ+_2015_crop_production/"
sp.agg = subbasin_gaez(p)
writeOGR(sp.agg, dsn = "data/subbasin_poly_Production/", layer = "subbasin_poly_Production", driver = "ESRI Shapefile")

# plot
png("figures/Historical/subbasin_poly_Production_SUBBASIN_tonnes.png", width = 1500, height=1000, res=130)
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

png("figures/Historical/Crop_yield_SUBBASIN_tonnes_per_kha.png", width = 1500, height=1000, res=130)
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
png("figures/Historical/Crop_yield_SUBBASIN_tonnes_per_kha_50percent.png", width = 1500, height=1000, res=130)
spplot(sp.agg.p50, zcol=2, sp.layout = list(basin.layer, coastline.layer), 
       col.regions = green.pal, col = "transparent", as.table=TRUE, 
       xlim=c(57.4, 120.6), ylim=c(9.9, 49.4))
dev.off()

##########################################################################################################################################

# calculate irrigation vars for key subbasins
sub_harv_50 = readOGR(dsn = "data/subbasin_poly_HarvArea50p/", layer = "subbasin_poly_HarvArea50p")
sub_prod_50 = readOGR(dsn = "data/subbasin_poly_Production50p/", layer = "subbasin_poly_Production50p")

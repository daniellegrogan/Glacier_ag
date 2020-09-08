# Study domain description table

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))


# shapefiles for spatial agg
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over

basin.area = area(basins)*1e-6*1e-3  # unit: 1000 km2

# crop production
crop.prod = raster("data/GAEZ_2015_crop_production.nc")
irr.prod = raster("data/GAEZ_2015_Irrigated_crop_production.nc")

crop.prod.basins = extract(crop.prod, basins, fun=sum, na.rm=T, sp=F)
irr.prod.basins  = extract(irr.prod,  basins, fun=sum, na.rm=T, sp=F)

crop.prod.global = cellStats(crop.prod, sum)
irr.prod.global = cellStats(irr.prod, sum)

crop.prod.basins.percent = 100*(crop.prod.basins/crop.prod.global)
irr.prod.basins.percent = 100*(irr.prod.basins/irr.prod.global)

hma.crop.prod.total = sum(crop.prod.basins)
hma.irr.prod.total = sum(irr.prod.basins)

hma.crop.prod.percent = 100*(hma.crop.prod.total/crop.prod.global)
hma.irr.prod.percent = 100*(hma.irr.prod.total/irr.prod.global)

hma.irr.per = 100*(hma.irr.prod.total/hma.crop.prod.total)

# glacier area
glacier.area = raster("/net/nfs/yukon/raid5/data/RGI6/global_rgi6_glaciers_1km.tif")
glacier.area = crop(glacier.area, extent(basins))
cell.area = raster::area(glacier.area)
glacier.area.km2 = cell.area * glacier.area
cellStats(glacier.area.km2, sum)

# precip 
precip.yc = read.csv("results/precip/ERA_hist_basin_precip_km3_1980_2009_yc.csv")

# glacier ice melt
glmelt.y = read.csv("results/glMelt/ERA_hist_basin_glMelt_km3_1980_2009_yearly.csv")
glmelt.yc = rowMeans(glMelt.y[,3:ncol(glmelt.y)], na.rm=T)
names(glmelt.yc) = glmelt.y$Basin

## Make table
study.domain = cbind(basins$name, basin.area, crop.prod.basins, irr.prod.basins, precip.yc$Mean[1:15], as.numeric(glmelt.yc)[1:15])
study.domain[,1] = as.character(basins$name)
colnames(study.domain) = c("Basin", "Area (1000 km2)", 
                         "Crop Production (1000 tonnes)", "Irrigated Crop Production (1000 tonnes)", 
                         "Precipitation (km3/yr)", "Glacier Ice Melt (km3/yr)")
                         
write.csv(study.domain, "results/Tables/Study_domain.csv", row.names=F)



# irrigated cropland
irr.land = raster("/net/nfs/bog/raid/data/dgrogan/MIRCA_data/WBM_format/IrrAreaFraction_MIRCA2000_05mn.nc")
cell.area = raster::area(irr.land)

irr.land.km2 = irr.land * cell.area
irr.land.km2.basins = extract(irr.land.km2, basins, fun=sum, na.rm=T, sp=F)

sum(irr.land.km2.basins)

# reservoirs
dams = read.csv("/net/nfs/zero/home/WBM_TrANS/spreadsheets/GRanD_dams_v1_3.csv")
dams = as.data.frame(dams)
coordinates(dams) <- ~ LONG_DD + LAT_DD   
crs(dams) = crs(basins)
dams.hma = dams[basins,] 
dim(dams.hma)
sum(dams.hma$USE_IRRI == "Main" | dams.hma$USE_IRRI == "Sec")
dams.irri = subset(dams, dams.hma$USE_IRRI == "Main" | dams.hma$USE_IRRI == "Sec")
sum(dams.irri$CAP_MAX)

# IBTs


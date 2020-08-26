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

# precip 
precip.yc = read.csv("results/precip/ERA_hist_basin_precip_km3_1980_2009_yc.csv")

# glacier ice melt
glmelt.yc = read.csv("results/Glacier_ice_melt/ERA_hist_glacier_melt_basins_1980_2009_yc.csv")

# yearly melt
glmelt.y = read.csv("results/Glacier_ice_melt/ERA_hist_glacier_melt_basins_yearly.csv")

## Make table
study.domain = cbind(basins$name, basin.area, crop.prod.basins, irr.prod.basins, precip.yc$Mean, glmelt.yc$Mean)
study.domain[,1] = as.character(basins$name)
colnames(study.domain) = c("Basin", "Area (1000 km2)", 
                         "Crop Production (1000 tonnes)", "Irrigated Crop Production (1000 tonnes)", 
                         "Precipitation (km3/yr)", "Glacier Ice Melt (km3/yr)")
                         
write.csv(study.domain, "results/Tables/Study_domain.csv", row.names=F)

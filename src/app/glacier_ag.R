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




### File paths ###
path.base = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/Peak_Storage/"
path.out  = "results"
path.yr   = file.path(path.base, mod, "yearly")
path.mo   = file.path(path.base, mod, "monthly")
path.c = file.path(path.base, mod, "climatology")
map.dir = "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Frontiers/Figures"

### Contribution of water components to agriculture  ###
mod = "ERA_hist"

### Glacier water
# 1. annual
vars = c("irrigationGross",
         'GrossIrr_mm_pg')
percent.nm = "GrossIrr_pg_percent"
basin.agg.pg = agg_contribution(path.yr, basins, vars, percent.nm) 
out.nm = paste(path.out, mod, "_basin_IrrGross_pg_yearly.csv", sep="")
write.table(basin.agg.pg, 
            out.nm,
            sep=",")

# 2. monthly
vars = c("irrigationGross",
         'GrossIrr_mm_pg')
percent.nm = "GrossIrr_pg_percent"
basin.agg.pg.m = agg_contribution(path.mo, basins, vars, percent.nm) 
out.nm = paste(path.out, mod, "_basin_IrrGross_pg_monthly.csv", sep="")
write.table(basin.agg.pg.m, 
            out.nm,
            sep=",")

#### MAKE THIS INTO A FUNCTION:
# calculate month of max km3 irr_pg
irr.pg = subset(basin.agg.pg.m, select = grepl("mm_pg", colnames(basin.agg.pg.m)))
irr.pg.stdev = irr.pg[,13:24]
irr.pg = irr.pg[,1:12]

max.irr.pg = apply(irr.pg, c(1), max, na.rm=T)
max.month = (irr.pg == max.irr.pg)
max.month.id = apply(max.month, MARGIN = c(1), FUN = function(x) which(x == max(x, na.rm=T)))
month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
max.month.names = cbind(names(max.month.id), month.names[max.month.id])
max.month.out = cbind(max.month.names, max.irr.pg)

out.nm = paste(path.out, mod, "_basin_IrrGross_pg_km3_month_max.csv", sep="")
write.table(max.month.out, 
            out.nm,
            sep=",")

# calculate month of max % irr_pg
irr.pg = subset(basin.agg.pg.m, select = grepl("percent", colnames(basin.agg.pg.m)))
irr.pg.stdev = irr.pg[,13:24]
irr.pg = irr.pg[,1:12]

max.irr.pg = apply(irr.pg, c(1), max, na.rm=T)
max.month = (irr.pg == max.irr.pg)
max.month.id = apply(max.month, MARGIN = c(1), FUN = function(x) which(x == max(x, na.rm=T)))
month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
max.month.names = cbind(names(max.month.id), month.names[max.month.id])
max.month.out = cbind(max.month.names, max.irr.pg)

out.nm = paste(path.out, mod, "_basin_IrrGross_pg_percent_month_max.csv", sep="")
write.table(max.month.out, 
            out.nm,
            sep=",")

# 3. monthly SW_pg 
vars = c("irrigationGross",
         'IrrFlow_mm_pg')
percent.nm = "GrossIrr_SWpg_percent"
basin.agg.SW.pg.m = agg_contribution(path.mo, basins, vars, percent.nm) 
out.nm = paste(path.out, mod, "_basin_IrrGross_SWpg_monthly.csv", sep="")
write.table(basin.agg.SW.pg.m, 
            out.nm,
            sep=",")

# 4. monthly GW_pg 
vars = c("irrigationGross",
         'IrrGrwt_mm_pg')
percent.nm = "GrossIrr_GWpg_percent"
basin.agg.GW.pg.m = agg_contribution(path.mo, basins, vars, percent.nm) 
out.nm = paste(path.out, mod, "_basin_IrrGross_GWpg_monthly.csv", sep="")
write.table(basin.agg.GW.pg.m, 
            out.nm,
            sep=",")
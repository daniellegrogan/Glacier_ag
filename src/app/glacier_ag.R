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

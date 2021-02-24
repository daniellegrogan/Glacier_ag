# compare discharge_pgi reaching ocean between spinup times

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# mouth_ts()
mouth_ts.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/mouth_ts.R", ssl.verifypeer=F)
eval(parse(text=mouth_ts.script))

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))


### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)

########################################################################################################################
# paths
path.base = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_09/"

sp30  = "ERA_hist_30"
sp60  = "ERA_hist_60"
sp300 = "ERA_hist_300"
sp600 = "ERA_hist_600"

sp.list = c(sp30, sp60, sp300, sp600)

# basin data
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
basin.ID<-raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_IDs_Subset.asc")
up.area  = raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_Subset_upstrArea.asc")

### discharge_pgi ###
# other var settings
varname  = "discharge_m3s_pgi"
yrs = seq(1980, 2009)

# set up output table
q.pgi.table = data.frame(matrix(nr=length(basins$Basin_ID), nc=length(sp.list)))
rownames(q.pgi.table) = basins$name
colnames(q.pgi.table) = sp.list

for(s in 1:length(sp.list)){
  # yearly clim
  path = file.path(path.base, sp.list[s], "climatology/wbm_discharge_m3s_pgi_yc.nc")
  wbm.data = raster(path)
  
  for(i in 1:length(basins$Basin_ID)){
    pt = id_mouth(basin.ID = basins$Basin_ID[i],
                  ID = basin.ID, 
                  up.area)
    if(length(pt) == 1){
      q.pgi.table[i,s] = extract(wbm.data, pt)       # extract wbm data from basin mouth point
    }  # otherwise the basin in endorheic, so leave value as NA
    
  }
}


### etIrrCrops_mm_pgi ###
varname  = "etIrrCrops_mm_pgi"
yrs = seq(1980, 2009)

# set up output table
et.pgi.table = data.frame(matrix(nr=length(basins$Basin_ID), nc=length(sp.list)))
rownames(et.pgi.table) = basins$name
colnames(et.pgi.table) = sp.list

for(s in 1:length(sp.list)){
  # yearly clim
  path = file.path(path.base, sp.list[s], "climatology/wbm_etIrrCrops_mm_pgi_yc.nc")
  wbm.data = raster(path)*365
  et.pgi.table[,s] = spatial_aggregation(wbm.data, basins, s=1)$layer
}

diff.30 = et.pgi.table[,2:4] - et.pgi.table[,1]
diff.30.max = apply(diff.30, c(1), FUN=max)
diff.30.max.percent = 100*(diff.30.max/et.pgi.table[,1])

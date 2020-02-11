# glacier_groundwater()

# Glacier contributions to agriculture paper: 
#    Analysis: how much glacier melt enters the groundwater system?  How does spinup effect this?
# project: NASA HiMAT
# Danielle S Grogan

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(maps)
library(abind)

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

# month_day_ids()
month_day_ids.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/month_day_ids.R", ssl.verifypeer=F)
eval(parse(text=month_day_ids.script))

# raster_time_ave()
raster_time_ave.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/raster_time_ave.R", ssl.verifypeer=F)
eval(parse(text=raster_time_ave.script))

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)

#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

# WBM test runs to assess glacier melt tracking:
# TEST 1: no glacier melt in spin-up (30 years spin-up)
# TEST 2: no glacier melt AFTER spin-up.  Spin-up glacier melt is climatology of ERA-Interim glacier melt

# shapefile for spatial agg
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
region = gUnaryUnion(basins, id = rep(1, length(basins)))

### TEST 2 ###
### test if mm sums are correct 
path.t2 = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/"

test_tracking_mm(path = file.path(path.t2, "monthly"), 
                 sum.var = "grdWater", 
                 component.vars = c("grndWater_mm_ps", "grndWater_mm_pr", "grndWater_mm_pu", "grndWater_mm_pgi", "grndWater_mm_pgn"), 
                 years = 1980)
test_tracking_mm(path = file.path(path.t2, "monthly"), 
                 sum.var = "irrigationGross", 
                 component.vars = c("GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu", "GrossIrr_mm_pgi", "GrossIrr_mm_pgn"), 
                 years = 1980)

### aggregate glacier water by basin
path.t2 = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly"
years = seq(1980, 1985)

# groundwater pgi
grndWater_mm_pgi = wbm_load(file.path(path.t2, "grndWater_mm_pgi"), varname = "grndWater_mm_pgi", years) # monthly
grndWater_km3_pgi.basins = spatial_aggregation(grndWater_mm_pgi, basins, s=1, poly.out=F)
rownames(grndWater_km3_pgi.basins) = basins$name
colnames(grndWater_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("1985-12-31"), by = "month"))
write.csv(grndWater_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_2_grndWater_km3_pgi_basins.csv")

for(i in 1:nrow(grndWater_km3_pgi.basins)){
  plot(grndWater_km3_pgi.basins[i,], type='l', main = rownames(grndWater_km3_pgi.basins)[i])
}

# irrigation pgi
GrossIrr_mm_pgi = wbm_load(file.path(path.t2, "GrossIrr_mm_pgi"), varname = "GrossIrr_mm_pgi", years) # monthly
GrossIrr_km3_pgi.basins = spatial_aggregation(GrossIrr_mm_pgi, basins, s=1, poly.out=F)
rownames(GrossIrr_km3_pgi.basins) = basins$name
colnames(GrossIrr_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("1985-12-31"), by = "month"))
write.csv(GrossIrr_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_2_GrossIrr_km3_pgi_basins.csv")

for(i in 1:nrow(GrossIrr_km3_pgi.basins)){
  plot(GrossIrr_km3_pgi.basins[i,], type='l', main = rownames(GrossIrr_km3_pgi.basins)[i])
}

# baseflow pgi
baseflow_mm_pgi = wbm_load(file.path(path.t2, "baseflow_mm_pgi"), varname = "baseflow_mm_pgi", years)
baseflow_km3_pgi.basins = spatial_aggregation(baseflow_mm_pgi, basins, s=1, poly.out=F)
rownames(baseflow_km3_pgi.basins) = basins$name
colnames(baseflow_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("1985-12-01"), by = "month"))
write.csv(baseflow_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_2_baseflow_km3_pgi_basins.csv")

for(i in 1:nrow(baseflow_km3_pgi.basins)){
  plot(baseflow_km3_pgi.basins[i,], type='l', main = rownames(baseflow_km3_pgi.basins)[i])
}

# mouth time series
basin.ID = raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_Subset_IDs.asc")
path     = path.t2
up.area  = raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_Subset_upstrArea.asc") 
varname  = "discharge_m3s_pgi"
ids      = basins$Basin_ID 

for(i in ids){
  q_pgi = mouth_ts(i, basin.ID, up.area, path = file.path(path.t2, varname), varname, years)
  basin.name = as.character(basins$name[which(basins$Basin_ID == i)])
  plot(as.numeric(q_pgi), type='l', main = basin.name)
  write.csv(q_pgi, paste("/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_2_discharge_m3s_pgi_mouth_Basin_", basin.name, ".csv", sep=""))
}





### TEST 1 ###
### test if mm sums are correct 
path.t2 = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/monthly"

test_tracking_mm(path = file.path(path.t2, "monthly"), 
                 sum.var = "grdWater", 
                 component.vars = c("grndWater_mm_ps", "grndWater_mm_pr", "grndWater_mm_pu", "grndWater_mm_pgi", "grndWater_mm_pgn"), 
                 years = 1980)
test_tracking_mm(path = file.path(path.t2, "monthly"), 
                 sum.var = "irrigationGross", 
                 component.vars = c("GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu", "GrossIrr_mm_pgi", "GrossIrr_mm_pgn"), 
                 years = 1980)

### aggregate glacier water by basin
path.t2 = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/monthly"
years = seq(1980, 2015)

# groundwater pgi
grndWater_mm_pgi = wbm_load(file.path(path.t2, "grndWater_mm_pgi"), varname = "grndWater_mm_pgi", years) # monthly
grndWater_km3_pgi.basins = spatial_aggregation(grndWater_mm_pgi, basins, s=1, poly.out=F)
rownames(grndWater_km3_pgi.basins) = basins$name
colnames(grndWater_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("2015-12-01"), by = "month"))
write.csv(grndWater_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_1_grndWater_km3_pgi_basins.csv")

for(i in 1:nrow(grndWater_km3_pgi.basins)){
  plot(grndWater_km3_pgi.basins[i,], type='l', main = rownames(grndWater_km3_pgi.basins)[i])
}

# irrigation pgi
GrossIrr_mm_pgi = wbm_load(file.path(path.t2, "GrossIrr_mm_pgi"), varname = "GrossIrr_mm_pgi", years) # monthly
GrossIrr_km3_pgi.basins = spatial_aggregation(GrossIrr_mm_pgi, basins, s=1, poly.out=F)
rownames(GrossIrr_km3_pgi.basins) = basins$name
colnames(GrossIrr_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("2015-12-01"), by = "month"))
write.csv(GrossIrr_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_1_GrossIrr_km3_pgi_basins.csv")

for(i in 1:nrow(GrossIrr_km3_pgi.basins)){
  plot(GrossIrr_km3_pgi.basins[i,], type='l', main = rownames(GrossIrr_km3_pgi.basins)[i])
}

# baseflow pgi
baseflow_mm_pgi = wbm_load(file.path(path.t2, "baseflow_mm_pgi"), varname = "baseflow_mm_pgi", years)
baseflow_km3_pgi.basins = spatial_aggregation(baseflow_mm_pgi, basins, s=1, poly.out=F)
rownames(baseflow_km3_pgi.basins) = basins$name
colnames(baseflow_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("2015-12-01"), by = "month"))
write.csv(baseflow_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_1_baseflow_km3_pgi_basins.csv")

for(i in 1:nrow(baseflow_km3_pgi.basins)){
  plot(baseflow_km3_pgi.basins[i,], type='l', main = rownames(baseflow_km3_pgi.basins)[i])
}

# mouth time series
basin.ID = raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_Subset_IDs.asc")
path     = path.t2
up.area  = raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_Subset_upstrArea.asc") 
varname  = "discharge_m3s_pgi"
ids      = basins$Basin_ID 

for(i in ids){
  q_pgi = mouth_ts(i, basin.ID, up.area, path = file.path(path.t2, varname), varname, years)
  basin.name = as.character(basins$name[which(basins$Basin_ID == i)])
  plot(as.numeric(q_pgi), type='l', main = basin.name)
  write.csv(q_pgi, paste("/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_1_discharge_m3s_pgi_mouth_Basin_", basin.name, ".csv", sep=""))
}
################################################################################################
# ## SANDBOX
# 
# path.era = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/"
# 
# test_tracking_mm(path = file.path(path.era, "monthly"), 
#                  sum.var = "grdWater", 
#                  component.vars = c("grndWater_mm_ps", "grndWater_mm_pr", "grndWater_mm_pu", "grndWater_mm_pgi", "grndWater_mm_pgn"), 
#                  years = 1980)
# 
# 
# test_tracking_mm(path = file.path(path.era, "monthly"), 
#                  sum.var = "irrigationGross", 
#                  component.vars = c("GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu", "GrossIrr_mm_pgi", "GrossIrr_mm_pgn"), 
#                  years = 1980)



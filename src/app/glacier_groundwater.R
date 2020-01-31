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

### test if month ave makes sense
path.t2 = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/"

# test mm
grndWater_mm_pgi.d = brick(file.path(path.t2, "daily", "wbm_1980.nc"), varname = "grndWater_mm_pgi")
grndWater_mm_pgi.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_mm_pgi/wbm_1980.nc")

m.ids = month_day_ids(leap = T)
grndWater_d_to_m.ave = stackApply(grndWater_mm_pgi.d, m.ids, fun = mean)

diff = (grndWater_d_to_m.ave - grndWater_mm_pgi.m)
for(i in 1:12){
    a = round(min(as.matrix(subset(diff,i)), na.rm=T), 6)
    b = round(max(as.matrix(subset(diff,i)), na.rm=T), 6)
    print(paste("layer ", i, "min ", a, "; max ", b))
}

# test fraction
grndWater_pgi.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_pgi/wbm_1980.nc")
grndWater_pgn.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_pgn/wbm_1980.nc")
grndWater_ps.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_ps/wbm_1980.nc")
grndWater_pr.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_pr/wbm_1980.nc")
grndWater_pu.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_pu/wbm_1980.nc")

grndWater.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grdWater/wbm_1980.nc")

frac.sum = grndWater_pgi.m + grndWater_pgn.m + grndWater_ps.m + grndWater_pr.m + grndWater_pu.m

test = (frac.sum > 0 & frac.sum < 1)
for(i in 1:12){
  plot(subset(frac.sum, i), main = i)
}

#####
# Compare January: monthly to daily fractions

grndWater.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grdWater/wbm_1980.nc")

comp = c("pgi", "pgn", "pr", "ps", "pu")
comp.table = data.frame(matrix(nc=2*length(comp), nr=12))
for(i in 1:length(comp)){
  frac.m =  brick(paste("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_", comp[i], "/wbm_1980.nc", sep=""))
  dpth.m = brick(paste("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_mm_", comp[i], "/wbm_1980.nc", sep=""))
  
  frac.x.mm = frac.m * grndWater.m
  
  dpth.m.agg    = spatial_aggregation(dpth.m, region, s=1, poly.out=F)
  frac.x.mm.agg
  
}

# fractions
grndWater_pgi.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_pgi/wbm_1980.nc")
grndWater_pgi.d = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/wbm_1980.nc", varname = "grndWater_pgi")

# mm
grndWater_mm_pgi.d = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/wbm_1980.nc", varname = "grndWater_mm_pgi")
grndWater_mm_pgi.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grndWater_mm_pgi/wbm_1980.nc")

# groundWater
grndWater.d = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/wbm_1980.nc", varname = "grdWater")
grndWater.m = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/grdWater/wbm_1980.nc")

# monthly frac x gw
grndWater_pgi_m.calc = grndWater_pgi.m * grndWater.m

# compare to reported monthly mm_pgi
test = grndWater_mm_pgi.m - grndWater_pgi_m.calc
for(i in 1:12){
  a = round(min(as.matrix(subset(test,i)), na.rm=T), 6)
  b = round(max(as.matrix(subset(test,i)), na.rm=T), 6)
  print(paste("layer ", i, "min ", a, "; max ", b))
}

#####
# test fraction DAILY
grndWater_pgi.d = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/wbm_1980.nc", varname = "grndWater_pgi")
grndWater_pgn.d = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/wbm_1980.nc", varname = "grndWater_pgn")
grndWater_ps.d = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/wbm_1980.nc", varname = "grndWater_ps")
grndWater_pr.d = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/wbm_1980.nc", varname = "grndWater_pr")
grndWater_pu.d = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/wbm_1980.nc", varname = "grndWater_pu")

frac.sum = grndWater_pgi.d + grndWater_pgn.d + grndWater_ps.d + grndWater_pr.d + grndWater_pu.d
test = (frac.sum > 0 & frac.sum < 0.999999 )
for(i in 1:365){
  print(max(as.matrix(subset(test,i)),na.rm=T))
  if(max(as.matrix(subset(test,i)),na.rm=T) == 1){
    plot(subset(test, i)*subset(frac.sum, i), main = i)
  }
}

grndWater_mm_pgi.m.calc = grndWater_pgi.m * grndWater.m
diff = (grndWater_mm_pgi.m.calc - grndWater_mm_pgi.m)
for(i in 1:12){
  a = round(min(as.matrix(subset(diff,i)), na.rm=T), 6)
  b = round(max(as.matrix(subset(diff,i)), na.rm=T), 6)
  print(paste("layer ", i, "min ", a, "; max ", b))
}

### TEST 2 ###
path.t2 = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/monthly/"
#years = seq(1980,2015)
years = seq(1980, 1989)

grndWater_mm_pgi = wbm_load(file.path(path.t2, "grndWater_mm_pgi"), varname = "grndWater_mm_pgi", years) # monthly
grndWater_km3_pgi.basins = spatial_aggregation(grndWater_mm_pgi, basins, s=1, poly.out=F)
rownames(grndWater_km3_pgi.basins) = basins$name
colnames(grndWater_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("1989-12-31"), by = "month"))
write.csv(grndWater_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_2_grndWater_km3_pgi_basins.csv")

for(i in 1:nrow(grndWater_km3_pgi.basins)){
  plot(grndWater_km3_pgi.basins[i,1:36]*1e9, type='l', main = rownames(grndWater_km3_pgi.basins)[i])
}




GrossIrr_mm_pgi = wbm_load(path.t2, varname = "GrossIrr_mm_pgi", years)
GrossIrr_km3_pgi.basins = spatial_aggregation(GrossIrr_mm_pgi, basins, s=1, poly.out=F)
rownames(GrossIrr_km3_pgi.basins) = basins$name
colnames(GrossIrr_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("2015-12-31"), by = "day"))
write.csv(GrossIrr_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_2_GrossIrr_km3_pgi_basins.csv")

baseflow_mm_pgi = wbm_load(path.t2, varname = "baseflow_mm_pgi", years)
baseflow_km3_pgi.basins = spatial_aggregation(baseflow_mm_pgi, basins, s=1, poly.out=F)
rownames(baseflow_km3_pgi.basins) = basins$name
colnames(baseflow_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("2015-12-31"), by = "day"))
write.csv(baseflow_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_2_baseflow_km3_pgi_basins.csv")

# mouth time series
basin.ID = raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_Subset_IDs.asc")
path     = path.t2
up.area  = raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_Subset_upstrArea.asc") 
varname  = "discharge_m3s_pgi"
ids      = basins$Basin_ID 

for(i in ids){
  q_pgi = mouth_ts(i, basin.ID, up.area, path, varname, years)
  basin.name = as.character(basins$name[which(basins$Basin_ID == i)])
  write.csv(q_pgi, paste("/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_2_discharge_m3s_pgi_mouth_Basin_", basin.name, ".csv", sep=""))
}


### TEST 1 ###
path.t1 = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/"
years = seq(1980,2013)

grndWater_mm_pgi = wbm_load(path.t1, varname = "grndWater_mm_pgi", years)
grndWater_km3_pgi.basins = spatial_aggregation(grndWater_mm_pgi, basins, s=1, poly.out=F)
rownames(grndWater_km3_pgi.basins) = basins$name
colnames(grndWater_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by = "day"))
write.csv(grndWater_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_1_grndWater_km3_pgi_basins.csv")

GrossIrr_mm_pgi = wbm_load(path.t1, varname = "GrossIrr_mm_pgi", years)
GrossIrr_km3_pgi.basins = spatial_aggregation(GrossIrr_mm_pgi, basins, s=1, poly.out=F)
rownames(GrossIrr_km3_pgi.basins) = basins$name
colnames(GrossIrr_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("2013-12-31"), by = "day"))
write.csv(GrossIrr_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_1_GrossIrr_km3_pgi_basins.csv")

baseflow_mm_pgi = wbm_load(path.t1, varname = "baseflow_mm_pgi", years)
baseflow_km3_pgi.basins = spatial_aggregation(baseflow_mm_pgi, basins, s=1, poly.out=F)
rownames(baseflow_km3_pgi.basins) = basins$name
colnames(baseflow_km3_pgi.basins) = as.character(seq(as.Date("1980-01-01"), as.Date("2015-12-31"), by = "day"))
write.csv(baseflow_km3_pgi.basins, "/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_1_baseflow_km3_pgi_basins.csv")

path     = path.t1
for(i in ids){
  q_pgi = mouth_ts(i, basin.ID, up.area, path, varname, years)
  basin.name = as.character(basins$name[which(basins$Basin_ID == i)])
  write.csv(q_pgi, paste("/net/nfs/squam/raid/userdata/dgrogan/HiMAT/Glacier_TEST_1_discharge_m3s_pgi_mouth_Basin_", basin.name, ".csv", sep=""))
}




# vars.mm = c("grdWater", 
#             "grndWater_mm_ps", "grndWater_mm_pr", "grndWater_mm_pu", "grndWater_mm_pgi", "grndWater_mm_pgn")
# vars.f  = c("grndWater_ps",    "grndWater_pr",    "grndWater_pu",    "grndWater_pgi",    "grndWater_pgn")
# 
# 
# # load year 1980
# gw.1980 = brick(file.path(path.t2, "wbm_1980.nc"), varname = "grdWater")
# gw.mm.pgi.1980 = brick(file.path(path.t2, "wbm_1980.nc"), varname = "grndWater_mm_pgi")
# gw.fr.pgi.1980 = brick(file.path(path.t2, "wbm_1980.nc"), varname = "grndWater_pgi")
# 
# # calculate volume of shallow groundwater each day in 1980
# gw.km3 = spatial_aggregation(gw.1980, shapefile = basins, s = 1, poly.out = F)
# rownames(gw.km3) = basins$name
# colnames(gw.km3) = as.character(seq(as.Date("1980-01-01"), as.Date("1980-12-31"), by = "day"))
# 
# # calculate volume of pgi in shallow groundwater each day in 1980
# gw.pgi.km3 = spatial_aggregation(gw.mm.pgi.1980, shapefile = basins, s = 1, poly.out = F)
# rownames(gw.pgi.km3) = basins$name
# colnames(gw.pgi.km3) = as.character(seq(as.Date("1980-01-01"), as.Date("1980-12-31"), by = "day"))
# 
# for(i in 1:nrow(gw.km3)){
#   plot(gw.km3[i,], type='l', ylim=c(0, max(gw.km3[i,])), main = rownames(gw.km3)[i])
#   lines(gw.pgi.km3[i,], col='blue')
# }
# 
# # load years 1980 - 2008
# years = seq(1980, 2008)
# 
# gw        = wbm_load(path = path.t2, varname = "grdWater",         years)
# gw.mm.pgi = wbm_load(path = path.t2, varname = "grndWater_mm_pgi", years)
# 
# # aggregate over full region (all basins grouped together)
# gw.km3.region     = spatial_aggregation(gw,        region, s = 1, poly.out = F)
# gw.pgi.km3.region = spatial_aggregation(gw.mm.pgi, region, s = 1, poly.out = F)
# 
# 
# path = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_2/daily/"
# frac.vars = c("grndWater_ps",    "grndWater_pr",    "grndWater_pu",    "grndWater_pgi",    "grndWater_pgn")
# years=1980
# #years = seq(1980, 2015)
# test_tracking_fractions(path, frac.vars, years)
# 
# frac.vars = c("GrossIrr_ps",    "GrossIrr_pr",    "GrossIrr_pu",    "GrossIrr_pgi",    "GrossIrr_pgn")
# test_tracking_fractions(path, frac.vars, years)
# 
# path = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/"
# years = seq(1980, 2012)
# frac.vars = c("grndWater_ps",    "grndWater_pr",    "grndWater_pu",    "grndWater_pgi",    "grndWater_pgn")
# test_tracking_fractions(path, frac.vars, years)
# 
# frac.vars = c("GrossIrr_ps",    "GrossIrr_pr",    "GrossIrr_pu",    "GrossIrr_pgi",    "GrossIrr_pgn")
# test_tracking_fractions(path, frac.vars, years)
# 
# sum.var = "GrossIrr_mm_pgi"
# component.vars = c("IrrGrwt_mm_pgi", "IrrFlow_mm_pgi")
# test_tracking_mm(path, sum.var, component.vars, years)
# 
# 
# GrossIrr_mm_pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/wbm_1980.nc", varname = "GrossIrr_mm_pgi")
# IrrGrwt_mm_pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/wbm_1980.nc",  varname = "IrrGrwt_mm_pgi")
# IrrFlow_mm_pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/wbm_1980.nc",  varname = "IrrFlow_mm_pgi")
# 
# test.1 = subset(GrossIrr_mm_pgi, 244) - (subset(IrrGrwt_mm_pgi, 244) + subset(IrrFlow_mm_pgi, 244))
# plot(test.1)
# 
# basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
# test.basins = spatial_aggregation(test.1, basins, s=1, poly.out=F)
# 
# GrossIrr_mm_pgi.basins = spatial_aggregation(subset(GrossIrr_mm_pgi, 244), basins, s=1, poly.out=F)
# 
# diff.frac = test.basins/GrossIrr_mm_pgi.basins
# rownames(diff.frac) = basins$name
# 
# 
# ####
# GrossIrr_mm_pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/wbm_1980.nc", varname = "GrossIrr_mm_pgi")
# IrrGrwt_mm_pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/wbm_1980.nc",  varname = "IrrGrwt_mm_pgi")
# IrrFlow_mm_pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/wbm_1980.nc",  varname = "IrrFlow_mm_pgi")
# 
# GrossIrr_pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/wbm_1980.nc", varname = "GrossIrr_pgi")
# 
# GrossIrr_tot = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_01/Glacier_TEST_1/daily/wbm_1980.nc", varname = "irrigationGross")
# 
# GrossIrr_mm_pgi_calc = overlay(GrossIrr_pgi, GrossIrr_tot, fun = function(x,y) x*y)
# 
# diff.calc = overlay(GrossIrr_mm_pgi, GrossIrr_mm_pgi_calc, fun = function(x,y) x-y)
# for(i in 1:nlayers(diff.calc)){
#   a = round(min(as.matrix(subset(diff.calc,i)), na.rm=T), 6)
#   b = round(max(as.matrix(subset(diff.calc,i)), na.rm=T), 6)
#   print(paste("layer ", i, "min ", a, "; max ", b))
# }


# test spinup time

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
# library(maptools)
# library(maps)
# library(abind)

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
subbasins.poly = readOGR(dsn = "data/subbasin_poly/", layer = "subbasin_poly")

# spinup test runs
base.path = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_09/ERA_hist_"
sp.char = c("30", "60", "300")

# key variables
vars = c(# storage variables
          "endoStrg",       "cEndoP_pgi", 
          "grdWater",      "grndWater_mm_pgi",
          "irrRffStorage",
          "resStorage",     "resStorage_m3_pgi",
          "ricePaddyStrg", 
          "soilMoist",      "soilMoist_mm_pgi",
          "surfRffStorage", "runoffStg_pgi",    # NB: look up if irrRffStorage and/or ricePaddyStrg is included in either of these

         # flux variables       
         # "discharge",  "discharge_m3s_pgi", 
          "baseflow",   "baseflow_mm_pgi",
          "etIrrCrops", "etIrrCrops_mm_pgi",
          "runoff", "runoff_mm_pgi",
          "IrrEvap_mm_pgi", 
          "IrrPercIneff_mm_pgi", 
          "IrrPercRice_mm_pgi", 
          "endoEvap",
          
          # other
          "total_mass")


lapply(vars, FUN = function(var){create_dir(file.path("results/spinup_test/", var))})
years = seq(1980, 2016)  

for(i in sp.char){
  path.base = paste(base.path, i, sep="")
  path.out = "results/spinup_test/"
  for(var in vars){
    extract_ts(raster.path = file.path(path.base, "monthly", var), 
               shp = basins, 
               years, 
               var, 
               row.nm = as.character(basins$name),
               out.nm = paste(path.out, var, "/", "Spinup", i, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep=""),
               check.file = 1)
    print(var)
  }
  print(i)
}


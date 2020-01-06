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

### Function to calculate contribution of water components to agriculture  ###
glacier_ag = function(mod,          # character string: name of model, e.g., "ERA_hist" or "MIROC5"
                      years,        # vector of years over which to process, e.g., seq(2000, 2005)
                      vars,         # list of 2 character strings, matching variables. e.g., c("irrigationGross", "GrossIrr_mm_pgi")
                      path.base,    # character string: path to model output, e.g., "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12"
                      path.out,     # character string: path to write output, e.g., "/results"
                      basins        # shapefile: basins over which to aggregate
                      ){
  
  # create file path for model-specific output, if it does not already exist.
  create_dir(file.path("results", mod))
  
  ### File paths ###
  path.yr   = file.path(path.base, mod, "yearly")
  path.mo   = file.path(path.base, mod, "monthly")
  path.c    = file.path(path.base, mod, "climatology")
  
  # names
  percent.nm = sub("mm", "percent", c(vars[2]))
  out.var = sub("mm_", "", c(vars[2]))
 
  # 1. annual contribution
  basin.agg.pgi = agg_contribution(path.yr, basins, vars, years, percent.nm) 
  out.nm = paste(path.out, mod, "/", mod, "_basin_", out.var, "_yearly.csv", sep="")
  write.table(basin.agg.pgi, 
              out.nm,
              sep=",")
  
  # 2. monthly contribution (NB: this step takes a long time)
  basin.agg.pgi.m = agg_contribution(path.mo, basins, vars, years, percent.nm) 
  out.nm = paste(path.out, mod, "/", mod, "_basin_", out.var, "_monthly.csv", sep="")
  write.table(basin.agg.pgi.m, 
              out.nm,
              sep=",")
  
  
  # 2. Calculate month of max km3
  irr_pgi_max_month = max_month(var.m = basin.agg.pgi.m,
                                var   = "pgi",
                                unit  = "km3")
  out.nm = paste(path.out, mod, "/", mod, "_basin_", out.var, "_km3_month_max.csv", sep="")
  write.table(irr_pgi_max_month, 
              out.nm,
              quote = F,
              sep=",")
  
  # 4. Calculate month of max % 
  irr_pgi_max_month.percent = max_month(var.m = basin.agg.pgi.m,
                                        var   = "pgi",
                                        unit  = "percent")
  out.nm = paste(path.out, mod, "/", mod, "_basin_", out.var, "_percent_month_max.csv", sep="")
  write.table(irr_pgi_max_month, 
              out.nm,
              quote = F,
              sep=",")

} # end of function

#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

mod = "ERA_hist"
years = seq(2000, 2001)  # for testing

# Inputs: same for all variables
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
path.base = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12"
path.out  = "results/"
map.dir   = "figures/"

# Inputs: variable pairs for which to calculate contributions
# NB  _pgi := glacier ice melt. 
#     _pgn := glacier non-ice runoff (e.g., rain on the glacier)

# 1. Ice melt (pgi) component in gross irrigation
vars = c("irrigationGross",
         "GrossIrr_mm_pgi")
glacier_ag(mod, years, vars, path.base, path.out, basins)

vars = c("irrigationGross",
         "IrrFlow_mm_pgi")
glacier_ag(mod, years, vars, path.base, path.out, basins)

vars = c("irrigationGross",
         'IrrGrwt_mm_pgi')
glacier_ag(mod, years, vars, path.base, path.out, basins)


# end of MAIN
#######################################################################################################################################

# sandbox:

# # inputs needed to identify basin mouths - not used yet
# basin.ID = raster(file.path(netwk.path, "HiMAT_full_210_IDs_Subset.asc"))
# up.area  = raster(file.path(netwk.path,"flowdirection210_upstrArea.asc"))
# 
# # find IDs for exorheic basins
# ex.basins = basin.shape[basin.shape$name == "Ganges" |
#                           basin.shape$name == "Mekong" |
#                           basin.shape$name == "Irawaddy" |
#                           basin.shape$name == "Luni_ext" |
#                           basin.shape$name == "Indus" |
#                           basin.shape$name == "Brahmaputra" |
#                           basin.shape$name == "Salween" |
#                           basin.shape$name == "Yangtze" |
#                           basin.shape$name == "Yellow",]

# netwk.path = "/net/nfs/zero/data3/WBM_TrANS/data"


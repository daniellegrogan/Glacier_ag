# basin_aggregation()

# aggregate WBM results by basin, and calculate yearly sums

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
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

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)


#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

# shapefile for spatial agg
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over

# spatial aggregation
vars = c("irrigationGross", "GrossIrr_mm_pgi", "GrossIrr_mm_pgn", "GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu",   # Gross irrigation variables
         "irrigationFlow", "IrrFlow_mm_pgi", "IrrFlow_mm_pgn", "IrrFlow_mm_ps", "IrrFlow_mm_pr", "IrrFlow_mm_pu",         # surface water irrigation variables
         "irrigationGrwt", "IrrGrwt_mm_pgi", "IrrGrwt_mm_pgn", "IrrGrwt_mm_ps", "IrrGrwt_mm_pr", "IrrGrwt_mm_pu",         # ground water irrigation variables
         'runoff', 'irrRunoff', 'snowMelt', 'snowFall', 'precip', 'irrigationGrwt', 'irrigationExtra', 'glMelt',          # water sources
         'baseflow_mm_pgi', 'etIrrCrops', 'soilMoist_mm_pgi')                                                             # other useful vars
vars = c("etIrrCrops", "etIrrCrops_mm_pgi", "etIrrCrops_mm_pgn", "etIrrCrops_mm_ps", "etIrrCrops_mm_pr", "etIrrCrops_mm_pu")
vars = c("IrrEvap_mm_pgi", "endoStrg_mm_pgi", "IrrEvap_mm_pgn", "endoStrg_mm_pgn")
lapply(vars, FUN = function(var){create_dir(file.path("results", var))})

# test
for(v in vars){
  file.nm.y = paste("results/", v, "/ERA_hist_basin_", v, "_km3_1980_2009_yearly.csv", sep="")
  file.nm.m = paste("results/", v, "/ERA_hist_basin_", v, "_km3_1980_2009_monthly.csv", sep="")
  if(!file.exists(file.nm.y)){
    print(paste("yearly file", v, "missing"))
  }
  if(!file.exists(file.nm.m)){
    print(paste("monthly file", v, "missing"))
  }
}

############ ERA HISTORICAL ################
mod = "ERA_hist"
path.out  = "results"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)
years = seq(1980, 2009)  # climatology historical time series

monthly.agg = lapply(vars, function(var) extract_ts(raster.path = file.path(path.base, "monthly", var), 
                                                    shp = basins, 
                                                    years, 
                                                    var, 
                                                    row.nm = as.character(basins$name),
                                                    out.nm = paste(path.out, "/", var, "/", mod, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep=""),
                                                    check.file = 0))
# sum monthly aggregates to yearly
yearly.agg = lapply(vars, function(var) monthly_to_yearly(data.m = read.csv(paste(path.out, "/", var, "/", mod, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")),
                                                          out.nm =          paste(path.out, "/", var, "/", mod, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")))

yc.agg = lapply(vars, function(var) yearly_to_yc(data.y = read.csv(paste(path.out, "/", var, "/", mod, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")),
                                                 years = seq(1980,2009),
                                                 out.nm = paste(path.out, "/", var, "/", mod, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yc.csv", sep="")))
# monthly aggregates to monthly climatology
mc.agg = lapply(vars, function(var) monthly_to_mc(data.m = read.csv(paste(path.out, "/", var, "/", mod, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")),
                                                  years  = seq(1980, 2009),
                                                  out.nm =          paste(path.out, "/", var, "/", mod, "_basin_", var, "_km3_",  min(years), "_", max(years), "_mc.csv", sep="")))

############ GCM HISTORICAL ################
mods  = c("CCSM4", "MIROC5")
r = "historical"
path.out  = "results"
years = seq(2000, 2005) 

for(m in mods){
  path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", m, r)
  monthly.agg = lapply(vars, function(var) extract_ts(raster.path = file.path(path.base, "monthly"), 
                                                      shp = basins, 
                                                      years, 
                                                      var, 
                                                      row.nm = as.character(basins$name),
                                                      out.nm = paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")))
  # sum monthly aggregates to yearly
  yearly.agg = lapply(vars, function(var) monthly_to_yearly(data.m = read.csv(paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")),
                                                            out.nm =          paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")))
  
  
}

############ GCM Future ################

vars = c("irrigationGross", "GrossIrr_mm_pgi", "GrossIrr_mm_pgn", "GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu",   # Gross irrigation variables
         "irrigationFlow", "IrrFlow_mm_pgi", "IrrFlow_mm_pgn", "IrrFlow_mm_ps", "IrrFlow_mm_pr", "IrrFlow_mm_pu",         # surface water irrigation variables
         "irrigationGrwt", "IrrGrwt_mm_pgi", "IrrGrwt_mm_pgn", "IrrGrwt_mm_ps", "IrrGrwt_mm_pr", "IrrGrwt_mm_pu",         # ground water irrigation variables
         'runoff', 'irrRunoff', 'snowMelt', 'snowFall', 'precip', 'irrigationGrwt', 'irrigationExtra', 'glMelt',          # water sources
         'baseflow_mm_pgi', 'etIrrCrops', 'soilMoist_mm_pgi')                                                             # other useful vars

vars = c("etIrrCrops", "etIrrCrops_mm_pgi", "etIrrCrops_mm_pgn", "etIrrCrops_mm_ps", "etIrrCrops_mm_pr", "etIrrCrops_mm_pu")


mods  = c("CanESM2", 
          "CNRM-CM5", 
          "MPI-ESM-LR", 
          "NorESM1-M", 
          "MIROC5",
          "CCSM4",
          "bcc-csm1-1", 
          "CESM1-CAM5", 
          "CSIRO-Mk3-6-0",
          "GFDL-CM3",
          "GFDL-ESM2M", 
          "GISS-E2-R", 
          "IPSL-CM5A-LR",  
          "IPSL-CM5A-MR",  
          "MIROC-ESM", 
          "MIROC-ESM-CHEM", 
          "MRI-CGCM3", 
          "NorESM1-ME")
rcp = c("rcp26", "rcp45", "rcp85")
path.out  = "results"
years = seq(2006, 2099) 

for(m in mods){
  for(r in rcp){
    path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", m, r)
    monthly.agg = lapply(vars, function(var) extract_ts(raster.path = file.path(path.base, "monthly"),
                                                        shp = basins,
                                                        years,
                                                        var,
                                                        row.nm = as.character(basins$name),
                                                        out.nm = paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")))
    removeTmpFiles(h=4) # remove temporary files older than 4 hours
  }
}



####################################################################################################################################################################
# fix monthly to yearly and clim

vars = c("irrigationGross", "GrossIrr_mm_pgi", "GrossIrr_mm_pgn", "GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu",   # Gross irrigation variables
         "irrigationFlow", "IrrFlow_mm_pgi", "IrrFlow_mm_pgn", "IrrFlow_mm_ps", "IrrFlow_mm_pr", "IrrFlow_mm_pu",         # surface water irrigation variables
         "irrigationGrwt", "IrrGrwt_mm_pgi", "IrrGrwt_mm_pgn", "IrrGrwt_mm_ps", "IrrGrwt_mm_pr", "IrrGrwt_mm_pu",         # ground water irrigation variables
         'runoff', 'irrRunoff', 'snowMelt', 'snowFall', 'precip', 'irrigationGrwt', 'irrigationExtra', 'glMelt',          # water sources
         'baseflow_mm_pgi', 'etIrrCrops', 'soilMoist_mm_pgi')                                                             # other useful vars

mods  = c("CanESM2", 
          "CNRM-CM5", 
          "MPI-ESM-LR", 
          "NorESM1-M", 
          "MIROC5",
          "CCSM4",
          "bcc-csm1-1", 
          "CESM1-CAM5", 
          "CSIRO-Mk3-6-0",
          "GFDL-CM3",
          "GFDL-ESM2M", 
          "GISS-E2-R", 
          "IPSL-CM5A-LR",  
          "IPSL-CM5A-MR",  
          "MIROC-ESM", 
          "MIROC-ESM-CHEM", 
          "MRI-CGCM3", 
          "NorESM1-ME")

vars = c("etIrrCrops", "etIrrCrops_mm_pgi", "etIrrCrops_mm_pgn", "etIrrCrops_mm_ps", "etIrrCrops_mm_pr", "etIrrCrops_mm_pu")

rcp = c("rcp26", "rcp45", "rcp60", "rcp85")
path.out  = "results"
years = seq(2006, 2099) 

for(m in mods){
  for(r in rcp){
    for(var in vars){
      # check if file exist
      m.file = paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")
      if(file.exists(m.file)){
        
        monthly_to_yearly(data.m = read.csv(paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")),
                          out.nm =          paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep=""))
        
        
        clim.yrs  = seq(2040, 2069)
        yearly_to_yc(data.y = read.csv(paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")),
                     years = clim.yrs,
                     out.nm =          paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(clim.yrs), "_", max(clim.yrs), "_yc.csv", sep=""))
        
        monthly_to_mc(data.m = read.csv(paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")),
                      years = clim.yrs,
                      out.nm =          paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(clim.yrs), "_", max(clim.yrs), "_mc.csv", sep=""))
        
        
        clim.yrs  = seq(2070, 2099)
        yearly_to_yc(data.y = read.csv(paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")),
                     years = clim.yrs,
                     out.nm =          paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(clim.yrs), "_", max(clim.yrs), "_yc.csv", sep=""))
        
        monthly_to_mc(data.m = read.csv(paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")),
                      years = clim.yrs,
                      out.nm =          paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(clim.yrs), "_", max(clim.yrs), "_mc.csv", sep=""))
      }
    }
  }
}



# glacier ice melt

mods  = c("CanESM2", 
          "CNRM-CM5", 
          "MPI-ESM-LR", 
          "NorESM1-M", 
          "MIROC5",
          "CCSM4",
          "bcc-csm1-1", 
          "CESM1-CAM5", 
          "CSIRO-Mk3-6-0",
          "GFDL-CM3",
          "GFDL-ESM2M", 
          "GISS-E2-R", 
          "IPSL-CM5A-LR",  
          "IPSL-CM5A-MR",  
          "MIROC-ESM", 
          "MIROC-ESM-CHEM", 
          "MRI-CGCM3", 
          "NorESM1-ME")

rcp = c("rcp26", "rcp45", "rcp85")
for(m in mods){
  for(r in rcp){
    clim.yrs  = seq(2040, 2069)
    monthly_to_mc(data.months = read.csv(paste("results/Glacier_ice_melt/", m, "_", r, "_glacier_melt_basins_monthly.csv", sep="")), 
                  clim.yrs, 
                  out.nm = paste("results/Glacier_ice_melt/", m, "_", r, "_glacier_melt_basins_", min(clim.yrs), "_", max(clim.yrs), "_mc.csv", sep=""))
    
    clim.yrs  = seq(2070, 2099)
    monthly_to_mc(data.months = read.csv(paste("results/Glacier_ice_melt/", m, "_", r, "_glacier_melt_basins_monthly.csv", sep="")), 
                  clim.yrs, 
                  out.nm = paste("results/Glacier_ice_melt/", m, "_", r, "_glacier_melt_basins_", min(clim.yrs), "_", max(clim.yrs), "_mc.csv", sep=""))
    
  }
}

# Rainfall

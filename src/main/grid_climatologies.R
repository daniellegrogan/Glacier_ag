# grid_climatologies()
# Generate climatology grids of WBM output
# project: NASA HiMAT
# Danielle S Grogan

# Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgdal)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))

# raster_time_ave()  for annual climatologies
raster_time_ave.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/raster_time_ave.R", ssl.verifypeer=F)
eval(parse(text=raster_time_ave.script))

# raster_monthly_ave()  for monthly climatologies
raster_monthly_ave.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/raster_monthly_ave.R", ssl.verifypeer=F)
eval(parse(text=raster_monthly_ave.script))

# create_dir()
create_dir.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/create_dir.R", ssl.verifypeer=F)
eval(parse(text=create_dir.script))

#####################################################################################################################################
# MAIN #
base.path = ("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12")
#vars = c("discharge", "discharge_m3s_pgi", "discharge_m3s_pgn", "discharge_m3s_pu", "discharge_m3s_ps", "discharge_m3s_pr")
# vars = c("irrigationGross", "GrossIrr_mm_pgi", "GrossIrr_mm_pgn")
# vars = c("etIrrCrops", "etIrrCrops_mm_pgi", "etIrrCrops_mm_pgn", "etIrrCrops_mm_ps", "etIrrCrops_mm_pr", "etIrrCrops_mm_pu")
# vars = c("GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu")

vars = c("irrigationFlow", "IrrFlow_mm_pgi", "IrrFlow_mm_pgn", "IrrFlow_mm_ps", "IrrFlow_mm_pr", "IrrFlow_mm_pu",         # surface water irrigation variables
         "irrigationGrwt", "IrrGrwt_mm_pgi", "IrrGrwt_mm_pgn", "IrrGrwt_mm_ps", "IrrGrwt_mm_pr", "IrrGrwt_mm_pu",         # ground water irrigation variables
         'runoff', 'irrRunoff', 'snowMelt', 'snowFall', 'precip', 'irrigationGrwt', 'irrigationExtra', 'glMelt',          # water sources
         'baseflow_mm_pgi', 'etIrrCrops', 'soilMoist_mm_pgi')                                                             # other useful vars

lapply(vars, FUN = function(var){create_dir(file.path("results/grid_climatology", var))})

# ERA historical
mod = "ERA_hist"
clim.yrs = seq(1980, 2009)

lapply(vars, FUN = function(x) raster_monthly_ave(brk.data = wbm_load(path    = file.path(base.path, mod, "monthly", x),
                                                                      varname = x,
                                                                      years   = clim.yrs),
                                                  out.dir = file.path("results/grid_climatology", x),
                                                  out.nm = paste(mod, "_", x, "_", min(clim.yrs), "_", max(clim.yrs), "_mc.nc", sep=""),
                                                  r = 0)
)

#####################################################################################################################################
# GCMs 

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


# early
clim.yrs.early = seq(2010, 2039)
for(r in rcp){
  for(mod in mods){
    lapply(vars, FUN = function(x) raster_monthly_ave(brk.data = wbm_load(path    = file.path(base.path, mod, r, "monthly"),
                                                                          varname = x,
                                                                          years   = clim.yrs.early),
                                                      out.dir = file.path("results/grid_climatology", x),
                                                      out.nm = paste(mod, "_", x, "_", min(clim.yrs.early), "_", max(clim.yrs.early), "_mc.nc", sep=""),
                                                      r = 0)
    )
    print(mod)
  }
  print(r)
}

# mid
clim.yrs.mid   = seq(2040, 2069)
for(r in rcp){
  for(mod in mods){
    lapply(vars, FUN = function(x) raster_monthly_ave(brk.data = wbm_load(path    = file.path(base.path, mod, r, "monthly"),
                                                                          varname = x,
                                                                          years   = clim.yrs.mid),
                                                      out.dir = file.path("results/grid_climatology", x),
                                                      out.nm = paste(mod, "_", x, "_", min(clim.yrs.mid), "_", max(clim.yrs.mid), "_mc.nc", sep=""),
                                                      r = 0)
    )
    print(mod)
  }
  print(r)
}

# late
clim.yrs.late  = seq(2070, 2099)
for(r in rcp){
  for(mod in mods){
    lapply(vars, FUN = function(x) raster_monthly_ave(brk.data = wbm_load(path    = file.path(base.path, mod, r, "monthly"),
                                                                          varname = x,
                                                                          years   = clim.yrs.late),
                                                      out.dir = file.path("results/grid_climatology", x),
                                                      out.nm = paste(mod, "_", x, "_", min(clim.yrs.late), "_", max(clim.yrs.late), "_mc.nc", sep=""),
                                                      r = 0)
    )
    print(mod)
  }
  print(r)
}


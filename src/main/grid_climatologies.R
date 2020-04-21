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
#####################################################################################################################################
# MAIN #
base.path = ("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12")
vars = c("discharge", "discharge_m3s_pgi", "discharge_m3s_pgn", "discharge_m3s_pu", "discharge_m3s_ps", "discharge_m3s_pr")

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
    print(m)
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
    print(m)
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
    print(m)
  }
  print(r)
}


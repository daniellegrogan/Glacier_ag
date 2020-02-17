# glacier_ag()

# Glacier contributions to agriculture paper: 
#    Analysis: how much does glacier meltwater contribute to irrigation?
# project: NASA HiMAT
# Danielle S Grogan

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
# writes 10 csv files for each pair of input vars

glacier_ag = function(mod,          # character string: name of model, e.g., "ERA_hist" or "MIROC5"
                      rcp,          # character string: one of "historical", "rcp45", or "rcp85"
                      years,        # vector of years over which to process, e.g., seq(2000, 2005)
                      vars,         # list of 2 character strings, matching variables. e.g., c("irrigationGross", "GrossIrr_mm_pgi")
                      path.base,    # character string: path to model output, e.g., "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12"
                      path.out,     # character string: path to write output, e.g., "/results"
                      basins        # shapefile: basins over which to aggregate
                      ){

  ### File paths ###
  path.yr   = file.path(path.base, "yearly")
  path.mo   = file.path(path.base, "monthly")
  path.c    = file.path(path.base, "climatology")
  
  # names
  percent.nm = sub("mm", "percent", c(vars[2]))
  out.var = sub("mm_", "", c(vars[2]))
 
  # 1. annual contribution
  if(mod == "ERA_hist"){
    m.char = mod
  }else{
    m.char = paste(mod, "_", rcp, sep="")
  }
  
  out.nm.pre = paste(path.out, "/Irrigation/", vars[1], "/", m.char, "_basin_", sep="")
  out.nm     = paste(path.out, "/Irrigation/", vars[1], "/", m.char, "_basin_", out.var, "_",  min(years), "_", max(years), "_yearly_stats.csv", sep="")
  
  basin.agg.pgi = agg_contribution(path.yr, basins, vars, years, percent.nm, out.nm.pre)
  write.table(basin.agg.pgi, 
              out.nm,
              sep=",")
  
  # 2. monthly contribution (NB: this step takes a long time)
  basin.agg.pgi.m = agg_contribution(path.mo, basins, vars, years, percent.nm, out.nm.pre) 
  out.nm = paste(path.out, "/Irrigation/", vars[1], "/", m.char, "_basin_", out.var, "_",  min(years), "_", max(years), "_monthly_stats.csv", sep="")
  write.table(basin.agg.pgi.m, 
              out.nm,
              sep=",")
  
  
  # 3. Calculate month of max km3
  s = unlist(strsplit(vars[2], "_"))
  v_suffix = s[length(s)]
  irr_pgi_max_month = max_month(var.m = basin.agg.pgi.m,
                                var   = v_suffix,
                                unit  = "km3")
  out.nm = paste(path.out, "/Irrigation/", vars[1], "/", m.char, "_basin_", out.var, "_",  min(years), "_", max(years), "_km3_month_max.csv", sep="")
  write.table(irr_pgi_max_month, 
              out.nm,
              quote = F,
              sep=",")
  
  # 4. Calculate month of max % 
  irr_pgi_max_month.percent = max_month(var.m = basin.agg.pgi.m,
                                        var   = v_suffix,
                                        unit  = "percent")
  out.nm = paste(path.out, "/Irrigation/", vars[1], "/", m.char, "_basin_", out.var, "_",  min(years), "_", max(years), "_percent_month_max.csv", sep="")
  write.table(irr_pgi_max_month.percent, 
              out.nm,
              quote = F,
              sep=",")

  removeTmpFiles(h=6) # remove temporary files older than 6 hours
  
} # end of function

#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

# shapefile for spatial agg
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over

############ HISTORICAL ################
mod = "ERA_hist"
path.out  = "results"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod)
years = seq(1980, 2009)  # climatology historical time series
r = 'historical'


# spatial aggregation
vars = c("irrigationGross", "GrossIrr_mm_pgi", "GrossIrr_mm_pgn", "GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu")
monthly.agg = lapply(vars, function(var) extract_ts(raster.path = file.path(path.base, "monthly", var), 
                                                    shp = basins, 
                                                    years, 
                                                    var, 
                                                    row.nm = as.character(basins$name),
                                                    out.nm = paste(path.out, "/Irrigation/", var, "/ERA_hist_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")))

# sum monthly aggregates to yearly
yearly.agg = lapply(vars, function(var) monthly_to_yearly(data.m = read.csv(paste(path.out, "/Irrigation/", var, "/ERA_hist_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")),
                                                          out.nm = paste(path.out, "/Irrigation/", var, "/ERA_hist_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")))
data.m = read.csv("results/Irrigation/irrigationGross/ERA_hist_basin_irrigationGross_km3_1980_1981_monthly.csv")





# Inputs: variable pairs for which to calculate contributions
# NB  _pgi := glacier ice melt. 
#     _pgn := glacier non-ice runoff (e.g., rain on the glacier)

# 1. Ice melt (pgi) component in irrigation
vars = c("irrigationGross",
         "GrossIrr_mm_pgi")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationFlow",
         "IrrFlow_mm_pgi")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationGrwt",
         'IrrGrwt_mm_pgi')
glacier_ag(mod, r, years, vars, path.base, path.out, basins)


# 2. Glacier runoff (pgn) component in irrigation
vars = c("irrigationGross",
         "GrossIrr_mm_pgn")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationFlow",
         "IrrFlow_mm_pgn")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationGrwt",
         'IrrGrwt_mm_pgn')
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

# 3. Snow melt (ps) component in irrigation
vars = c("irrigationGross",
         "GrossIrr_mm_ps")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationFlow",
         "IrrFlow_mm_ps")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationGrwt",
         'IrrGrwt_mm_ps')
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

# 4. UGW (pu) component in irrigation
vars = c("irrigationGross",
         "GrossIrr_mm_pu")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationFlow",
         "IrrFlow_mm_pu")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationGrwt",
         'IrrGrwt_mm_pu')
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

# 5. Rainfall (pr) component in irrigation
vars = c("irrigationGross",
         "GrossIrr_mm_pr")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationFlow",
         "IrrFlow_mm_pr")
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

vars = c("irrigationGrwt",
         'IrrGrwt_mm_pr')
glacier_ag(mod, r, years, vars, path.base, path.out, basins)

############ FUTURE ################
mods  = c("CCSM4", "MIROC5")
rcp = c("historical", "rcp26", "rcp45", "rcp85") 
hist.yrs  = seq(2000, 2005) 
fut.yrs = seq(2006, 2099)

path.out  = "results"

for(mod in mods){
  for(r in rcp){
    path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", mod, r)
    if(r == "historical"){
      years = hist.yrs
    }else{
      years = fut.yrs
    }
    
    # 1. Ice melt (pgi) component in irrigation
    vars = c("irrigationGross",
             "GrossIrr_mm_pgi")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationFlow",
             "IrrFlow_mm_pgi")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationGrwt",
             'IrrGrwt_mm_pgi')
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    # 2. Glacier runoff (pgn) component in irrigation
    vars = c("irrigationGross",
             "GrossIrr_mm_pgn")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationFlow",
             "IrrFlow_mm_pgn")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationGrwt",
             'IrrGrwt_mm_pgn')
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    # 3. Snow melt (ps) component in irrigation
    vars = c("irrigationGross",
             "GrossIrr_mm_ps")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationFlow",
             "IrrFlow_mm_ps")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationGrwt",
             'IrrGrwt_mm_ps')
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    # 4. UGW (pu) component in irrigation
    vars = c("irrigationGross",
             "GrossIrr_mm_pu")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationFlow",
             "IrrFlow_mm_pu")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationGrwt",
             'IrrGrwt_mm_pu')
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    # 5. Rainfall (pr) component in irrigation
    vars = c("irrigationGross",
             "GrossIrr_mm_pr")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationFlow",
             "IrrFlow_mm_pr")
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
    vars = c("irrigationGrwt",
             'IrrGrwt_mm_pr')
    glacier_ag(mod, r, years, vars, path.base, path.out, basins)
    
  
  }
}


# end of MAIN
#######################################################################################################################################

# sandbox:

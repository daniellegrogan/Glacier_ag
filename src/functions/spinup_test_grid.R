# test spinup time: just look at first year results, grid-level

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

#######################################################################################################################################
getmode <- function(x) {
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#######################################################################################################################################
### MAIN ####
#######################################################################################################################################

# shapefile for clipping gridded results to relevant area
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over

# spinup test runs
base.path = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_09/ERA_hist_"
sp.char = c("30", "60", "300")
out.path = "results/spinup_test_grid/"

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


out.table = as.data.frame(matrix(nr = length(vars), nc = 13))
colnames(out.table) = c("var", "min", "min_percent", "max", "max_percent", "mean", "mean_percent", "n.zero", "n.neg", "n.pos", "p.zero", "p.neg", "p.pos")
out.table[,1] = vars

for(v in 1:length(vars)){
  
  # load data from first year of simulation
  # grid.yr.30  = mask(raster(file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_09/ERA_hist_30/yearly/",  vars[v], "wbm_1980.nc")), basins)
  grid.yr.60  = mask(raster(file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_09/ERA_hist_60/yearly/",  vars[v], "wbm_1980.nc")), basins)
  grid.yr.300 = mask(raster(file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2020_09/ERA_hist_300/yearly/", vars[v], "wbm_1980.nc")), basins)
  
  # compare 300 to 60
  d.300.60         = grid.yr.300 - grid.yr.60
  d.300.60.percent = 100*(d.300.60/grid.yr.300)
  d.300.60.percent[d.300.60.percent == -Inf] = NA
    
  n.active.cells = sum(as.matrix(!is.na(d.300.60)))
  
  # fill in metrics
  out.table$min[v]          = min(as.matrix(d.300.60), na.rm=T)
  out.table$min_percent[v]  = min(as.matrix(d.300.60.percent), na.rm=T)
  out.table$max[v]          = max(as.matrix(d.300.60), na.rm=T)
  out.table$max_percent[v]  = max(as.matrix(d.300.60.percent), na.rm=T)
  out.table$mean[v]         = cellStats(d.300.60, mean)
  out.table$mean_percent[v] = cellStats(d.300.60.percent, mean)
  out.table$n.zero[v]       = sum(as.matrix(d.300.60 == 0), na.rm=T)
  out.table$n.neg[v]        = sum(as.matrix(d.300.60 < 0), na.rm=T)
  out.table$n.pos[v]        = sum(as.matrix(d.300.60 > 0), na.rm=T)
  out.table$p.zero[v]       = 100*out.table$n.zero[v]/n.active.cells
  out.table$p.neg[v]        = 100*out.table$n.neg[v]/n.active.cells
  out.table$p.pos[v]        = 100*out.table$n.pos[v]/n.active.cells
  
  print(vars[v])
}

write.csv("results/spinup_test_grid/Grid_compare_300_minus_60_stats.csv", row.names=F)








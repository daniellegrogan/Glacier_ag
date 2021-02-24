# sandbox

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

# spatial_aggregation()
spatial_aggregation.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/spatial_aggregation.R", ssl.verifypeer=F)
eval(parse(text=spatial_aggregation.script))

############################################################################################################################################

############################################################################################################################################
#### Storage of glacier water ####
############################################################################################################################################

# soilMoist_mm_pgi              : storage in soil moisture
# grndWater_mm_pgi              : storage in shallow renewable groundwater
# resStorage_m3_pgi             : storage in "reservoirs", := dams and in rivers
# irrRffSTroage_mm_pgi          : NOT YET INCLUDED IN OUTPUT irrigation runoff surface storage (includes rice paddy flood water)
# surfRffStorage_mm_pgi         : NOT YET INCLUDED IN OUTPUT surface runoff storage
# cEndoP_pgi * endoStrg         : storage in endorheic lakes.  Need to make a mm or m3 output available

# glVolume : NB this is the total volume of the glacier, not just the ice portion. Don't need to include this in the balance, just listing for completeness

# storages not used in these simulations, but would need to be included if they were turned on:
# smResStrg
# Aqf_Storage
############################################################################################################################################
#### Fluxes of glacier water ####
############################################################################################################################################

### Fluxes that move water around on the land surface, but do not change to basin-level mass balance ###
# runoff_mm_pgi        : runoff from land surface to rivers, including irrigation runoff
# baseflow_mm_pgi      : baseflow from shallow groundwater to rivers
# irrigationFlow_mm_pgi: water extracted from rivers and applied to soil for irrigation
# IrrPercRice_mm_pgi   : percolation from rice paddies to shallow groundwater
# IrrPercIneff_mm_pgi  : percolation of the inefficient portion of irrigation (the portion of (Irr Gross - Irr Net that percolates))
# irrigationGrwt_mm_pgi: water extracted from shallow groundwater and applied to soil for irrigation
# discharge_m3s_pgi    : flux through river systems. Discharge to ocean = final destination (See below) 


### Fluxes to Final Destinations (change basin-level mass balance) ###

## Destination: ocean ##
# discharge_m3s_pgi AT RIVER MOUTH : flux of water to the ocean

## Destination: atmosphere ##
# endoEvap_xxx_pgi                     : NOT YET INCLUDED IN OUTPUT evaporation out of endorheic lakes (endoEvap is in m3/pixel/day)
# IrrEvap_mm_pgi                   : evaporation due to inefficient irrigation (the portion of (Irr Gross - Irr Net that evaporates))
# etIrrCrops_mm_pgi                : evapotranspiration from crops
# openWaterEvap_mm_pgi             : NOT YET INCLUDED IN OUTPUT evap from open water (but not endorheic lakes) 

### Special case fluxes ###
# varname?? : inter-basin transfers (NB: This changes the basin-level mass balance, but is not yet a flux to a final destination)


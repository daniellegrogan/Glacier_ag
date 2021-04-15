# Mass balance table

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

# mouth_ts()
mouth_ts.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/mouth_ts.R", ssl.verifypeer=F)
eval(parse(text=mouth_ts.script))

### Source functions from within this project:
file.sources = list.files("src/functions", full.names = T)
sapply(file.sources, source)

########################################################################################################################
#### FLUX APPROACH ###
########################################################################################################################

# glacier ice melt
glMelt.y = read.csv("results/glMelt/ERA_hist_basin_glMelt_km3_1980_2009_yearly.csv")
glMelt.mc = read.csv("results/glMelt/ERA_hist_basin_glMelt_km3_1980_2009_mc.csv")

# yc
glMelt.y = glMelt.y[,3:ncol(glMelt.y)]
rownames(glMelt.y) = glMelt.mc$Basin
data.yc = rowMeans(glMelt.y, na.rm=T)
data.sd = apply(glMelt.y, MARGIN=1, FUN=sd)

out = cbind(data.yc, data.sd)
colnames(out) = c("Mean", "Stdev")
write.csv(out, "results/Tables/ERA_hist_basin_glMelt_yc.csv", quote = F, row.names = F)


# crop ET of pgi
etIrrCrops_pgi = read.csv("results/etIrrCrops_mm_pgi/ERA_hist_basin_etIrrCrops_mm_pgi_km3_1980_2009_monthly.csv")
etIrrCrops_pgi.y = monthly_to_yearly(etIrrCrops_pgi)
etIrrCrops_pgi.mc = monthly_to_mc(etIrrCrops_pgi, years = seq(1980, 2009))

# yc
etIrrCrops_pgi.y = etIrrCrops_pgi.y[,2:ncol(etIrrCrops_pgi.y)]
rownames(etIrrCrops_pgi.y) = etIrrCrops_pgi.mc$Basin
data.yc = rowMeans(etIrrCrops_pgi.y, na.rm=T)
data.sd = apply(etIrrCrops_pgi.y, MARGIN=1, FUN=sd)

out = cbind(data.yc, data.sd)
colnames(out) = c("Mean", "Stdev")
write.csv(out, "results/Tables/ERA_hist_basin_etIrrCrops_pgi_yc.csv", quote = F, row.names = F)



# pgi that reaches the ocean
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
basin.ID<-raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_IDs_Subset.asc")
up.area  = raster("/net/nfs/zero/home/WBM_TrANS/data/HiMAT_full_210_Subset_upstrArea.asc")

# yearly
path     = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/discharge_m3s_pgi/"
varname  = "discharge_m3s_pgi"
yrs = seq(1980, 2009)

q.pgi.table = data.frame(matrix(nr=length(basins$Basin_ID), nc=length(yrs)))
rownames(q.pgi.table) = basins$name
colnames(q.pgi.table) = yrs
for(i in 1:length(basins$Basin_ID)){
  q.pgi.table[i, ] = mouth_ts(basins$Basin_ID[i],            # ID of the basin for which you want data from the mouth
                              basin.ID,      # basin ID file (ascii grid of basin IDs)
                              up.area,       # upstream area file (ascii grid)
                              path,          # path to wbm output
                              varname,       # variable name in wbm output file to extract
                              yrs)           # years of wbm output to extract
}

# discharge at mouth is meaningless for endorheic basins!  Set value to 0
q.pgi.table[c(6,8,10,11,12,14),] = 0

q.pgi.table[16,] = colSums(q.pgi.table, na.rm=T)
rownames(q.pgi.table)[16] = rownames(glMelt.y)[16]

# convert units: m3/s to km3/year
km3_per_1m3 = 1e-09
s_per_1yr   = 3.154e+7
q.pgi.table.km3 = q.pgi.table*km3_per_1m3*s_per_1yr


# yc
data.yc = rowMeans(q.pgi.table.km3, na.rm=T)
data.sd = apply(q.pgi.table.km3, MARGIN=1, FUN=sd)

out = cbind(data.yc, data.sd)
colnames(out) = c("Mean", "Stdev")
write.csv(out, "results/Tables/ERA_hist_basin_Q_pgi_km3_yc.csv", quote = F, row.names = F) # NB: unit is in m3/s

# monthly
# path     = "/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/discharge_m3s_pgi/",
# varname  = "discharge_m3s_ps"
# yrs = seq(1980, 2009)
# 
# q.pgi.table.m = data.frame(matrix(nr=length(basins$Basin_ID), nc=length(yrs*12)+1))
# q.pgi.table[,1] = basins$name
# colnames(q.pgi.table) = colnames(etIrrCrops_pgi)
# for(i in 1:length(basins$Basin_ID)){
#   q.pgi.table[i, ] = mouth_ts(basins$Basin_ID[i],            # ID of the basin for which you want data from the mouth
#                               basin.ID,      # basin ID file (ascii grid of basin IDs)
#                               up.area,       # upstream area file (ascii grid)
#                               path,          # path to wbm output
#                               varname,       # variable name in wbm output file to extract
#                               yrs)           # years of wbm output to extract
# }
# # mc
# q.pgi.table.mc = data.frame(matrix(nr=length(basins$Basin_ID), nc=12))
# rownames(q.pgi.table.mc) = basins$name
# colnames(q.pgi.table.mc) = seq(1,12)
# wbm.data = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/climatology/wbm_discharge_m3s_pgi_mc.nc")
# 
# for(i in 1:length(basins$Basin_ID)){
#   pt = id_mouth(basin.ID, ID = basins$Basin_ID[i], up.area)     # identify basin mouth point
#   q.pgi.table.mc[i, ] = extract(wbm.data, pt) 
# }


### Residual ###
gl_residual = glMelt.y - etIrrCrops_pgi.y - q.pgi.table.km3

data.yc = rowMeans(gl_residual, na.rm=T)
data.sd = apply(gl_residual, MARGIN=1, FUN=sd)

out = cbind(data.yc, data.sd)
colnames(out) = c("Mean", "Stdev")
write.csv(out, "results/Tables/ERA_hist_basin_Residual_yc.csv", quote = F, row.names = F)

################
## Make Table

glMelt.stats      = read.csv("results/Tables/ERA_hist_basin_glMelt_yc.csv")
etIrrCrops.stats  = read.csv("results/Tables/ERA_hist_basin_etIrrCrops_pgi_yc.csv")
Qpgi.stats        = read.csv("results/Tables/ERA_hist_basin_Q_pgi_km3_yc.csv")
residual.stats    = read.csv("results/Tables/ERA_hist_basin_Residual_yc.csv")

############################################################################################################
format_text = function(mean.val, stdev.val){
    out = paste(signif(mean.val, 3), 
                " (", 
                signif(stdev.val, 2), 
                ")", 
                sep="") 
  out
}
############################################################################################################

table_mass_balance = as.data.frame(matrix(nr=nrow(glMelt.stats), nc=4))
colnames(table_mass_balance) = c("Glacier Ice Melt (km3/yr)", "Loss via Crop ET (km3/yr)", "Loss to Ocean (km3/yr)", "Residual (Storage) (km3/yr)")
rownames(table_mass_balance) = rownames(q.pgi.table)
table_mass_balance[,1] =  mapply(function(x,y) format_text(x, y), x = glMelt.stats[,1],     y = glMelt.stats[,2]) 
table_mass_balance[,2] =  mapply(function(x,y) format_text(x, y), x = etIrrCrops.stats[,1], y = etIrrCrops.stats[,2]) 
table_mass_balance[,3] =  mapply(function(x,y) format_text(x, y), x = Qpgi.stats[,1],       y = Qpgi.stats[,2])
table_mass_balance[,4] =  mapply(function(x,y) format_text(x, y), x = residual.stats[,1],   y = residual.stats[,2])
write.csv(table_mass_balance, "results/Tables/Mass_balance_table_km3_yc.csv", quote=F, row.names=T)

############
# percentages
etIrrCrops.p = 100*(etIrrCrops_pgi.y/glMelt.y)
data.yc = rowMeans(etIrrCrops.p, na.rm=T)
data.sd = apply(etIrrCrops.p, MARGIN=1, FUN=sd)
out = cbind(data.yc, data.sd)
colnames(out) = c("Mean", "Stdev")
write.csv(out, "results/Tables/ERA_hist_basin_etIrrCrops_pgi_PercentMelt_yc.csv", quote = F, row.names = F)


Qpgi.p = 100*(q.pgi.table.km3/glMelt.y) 
data.yc = rowMeans(Qpgi.p, na.rm=T)
data.sd = apply(Qpgi.p, MARGIN=1, FUN=sd)
out = cbind(data.yc, data.sd)
colnames(out) = c("Mean", "Stdev")
write.csv(out, "results/Tables/ERA_hist_basin_Q_pgi_PercentMelt_yc.csv", quote = F, row.names = F)


residual.p = 100*(gl_residual/glMelt.y) 
data.yc = rowMeans(residual.p, na.rm=T)
data.sd = apply(residual.p, MARGIN=1, FUN=sd)
out = cbind(data.yc, data.sd)
colnames(out) = c("Mean", "Stdev")
write.csv(out, "results/Tables/ERA_hist_basin_Residual_PercentMelt_yc.csv", quote = F, row.names = F)


etIrrCrops.stats  = read.csv("results/Tables/ERA_hist_basin_etIrrCrops_pgi_PercentMelt_yc.csv")
Qpgi.stats        = read.csv("results/Tables/ERA_hist_basin_Q_pgi_PercentMelt_yc.csv")
residual.stats    = read.csv("results/Tables/ERA_hist_basin_Residual_PercentMelt_yc.csv")


table_mass_balance.p = as.data.frame(matrix(nr=nrow(glMelt.stats), nc=4))
colnames(table_mass_balance.p) = c("Glacier Ice Melt (km3/yr)", "Loss via Crop ET (%)", "Loss to Ocean (%)", "Residual (Storage) (%)")
rownames(table_mass_balance.p) = rownames(q.pgi.table)
table_mass_balance.p[,1] =  mapply(function(x,y) format_text(x, y), x = glMelt.stats[,1],     y = glMelt.stats[,2]) 
table_mass_balance.p[,2] =  mapply(function(x,y) format_text(x, y), x = etIrrCrops.stats[,1], y = etIrrCrops.stats[,2]) 
table_mass_balance.p[,3] =  mapply(function(x,y) format_text(x, y), x = Qpgi.stats[,1],       y = Qpgi.stats[,2])
table_mass_balance.p[,4] =  mapply(function(x,y) format_text(x, y), x = residual.stats[,1],   y = residual.stats[,2])
write.csv(table_mass_balance.p, "results/Tables/Mass_balance_table_percent_yc.csv", quote=F, row.names=T)


########################################################################################################################
#### STORAGE APPROACH ###
########################################################################################################################

# storage vars:
vars = c("resStorage",     "resStorage_mm_pgi",
         "endoStrg",       "endoStrg_mm_pgi", 
         "irrRffStorage",  "surfRffStorage", "runoffStg_mm_pgi",  # runoffStg = irrRffStorage + surfRffStorage
         "soilMoist",      "soilMoist_mm_pgi",
         "grndWater",      "grndWater_mm_pgi"
         )

         




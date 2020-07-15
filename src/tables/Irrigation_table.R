# Tables: irrigation components

############################################################################################################
format_text = function(mean.val, stdev.val){
  if(mean.val == 0 | is.na(mean.val)){
    out = "0"
  }else if(mean.val < 0.01){
    out = "< 0.01"
  }else{
    out = paste(signif(mean.val, 2), 
                " (", 
                signif(stdev.val, 2), 
                ")", 
                sep="") 
  }
  out
}
############################################################################################################

# Table Irr Annual pgi: ERA-Interim historical glacier ice melt, and its contribution to agriculture

mod.hist = "ERA_hist"
irrGross_pgi_y_stats  = read.csv(paste("results/Irrigation/irrigationGross/", mod.hist, "_basin_GrossIrr_pgi_1980_2009_yearly_stats.csv", sep=""))
irrFlow_pgi_y_stats   = read.csv(paste("results/Irrigation/irrigationFlow/",  mod.hist, "_basin_IrrFlow_pgi_1980_2009_yearly_stats.csv", sep=""))
irrGrwt_pgi_y_stats   = read.csv(paste("results/Irrigation/irrigationGrwt/",  mod.hist, "_basin_IrrGrwt_pgi_1980_2009_yearly_stats.csv", sep=""))

# calc mean and stdev for ice melt
icemelt.y_hist = read.csv(file.path("results/Glacier_ice_melt", paste(mod.hist, "_glacier_melt_basins_yearly.csv", sep="")), sep=",")
icemelt.y_hist = icemelt.y_hist[2:ncol(icemelt.y_hist)]

all_basin_sums = apply(icemelt.y_hist, c(2), sum)
icemelt.y_hist[16,] = as.numeric(all_basin_sums)

icemelt.y_mean  = rowMeans(icemelt.y_hist)
icemelt.y_stdev = apply(icemelt.y_hist, c(1), FUN = sd) 

icemelt_y_stats = cbind(icemelt.y_mean, icemelt.y_stdev)
rownames(icemelt_y_stats) = rownames(irrGross_pgi_y_stats)

write.table(icemelt_y_stats, "results/ERA_hist/ERA_hist_glacier_melt_basins_yearly_stats.csv", sep=",")


# Format table:
table_irr_annual = data.frame(matrix(nr = 16, nc = 11))
colnames(table_irr_annual) = c("Basin",
                      "Total Ice Melt", 
                      "Gross Irrigation",         "Ice Melt used in Irrigation",          "Ice Melt in Irrigation (%)",
                      "Surface Water Irrigation", "Ice Melt in Surface Water Irrigation", "Ice Melt in Surface Water Irrigation (%)",
                      "Ground Water Irrigation",  "Ice Melt in Ground Water Irrigation",  "Ice Melt in Ground Water Irrigation (%)")
table_irr_annual[,1] = gsub("_", " ", rownames(irrGross_pgi_y_stats))
table_irr_annual[,2] =  mapply(function(x,y) format_text(x, y), x = icemelt_y_stats[,1],                       y = icemelt_y_stats[,2]) 
table_irr_annual[,3] =  mapply(function(x,y) format_text(x, y), x = irrGross_pgi_y_stats$irrigationGross,      y = irrGross_pgi_y_stats$irrigationGross_stdev) 
table_irr_annual[,4] =  mapply(function(x,y) format_text(x, y), x = irrGross_pgi_y_stats$GrossIrr_km3_pgi,     y = irrGross_pgi_y_stats$GrossIrr_mm_pgi_stdev) 
table_irr_annual[,5] =  mapply(function(x,y) format_text(x, y), x = irrGross_pgi_y_stats$GrossIrr_percent_pgi, y = irrGross_pgi_y_stats$GrossIrr_percent_pgi_stdev) 
table_irr_annual[,6] =  mapply(function(x,y) format_text(x, y), x = irrFlow_pgi_y_stats$irrigationFlow,        y = irrFlow_pgi_y_stats$irrigationFlow_stdev) 
table_irr_annual[,7] =  mapply(function(x,y) format_text(x, y), x = irrFlow_pgi_y_stats$IrrFlow_km3_pgi,       y = irrFlow_pgi_y_stats$IrrFlow_mm_pgi_stdev) 
table_irr_annual[,8] =  mapply(function(x,y) format_text(x, y), x = irrFlow_pgi_y_stats$IrrFlow_percent_pgi,   y = irrFlow_pgi_y_stats$IrrFlow_percent_pgi_stdev) 
table_irr_annual[,9] =  mapply(function(x,y) format_text(x, y), x = irrGrwt_pgi_y_stats$irrigationGrwt,        y = irrGrwt_pgi_y_stats$irrigationGrwt_stdev) 
table_irr_annual[,10]=  mapply(function(x,y) format_text(x, y), x = irrGrwt_pgi_y_stats$IrrGrwt_km3_pgi,       y = irrGrwt_pgi_y_stats$IrrGrwt_mm_pgi_stdev) 
table_irr_annual[,11]=  mapply(function(x,y) format_text(x, y), x = irrGrwt_pgi_y_stats$IrrGrwt_percent_pgi,   y = irrGrwt_pgi_y_stats$IrrGrwt_percent_pgi_stdev) 

write.table(table_irr_annual, "results/Tables/Historical_Ice_in_Irr.csv", sep=",", row.names=F)



############################################################################################################
# Table Irr Annual pgs: ERA-Interim historical snow melt, and its contribution to agriculture

mod.hist = "ERA_hist"
irrGross_ps_y_stats  = read.csv(paste("results/Irrigation/irrigationGross/", mod.hist, "_basin_GrossIrr_ps_1980_2009_yearly_stats.csv", sep=""))
irrFlow_ps_y_stats   = read.csv(paste("results/Irrigation/irrigationFlow/",  mod.hist, "_basin_IrrFlow_ps_1980_2009_yearly_stats.csv", sep=""))
irrGrwt_ps_y_stats   = read.csv(paste("results/Irrigation/irrigationGrwt/",  mod.hist, "_basin_IrrGrwt_ps_1980_2009_yearly_stats.csv", sep=""))

# calc mean and stdev for snow melt
snowmelt.y_hist = read.csv(file.path("results/snowMelt", paste(mod.hist, "_basin_snowMelt_km3_yearly.csv", sep="")), sep=",")
snowmelt.y_hist = snowmelt.y_hist[2:ncol(snowmelt.y_hist)]

snowmelt.y_mean  = rowMeans(snowmelt.y_hist)
snowmelt.y_stdev = apply(snowmelt.y_hist, c(1), FUN = sd) 

snowmelt_y_stats = cbind(snowmelt.y_mean, snowmelt.y_stdev)
rownames(snowmelt_y_stats) = rownames(irrGross_ps_y_stats)

write.table(snowmelt_y_stats, "results/snowMelt/ERA_hist_basin_snowMelt_yearly_stats.csv", sep=",")

# Format table:
table_irr_annual = data.frame(matrix(nr = 16, nc = 11))
colnames(table_irr_annual) = c("Basin",
                               "Total Snowmelt", 
                               "Gross Irrigation",         "Snowmelt used in Irrigation",          "Snowmelt in Irrigation (%)",
                               "Surface Water Irrigation", "Snowmelt in Surface Water Irrigation", "Snowmelt in Surface Water Irrigation (%)",
                               "Ground Water Irrigation",  "Snowmelt in Ground Water Irrigation",  "Snowmelt in Ground Water Irrigation (%)")
table_irr_annual[,1] = gsub("_", " ", rownames(irrGross_ps_y_stats))
table_irr_annual[,2] =  mapply(function(x,y) format_text(x, y), x = snowmelt_y_stats[,1],                       y = snowmelt_y_stats[,2]) 
table_irr_annual[,3] =  mapply(function(x,y) format_text(x, y), x = irrGross_ps_y_stats$irrigationGross,      y = irrGross_ps_y_stats$irrigationGross_stdev) 
table_irr_annual[,4] =  mapply(function(x,y) format_text(x, y), x = irrGross_ps_y_stats$GrossIrr_km3_ps,     y = irrGross_ps_y_stats$GrossIrr_mm_ps_stdev) 
table_irr_annual[,5] =  mapply(function(x,y) format_text(x, y), x = irrGross_ps_y_stats$GrossIrr_percent_ps, y = irrGross_ps_y_stats$GrossIrr_percent_ps_stdev) 
table_irr_annual[,6] =  mapply(function(x,y) format_text(x, y), x = irrFlow_ps_y_stats$irrigationFlow,        y = irrFlow_ps_y_stats$irrigationFlow_stdev) 
table_irr_annual[,7] =  mapply(function(x,y) format_text(x, y), x = irrFlow_ps_y_stats$IrrFlow_km3_ps,       y = irrFlow_ps_y_stats$IrrFlow_mm_ps_stdev) 
table_irr_annual[,8] =  mapply(function(x,y) format_text(x, y), x = irrFlow_ps_y_stats$IrrFlow_percent_ps,   y = irrFlow_ps_y_stats$IrrFlow_percent_ps_stdev) 
table_irr_annual[,9] =  mapply(function(x,y) format_text(x, y), x = irrGrwt_ps_y_stats$irrigationGrwt,        y = irrGrwt_ps_y_stats$irrigationGrwt_stdev) 
table_irr_annual[,10]=  mapply(function(x,y) format_text(x, y), x = irrGrwt_ps_y_stats$IrrGrwt_km3_ps,       y = irrGrwt_ps_y_stats$IrrGrwt_mm_ps_stdev) 
table_irr_annual[,11]=  mapply(function(x,y) format_text(x, y), x = irrGrwt_ps_y_stats$IrrGrwt_percent_ps,   y = irrGrwt_ps_y_stats$IrrGrwt_percent_ps_stdev) 

write.table(table_irr_annual, "results/Tables/Historical_snowMelt_in_Irr.csv", sep=",", row.names=F)


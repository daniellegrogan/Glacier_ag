# time_series()
### Figure for Glacier-Ag paper

# project: NASA HiMAT
# Danielle S Grogan

library(rgdal)
library(rgeos)
library(RColorBrewer)
library(ggplot2)
library(tidyr)
library(reshape2)
library(tibble)

#######################################################################################################################################
### Time series plots
#######################################################################################################################################
# 1. plot water inputs for each basin: monthly climatology
# precip, snow melt, glacier melt

water_source_ts = function(mod, rcp, yr.char){
  if(mod == "ERA_hist"){
    m.char = mod
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  
  rainfall = read.csv(paste("results/rainFall/", m.char, "_basin_rainFall_km3_", year.str, "_mc.csv", sep=""))     # total precip
  snowMelt = read.csv(paste("results/snowMelt/", m.char, "_basin_snowMelt_km3_", year.str, "_mc.csv", sep=""))
  iceMelt  = read.csv(paste("results/Glacier_ice_melt/", m.char, "_glacier_melt_basins_", year.str, "_mc.csv", sep=""))
  iceMelt[16,2:25] = colSums(iceMelt[,2:25]) 
  
  basins = rainfall$Basin
  
  # one plot per basin --> one tibble per basin
  for(b in 1:length(basins)){
    b.rain = subset(rainfall[b,])
    b.snow = subset(snowMelt[b,])
    b.gice = subset(iceMelt[b,])
    
    month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    b.data = as.data.frame(matrix(nr=12*3, nc=4))
    b.data[,1] = rep((seq(1,12)),3)
    b.data[,2] = c(rep("Rain", 12), rep("Snowmelt", 12), rep("Glacier ice melt", 12))
    b.data[,3] = c((as.numeric(b.rain)[2:13]), (as.numeric(b.snow)[2:13]),(as.numeric(b.gice)[2:13]))
    b.data[,4] = c((as.numeric(b.rain)[14:25]), (as.numeric(b.snow)[14:25]),(as.numeric(b.gice)[14:25]))
    colnames(b.data) = c("Month", "Water_Source", "Mean", "Stdev")
    b.data = as_tibble(b.data)
    
    b.data.plot = 
      ggplot(b.data, aes(x = Month, y = Mean, group = Water_Source, 
                         color = Water_Source)) +
      geom_ribbon(aes(ymin = b.data$Mean - b.data$Stdev, 
                      ymax = b.data$Mean + b.data$Stdev, 
                      fill = Water_Source), 
                  alpha=0.2, colour = NA) +
      geom_line(aes( color = Water_Source), alpha = 1, size = 1) +  
      labs(fill = "Water Source") +
      labs(colour = "Water Source") +
      labs(x = "Month", 
           y = expression(paste("Water Supply (km"^3~"month"^-1~")")), 
           title = paste(basins[b], m.char, yr.char)) +
      scale_x_continuous(breaks = seq(1, 12, by=1), 
                         expand = c(0,0),  
                         labels=month.names) +
      scale_y_continuous(expand = c(0,0))+
      theme_classic(base_size = 12) 
    
    #b.data.plot
    
    ggsave(filename = paste(m.char, "_", basins[b], "_basin_water_sources_", yr.str, "_mc.png", sep=""),
           plot = b.data.plot,
           device = "png",
           path = "figures/Water_Sources",
           scale = 1, width = 8, height = 4, units = c("in"),
           dpi = 300)
    
  }
}
 

#######################################################################################################################################
# 2. plot irrigation water use by source for each basin: monthly climatology
# precip, snow melt, glacier melt, non-ice melt glacier runoff, unsustainable groundwater

water_use_ts = function(mod, rcp, yr.char){
  if(mod == "ERA_hist"){
    m.char = mod
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  
  Rain     = read.csv(paste("results/GrossIrr_mm_pr/",  m.char, "_basin_GrossIrr_mm_pr_km3_",   yr.char, "_mc.csv", sep=""))     
  snowMelt = read.csv(paste("results/GrossIrr_mm_ps/",  m.char, "_basin_GrossIrr_mm_ps_km3_",   yr.char, "_mc.csv", sep=""))     
  iceMelt  = read.csv(paste("results/GrossIrr_mm_pgi/", m.char, "_basin_GrossIrr_mm_pgi_km3_",  yr.char, "_mc.csv", sep=""))      
  nonMelt  = read.csv(paste("results/GrossIrr_mm_pgn/", m.char, "_basin_GrossIrr_mm_pgn_km3_",  yr.char, "_mc.csv", sep=""))      
  UGW      = read.csv(paste("results/GrossIrr_mm_pu/",  m.char, "_basin_GrossIrr_mm_pu_km3_",   yr.char, "_mc.csv", sep=""))   
  
  basins =  Rain$Basin
  
  # one plot per basin --> one tibble per basin
  for(b in 1:length(basins)){
    b.rain = subset(Rain[b,2:25])
    b.snow = subset(snowMelt[b,2:25])
    b.gice = subset(iceMelt[b,2:25])
    b.nice = subset(nonMelt[b,2:25])
    b.ugw  = subset(UGW[b,2:25])
    
    month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    b.data = as.data.frame(matrix(nr=12*5, nc=6))
    b.data[,1] = rep((seq(1,12)),5)
    b.data[,2] = c(rep("Rain", 12), rep("Snowmelt", 12), 
                   rep("Glacier ice melt", 12), rep("Glacier non-ice runoff", 12),
                   rep("Unsust. groundwater", 12))
    b.data[,3] = c((as.numeric(b.rain)[1:12]), (as.numeric(b.snow)[1:12]), 
                   (as.numeric(b.gice)[1:12]), (as.numeric(b.nice)[1:12]),
                   (as.numeric(b.ugw)[1:12]))
    b.data[,4] = c((as.numeric(b.rain)[13:24]), (as.numeric(b.snow)[13:24]), 
                   (as.numeric(b.gice)[13:24]), (as.numeric(b.nice)[13:24]),
                   (as.numeric(b.ugw)[13:24]))
    r = b.data[,3] - b.data[,4]
    b.data[,5] = unlist(lapply(r, FUN=function(x) max(x,0, na.rm=T)))
    b.data[,6] = b.data[,3] + b.data[,4]
    colnames(b.data) = c("Month", "Water_Source", "Mean", "Stdev", "Min", "Max")
    b.data = as_tibble(b.data)
    
    b.data.plot = 
      ggplot(b.data, aes(x = Month, y = Mean, group = Water_Source, 
                         color = Water_Source)) +
      geom_ribbon(aes(ymin = b.data$Min,
                      ymax = b.data$Max, 
                      fill = Water_Source), 
                  alpha=0.2, colour = NA) +
      scale_fill_brewer(palette="Dark2")+
      geom_line(aes( color = Water_Source), alpha = 1, size = 1) +  
      scale_colour_brewer(palette="Dark2")+
      labs(fill = "Irrigation Water Source") +
      labs(colour = "Irrigation Water Source") +
      labs(x = "Month", 
           y = expression(paste("Irrigation Water (km"^3~"month"^-1~")")), 
           title = paste(basins[b], m.char, yr.char)) +
      scale_x_continuous(breaks = seq(1, 12, by=1), 
                         expand = c(0,0),  
                         labels=month.names) +
      scale_y_continuous(expand = c(0,0))+
      theme_classic(base_size = 12) 
    b.data.plot
    
    ggsave(filename = paste(m.char, basins[b], "basin_irrigation_water_use", yr.char, "mc.png", sep="_"),
           plot = b.data.plot,
           device = "png",
           path = "figures/Water_Use",
           scale = 1, width = 8, height = 4, units = c("in"),
           dpi = 300)
    
  }
}

#######################################################################################################################################
# MAIN #

mod = "ERA_hist"
yr.char = "1980_2009"
rcp="NA"
water_use_ts(mod, rcp, yr.char)
water_source_ts(mod, rcp, yr.char)

mod = "CCSM4"
rcp = "rcp45"
yr.char = "2040_2069"
water_use_ts(mod, rcp, yr.char)
water_source_ts(mod, rcp, yr.char)






#######################################################################################################################################
# SANDBOX
#######################################################################################################################################


years = seq(2006, 2099)
# precip
p.files = list.files("results/precip", pattern = "rcp26", full.names = T)

hist.precip.yr = read.csv("results/precip/ERA_hist_basin_precip_km3_1980_2009_yearly.csv")
precip.yr = p.files[which(grepl("yearly", c(p.files)))]
precip.m = p.files[which(grepl("monthly", c(p.files)))]

p.yr = lapply(precip.yr, FUN=read.csv)

for(b in 1:15){
  plot(seq(1980,2009), as.numeric(hist.precip.yr[b, 3:32]), typ="l", ylim=c(600,1600))
  plot(years,  as.numeric(p.yr[[1]][b,3:96]), typ="l", ylim=c(600,1600))
  lines(years, as.numeric(p.yr[[2]][b,3:96]), col='blue')
  lines(years, as.numeric(p.yr[[3]][b,3:96]), col='red')
  lines(years, as.numeric(p.yr[[4]][b,3:96]), col='darkgreen')
  lines(years, as.numeric(p.yr[[5]][b,3:96]), col='orange')
  lines(years, as.numeric(p.yr[[6]][b,3:96]), col='purple')
}



p.mo = lapply(precip.m, FUN=read.csv)

for(b in 1:15){
  plot(  as.numeric(p.mo[[1]][b,3:1129]), typ="l", ylim=c(0,600))
  lines( as.numeric(p.mo[[2]][b,3:1129]), col='blue')
  lines( as.numeric(p.mo[[3]][b,3:1129]), col='red')
  lines( as.numeric(p.mo[[4]][b,3:1129]), col='darkgreen')
  lines( as.numeric(p.mo[[5]][b,3:1129]), col='orange')
  lines( as.numeric(p.mo[[6]][b,3:1129]), col='purple')
}





years = seq(2006, 2099)
# glacier ice melt
p.files = list.files("results/precip", pattern = "rcp26", full.names = T)

hist.precip.yr = read.csv("results/precip/ERA_hist_basin_precip_km3_1980_2009_yearly.csv")
precip.yr = p.files[which(grepl("yearly", c(p.files)))]
precip.m = p.files[which(grepl("monthly", c(p.files)))]

p.yr = lapply(precip.yr, FUN=read.csv)

for(b in 1:15){
  plot(seq(1980,2009), as.numeric(hist.precip.yr[b, 3:32]), typ="l", ylim=c(600,1600))
  plot(years,  as.numeric(p.yr[[1]][b,3:96]), typ="l", ylim=c(600,1600))
  lines(years, as.numeric(p.yr[[2]][b,3:96]), col='blue')
  lines(years, as.numeric(p.yr[[3]][b,3:96]), col='red')
  lines(years, as.numeric(p.yr[[4]][b,3:96]), col='darkgreen')
  lines(years, as.numeric(p.yr[[5]][b,3:96]), col='orange')
  lines(years, as.numeric(p.yr[[6]][b,3:96]), col='purple')
}



p.mo = lapply(precip.m, FUN=read.csv)

for(b in 1:15){
  plot(  as.numeric(p.mo[[1]][b,3:1129]), typ="l", ylim=c(0,600))
  lines( as.numeric(p.mo[[2]][b,3:1129]), col='blue')
  lines( as.numeric(p.mo[[3]][b,3:1129]), col='red')
  lines( as.numeric(p.mo[[4]][b,3:1129]), col='darkgreen')
  lines( as.numeric(p.mo[[5]][b,3:1129]), col='orange')
  lines( as.numeric(p.mo[[6]][b,3:1129]), col='purple')
}
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

if(mod == "ERA_hist"){
  m.char = mod
}else{
  m.char = paste(mod, rcp, sep="_")
}

year.str = "1980_2009"

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
    labs(x = "Month", y = expression(paste("Water Supply (km"^3~"month"^-1~")")), title = basins[b]) +
    scale_x_continuous(breaks = seq(1, 12, by=1), 
                       expand = c(0,0),  
                       labels=month.names) +
    scale_y_continuous(expand = c(0,0))+
    theme_classic(base_size = 12) 

  #b.data.plot
  
  ggsave(filename = paste(m.char, basins[b], "basin_water_sources.png", sep="_"),
         plot = b.data.plot,
         device = "png",
         path = "figures/Water_Sources",
         scale = 1, width = 8, height = 4, units = c("in"),
         dpi = 300)

}

#######################################################################################################################################
# 2. plot irrigation water use by source for each basin: monthly climatology
# precip, snow melt, glacier melt, non-ice melt glacier runoff, unsustainable groundwater

if(mod == "ERA_hist"){
  m.char = mod
  yr.char = "1980_2016"
}else{
  m.char = paste(mod, rcp, sep="_")
}


Rain     = read.csv(paste("results/Irrigation/irrigationGross/", m.char, "_basin_GrossIrr_pr_",  yr.char, "_monthly_stats.csv", sep=""))     
snowMelt = read.csv(paste("results/Irrigation/irrigationGross/", m.char, "_basin_GrossIrr_ps_",  yr.char, "_monthly_stats.csv", sep=""))     
iceMelt  = read.csv(paste("results/Irrigation/irrigationGross/", m.char, "_basin_GrossIrr_pgi_", yr.char, "_monthly_stats.csv", sep="")) 
nonMelt  = read.csv(paste("results/Irrigation/irrigationGross/", m.char, "_basin_GrossIrr_pgn_", yr.char, "_monthly_stats.csv", sep=""))  
UGW      = read.csv(paste("results/Irrigation/irrigationGross/", m.char, "_basin_GrossIrr_pu_",  yr.char, "_monthly_stats.csv", sep="")) 

Rain     = subset(Rain,     select = c(grepl("GrossIrr_km3_pr", colnames(Rain))))
snowMelt = subset(snowMelt, select = c(grepl("GrossIrr_km3_ps", colnames(snowMelt))))
iceMelt  = subset(iceMelt,  select = c(grepl("GrossIrr_km3_pgi",colnames(iceMelt))))
nonMelt  = subset(nonMelt,  select = c(grepl("GrossIrr_km3_pgn",colnames(nonMelt))))
UGW      = subset(UGW,      select = c(grepl("GrossIrr_km3_pu", colnames(UGW))))

basins =  rainfall$Basin

# one plot per basin --> one tibble per basin
for(b in 1:length(basins)){
  b.rain = subset(Rain[b,])
  b.snow = subset(snowMelt[b,])
  b.gice = subset(iceMelt[b,])
  b.nice = subset(nonMelt[b,])
  b.ugw  = subset(UGW[b,])
  
  month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  b.data = as.data.frame(matrix(nr=12*5, nc=4))
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
  colnames(b.data) = c("Month", "Water_Source", "Mean", "Stdev")
  b.data = as_tibble(b.data)
  
  b.data.plot = 
    ggplot(b.data, aes(x = Month, y = Mean, group = Water_Source, 
                       color = Water_Source)) +
    geom_ribbon(aes(ymin = b.data$Mean - b.data$Stdev, 
                    ymax = b.data$Mean + b.data$Stdev, 
                    fill = Water_Source), 
                alpha=0.2, colour = NA) +
    scale_fill_brewer(palette="Dark2")+
    geom_line(aes( color = Water_Source), alpha = 1, size = 1) +  
    scale_colour_brewer(palette="Dark2")+
    labs(fill = "Irrigation Water Source") +
    labs(colour = "Irrigation Water Source") +
    labs(x = "Month", y = expression(paste("Irrigation Water (km"^3~"month"^-1~")")), title = basins[b]) +
    scale_x_continuous(breaks = seq(1, 12, by=1), 
                       expand = c(0,0),  
                       labels=month.names) +
    scale_y_continuous(expand = c(0,0))+
    theme_classic(base_size = 12) 
  #b.data.plot
  
  ggsave(filename = paste(m.char, basins[b], "basin_irrigation_water_use.png", sep="_"),
         plot = b.data.plot,
         device = "png",
         path = "figures/Water_Use",
         scale = 1, width = 8, height = 4, units = c("in"),
         dpi = 300)
  
}
#######################################################################################################################################

# SANDBOX


#######################################################################################################################################






# plot time series for each basin
plot.dir = "figures/"
mod = "ERA_hist"
icemelt.y_hist = read.csv(file.path("results", mod.hist, paste(mod.hist, "_glacier_melt_basins_y.csv", sep="")), sep="")
basin.names =  as.character(icemelt.y_hist$Basin)

Year = seq(1980, 2016)
for(i in 1:nrow(icemelt.y)){
  
  y = as.numeric(icemelt.y_hist[i,2:ncol(icemelt.y_hist)])
  
  png(paste(plot.dir, "GlacierIceMelt_annual_ts/", mod.hist, "_", mod.fut, "_", rcp, "_glacierIceMelt_ts_", as.character(basin.names[i]), ".png", sep=""))
  plot(Year, y, type='l', ylab = "Melt (km3)", xlim = c(1980, 2099))
  
  lin.fit = summary(lm(as.numeric(icemelt.y_hist[i,2:ncol(icemelt.y_hist)]) ~ Year))
  if(sum(as.numeric(icemelt.y_hist[i,2:ncol(icemelt.y_hist)])) > 0){  # NB: Luni_ext has no melt
    
    if(lin.fit$coefficients[,4][2] < 0.01){
      abline(lin.fit, col='blue', lty=2)
      mtext(paste("Slope =", signif(lin.fit$coefficients[,1][2], 2)), side=3, adj = 1)
    }else{
      abline(lin.fit, col='grey', lty=2)
    }
    
  }
  
  dev.off()
}


#######################################################################################################################################

icemelt.y_hist = read.csv(file.path("results", mod.hist, paste(mod.hist, "_glacier_melt_basins_y.csv", sep="")), sep="")
icemelt.y_fut  = read.csv(file.path("results", mod.fut, rcp, paste(mod.fut, "_", rcp, "_glacier_melt_basins_y.csv", sep="")))

Year.h = seq(1980, 2016)
Year.f = seq(2000, 2099)

for(i in 1:nrow(icemelt.y_hist)){
  
  y.hist = as.numeric(icemelt.y_hist[i,2:ncol(icemelt.y_hist)])
  y.fut  = as.numeric(icemelt.y_fut[i,2:ncol(icemelt.y_fut)])
  
  png(paste(plot.dir, "GlacierIceMelt_annual_ts/glacierIceMelt_ts_", as.character(basin.names[i]), ".png", sep=""))
  plot(Year.h,  y.hist, type='l', ylab = "Melt (km3)", xlab = "Year",
       xlim = c(1980, 2099),
       ylim = c(min(c(y.hist, y.fut)), max(c(y.hist, y.fut))) )
  lines(Year.f, y.fut, col = 'darkorange')
  legend("topright", 
         lty = c(1,1), 
         col=c('black', 'darkorange'), 
         legend = c("Historical", "CanESM2 RCP 8.5"), 
         bty = 'n')
  dev.off()
}

#######################################################################################################################################
# yearly irrigation gross

irrGross.y_hist = read.csv(file.path("results", mod.hist, paste(mod.hist, "_basin_irrigationGross_1980_2016_yearly.csv", sep="")), sep=",")
irrGross.y_fut  = read.csv(file.path("results", mod.fut, rcp, paste(mod.fut, "_basin_irrigationGross_2006_2099_yearly.csv", sep="")), sep=",")

Year.h = seq(1980, 2016)
Year.f = seq(2006, 2099)

for(i in 1:nrow(icemelt.y_hist)){
  
  y.hist = as.numeric(irrGross.y_hist[i,2:ncol(irrGross.y_hist)])
  y.fut  = as.numeric(irrGross.y_fut[i,2:ncol(irrGross.y_fut)])
  
  png(paste(plot.dir, "GrossIrrigation_annual_ts/grossIrrigation_annual_ts_", as.character(basin.names[i]), ".png", sep=""))
  plot(Year.h,  y.hist, type='l', ylab = "Gross Irrigation (km3)", xlab = "Year",
       xlim = c(1980, 2099),
       ylim = c(min(c(y.hist, y.fut)), max(c(y.hist, y.fut))) )
  lines(Year.f, y.fut, col = 'darkorange')
  legend("topleft", 
         lty = c(1,1), 
         col=c('black', 'darkorange'), 
         legend = c("Historical", "CanESM2 RCP 8.5"), 
         bty = 'n')
  dev.off()
}

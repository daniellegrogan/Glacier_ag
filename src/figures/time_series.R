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
library(gridExtra)

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
# MC BAR CHARTS
mod ="ERA_hist"
if(mod == "ERA_hist"){
  m.char = mod
  yr.char = "1980_2009"
}else{
  m.char = paste(mod, rcp, sep="_")
}

# ERA hist
Rain      = read.csv(paste("results/GrossIrr_mm_pr/",   m.char, "_basin_GrossIrr_mm_pr_km3_",    yr.char, "_mc.csv", sep=""))     
snowMelt  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_mc.csv", sep=""))     
iceMelt   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_mc.csv", sep=""))      
nonMelt   = read.csv(paste("results/GrossIrr_mm_pgn/",  m.char, "_basin_GrossIrr_mm_pgn_km3_",   yr.char, "_mc.csv", sep=""))      
UGW       = read.csv(paste("results/GrossIrr_mm_pu/",   m.char, "_basin_GrossIrr_mm_pu_km3_",    yr.char, "_mc.csv", sep=""))   
irrDemand = read.csv(paste("results/irrigationGross/",  m.char, "_basin_irrigationGross_km3_",   yr.char, "_mc.csv", sep=""))

basins =  Rain$Basin

b=2
for(b in 1:15){
  b.rain = subset(Rain[b,2:25])
  b.snow = subset(snowMelt[b,2:25])
  b.gice = subset(iceMelt[b,2:25])
  b.nice = subset(nonMelt[b,2:25])
  b.ugw  = subset(UGW[b,2:25])
 # b.dem  = subset(irrDemand[b,2:25])
  
  ### testing
  # create a dataset
  month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  Month = unlist(lapply(month.names, FUN=function(x) rep(x,5)))
  Source = rep(c("Rain", "Snow Melt", "Glacier Ice Melt", "Glacier Non-ice Runoff", "Unsustainable") , 12)
  value  = mat.or.vec(nr=12*5, nc=1) 
  for(m in 1:12){
    value[((m-1)*5+1):(m*5)] = c(b.rain[m], b.snow[m], b.gice[m], b.nice[m], -b.ugw[m])
  }
  source.data = as.data.frame(cbind(Month, Source, unlist(value)))
  source.data$Month <- factor(source.data$Month, levels = month.names)
  #colnames(source.data)[3] = "Value"
  
  # Stacked
  bp = ggplot() + 
    geom_bar(data = source.data, position="stack", stat="identity", aes(fill=Source, y=value, x=Month)) +
    scale_fill_brewer(palette="Set2") +
    labs(y = expression(paste("Irrigation Water Source (km"^3~"month"^-1~")")), 
         title = paste(basins[b], m.char, yr.char)) +
    theme_classic() 
  
  ggsave(filename = paste(m.char, basins[b], "basin_irrigation_water_use", yr.char, "mc_BarChart.png", sep="_"),
         plot = bp,
         device = "png",
         path = "figures/Water_Use",
         scale = 1, width = 8, height = 4, units = c("in"),
         dpi = 300)
  
}
#######################################################################################################################################
# Annual time series through entire 21st century

cols = c('#e6194b', '#3cb44b', '#4363d8', '#f58231', '#911eb4', 
         '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', 
         '#e6beff', '#9a6324', "#A9A9A9", '#800000', '#000075',
         '#808080','#aaffc3', '#808000', '#ffd8b1')



### Glacier Ice Melt
icemelt.files = dir("results/Glacier_ice_melt/", full.names = T, pattern ="yearly")

all.yrs = seq(1980, 2099)
hist = read.csv(icemelt.files[which(grepl("ERA", c(icemelt.files)))])
basins = hist$Basin

rcps = c("rcp26", "rcp45", "rcp60", "rcp85")

for(rcp in rcps){
  rcp.files = which(grepl(rcp, c(icemelt.files)))
  mod.nm.1 = sub("results/Glacier_ice_melt//", "", icemelt.files[rcp.files])
  mod.nm.2 = unlist(lapply(strsplit(mod.nm.1, "_"), FUN = function(x) unlist(x)[1]))
  
  for(b in 1:15){
    hist.basin = append(hist[b,2:ncol(hist)], rep(NA, (length(all.yrs) - (ncol(hist)-1))))
    names(hist.basin) = c(all.yrs)
    
   # png(paste("figures/GlacierIceMelt_annual_ts/", basins[b], "_Glacier_Ice_Melt_", rcp, "_yearly.png", sep=""), 
   #     res=300, height=6, width=12, unit="in")
   # par(mar=c(5.1, 4.5, 4.1, 2.1))
   # plot(all.yrs, as.numeric(hist.basin), type='l', ylim=c(0, 2*max(as.numeric(hist.basin), na.rm=T)), 
   #      main = paste(basins[b], "Annual Glacier Ice Melt", rcp),
    #     xlab = "Year", ylab = expression(paste("Glacier Ice Melt (km"^3~"year"^-1~")")))
    for(f in 1:length(rcp.files)){
      plot(all.yrs, as.numeric(hist.basin), type='l', ylim=c(0, 2*max(as.numeric(hist.basin), na.rm=T)), 
           main = paste(basins[b], icemelt.files[rcp.files[f]]),
           xlab = "Year", ylab = expression(paste("Glacier Ice Melt (km"^3~"year"^-1~")")))
      
      fut = read.csv(icemelt.files[rcp.files[f]])
      fut = append(rep(NA, 20), fut[b,2:ncol(fut)])
      names(fut) = c(all.yrs)
      lines(all.yrs, as.numeric(fut), col=cols[f])
    }
   # dev.off()
    legend("topright", lty=rep(1,19), col=cols, legend=mod.nm.2)
  }
  
}


### Snowmelt
snowmelt.files = dir("results/snowMelt/", full.names = T, pattern ="yearly")

all.yrs = seq(1980, 2099)
hist = read.csv("results/snowMelt/ERA_hist_basin_snowMelt_km3_1980_2009_yearly.csv")
basins = hist$Basin

rcps = c("rcp26", "rcp45", "rcp60", "rcp85")

for(rcp in rcps){
  rcp.files = which(grepl(rcp, c(snowmelt.files)))
  mod.nm.1 = sub("results/snowMelt//", "", snowmelt.files[rcp.files])
  mod.nm.2 = unlist(lapply(strsplit(mod.nm.1, "_"), FUN = function(x) unlist(x)[1]))
  
  for(b in 1:15){
    hist.basin = append(hist[b,3:ncol(hist)], rep(NA, (length(all.yrs) - (ncol(hist)-2))))
    names(hist.basin) = c(all.yrs)
    
    png(paste("figures/snowMelt_annual_ts/", basins[b], "_snowMelt_", rcp, "_yearly.png", sep=""), 
        res=300, height=6, width=12, unit="in")
    par(mar=c(5.1, 4.5, 4.1, 2.1))
    plot(all.yrs, as.numeric(hist.basin), type='l', ylim=c(0, 2*max(as.numeric(hist.basin), na.rm=T)), 
         main = paste(basins[b], "Annual Snow Melt", rcp),
         xlab = "Year", ylab = expression(paste("Snow Melt (km"^3~"year"^-1~")")))
    for(f in 1:length(rcp.files)){
      fut = read.csv(snowmelt.files[rcp.files[f]])
      fut = subset(fut, select=c(grepl("2", colnames(fut))))
      fut = append(rep(NA, 26), fut[b,1:ncol(fut)])
      names(fut) = c(all.yrs)
      lines(all.yrs, as.numeric(fut), col=cols[f])
    }
    dev.off()
    #legend("topright", lty=rep(1,19), col=cols, legend=mod.nm.2)
  }
  print(rcp)
}



### Precip
precip.files = dir("results/precip/", full.names = T, pattern ="yearly")

all.yrs = seq(1980, 2099)
hist = read.csv(precip.files[which(grepl("ERA", c(precip.files)))])
basins = hist$Basin

rcps = c("rcp26", "rcp45", "rcp60", "rcp85")

for(rcp in rcps){
  rcp.files = which(grepl(rcp, c(precip.files)))
  mod.nm.1 = sub("results/precip//", "", precip.files[rcp.files])
  mod.nm.2 = unlist(lapply(strsplit(mod.nm.1, "_"), FUN = function(x) unlist(x)[1]))
  
  for(b in 1:15){
    hist.basin = append(hist[b,3:ncol(hist)], rep(NA, (length(all.yrs) - (ncol(hist)-2))))
    names(hist.basin) = c(all.yrs)
    
    png(paste("figures/precip_annual_ts/", basins[b], "_Precip_", rcp, "_yearly.png", sep=""), 
        res=300, height=6, width=12, unit="in")
    par(mar=c(5.1, 4.5, 4.1, 2.1))
    plot(all.yrs, as.numeric(hist.basin), type='l', ylim=c(0, 2*max(as.numeric(hist.basin), na.rm=T)), 
         main = paste(basins[b], "Annual Precipitation", rcp),
         xlab = "Year", ylab = expression(paste("Precipitation (km"^3~"year"^-1~")")))
    for(f in 1:length(rcp.files)){
      fut = read.csv(precip.files[rcp.files[f]])
      fut = subset(fut, select=c(grepl("X", colnames(fut))))
      fut = append(rep(NA, 26), fut[b,2:ncol(fut)])
      names(fut) = c(all.yrs)
      lines(all.yrs, as.numeric(fut), col=cols[f])
    }
    dev.off()
    #legend("topright", lty=rep(1,19), col=cols, legend=mod.nm.2)
  }
  print(rcp)
}
#######################################################################################################################################
# bar plot: 
# facet each month, one bar each for historical, mid century, late century

# historical
mod ="ERA_hist"
if(mod == "ERA_hist"){
  m.char = mod
  yr.char = "1980_2009"
}else{
  m.char = paste(mod, rcp, sep="_")
}
Rain.hist      = read.csv(paste("results/GrossIrr_mm_pr/",   m.char, "_basin_GrossIrr_mm_pr_km3_",    yr.char, "_mc.csv", sep=""))     
snowMelt.hist  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_mc.csv", sep=""))     
iceMelt.hist   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_mc.csv", sep=""))      
nonMelt.hist   = read.csv(paste("results/GrossIrr_mm_pgn/",  m.char, "_basin_GrossIrr_mm_pgn_km3_",   yr.char, "_mc.csv", sep=""))      
UGW.hist       = read.csv(paste("results/GrossIrr_mm_pu/",   m.char, "_basin_GrossIrr_mm_pu_km3_",    yr.char, "_mc.csv", sep=""))   

# mid century
mods = c("CCSM4", "CanESM2", "NorESM1-M", "CESM1-CAM5", "CNRM-CM5")
for(mod in mods){
  rcp = "rcp85"
  if(mod == "ERA_hist"){
    m.char = mod
    yr.char = "1980_2009"
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  yr.char = "2040_2069"
  Rain.mid      = read.csv(paste("results/GrossIrr_mm_pr/",   m.char, "_basin_GrossIrr_mm_pr_km3_",    yr.char, "_mc.csv", sep=""))     
  snowMelt.mid  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_mc.csv", sep=""))     
  iceMelt.mid   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_mc.csv", sep=""))      
  nonMelt.mid   = read.csv(paste("results/GrossIrr_mm_pgn/",  m.char, "_basin_GrossIrr_mm_pgn_km3_",   yr.char, "_mc.csv", sep=""))      
  UGW.mid       = read.csv(paste("results/GrossIrr_mm_pu/",   m.char, "_basin_GrossIrr_mm_pu_km3_",    yr.char, "_mc.csv", sep=""))   
  
  # Late century
  if(mod == "ERA_hist"){
    m.char = mod
    yr.char = "1980_2009"
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  yr.char = "2070_2099"
  Rain.late      = read.csv(paste("results/GrossIrr_mm_pr/",   m.char, "_basin_GrossIrr_mm_pr_km3_",    yr.char, "_mc.csv", sep=""))     
  snowMelt.late  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_mc.csv", sep=""))     
  iceMelt.late   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_mc.csv", sep=""))      
  nonMelt.late   = read.csv(paste("results/GrossIrr_mm_pgn/",  m.char, "_basin_GrossIrr_mm_pgn_km3_",   yr.char, "_mc.csv", sep=""))      
  UGW.late       = read.csv(paste("results/GrossIrr_mm_pu/",   m.char, "_basin_GrossIrr_mm_pu_km3_",    yr.char, "_mc.csv", sep=""))   
  test = (Rain.mid == Rain.late)
  
  for(b in 1:15){
    # create a dataset
    month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    Month = unlist(lapply(month.names, FUN=function(x) rep(x,5*3)))
    #period.names = c(rep("Historical",5), rep("Mid Century",5), rep("Late Century",5))
    period.names = c(rep("H",5), rep("M",5), rep("L",5))
    Period = rep(period.names, 12)
    Source = rep(c("Rain", "Snow Melt", "Glacier Ice Melt", "Glacier Non-ice Runoff", "Unsustainable"), 12*3)
    value  = mat.or.vec(nr=12*5*3, nc=1) 
    for(m in 1:12){
      st.row = (m-1)*15 + 1
      value[st.row:(st.row+4)]       = c(Rain.hist[b,m+1], snowMelt.hist[b,m+1], iceMelt.hist[b,m+1], nonMelt.hist[b,m+1], -UGW.hist[b,m+1])
      value[(st.row+5):(st.row+9)]   = c(Rain.mid[b,m+1], snowMelt.mid[b,m+1], iceMelt.mid[b,m+1], nonMelt.mid[b,m+1], -UGW.mid[b,m+1])
      value[(st.row+10):(st.row+14)] = c(Rain.late[b,m+1], snowMelt.late[b,m+1], iceMelt.late[b,m+1], nonMelt.late[b,m+1], -UGW.late[b,m+1])
    }
    source.data = as.data.frame(cbind(Month, Source, Period, unlist(value)))
    source.data$Month <- factor(source.data$Month, levels = month.names)
    source.data$Period <- factor(source.data$Period, levels = c("H", "M", "L"))
    
    bp = ggplot() + 
      geom_bar(data = source.data, position="stack", stat="identity", aes(fill=Source, y=value, x=Period)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette="Set2") +
      facet_grid(~Month, switch='x') +
      labs(y = expression(paste("Irrigation Water Source (km"^3~"month"^-1~")")), 
           title = paste(basins[b], m.char)) +
      theme_classic() 
    ggsave(filename = paste(m.char, basins[b], "basin_irrigation_water_use", yr.char, "mc_BarChart_Climatologies.png", sep="_"),
           plot = bp,
           device = "png",
           path = "figures/Water_Use",
           scale = 1, width = 12, height = 4, units = c("in"),
           dpi = 300)
  }
  print(mod)
}

#######################################################################################################################################
# barplot: water supply vs use



# historical
mod ="ERA_hist"
if(mod == "ERA_hist"){
  m.char = mod
  yr.char = "1980_2009"
}else{
  m.char = paste(mod, rcp, sep="_")
}
snowMelt.use.hist  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_mc.csv", sep=""))     
iceMelt.use.hist   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_mc.csv", sep=""))      

snowMelt.supply.hist  = read.csv(paste("results/snowMelt/",   m.char, "_basin_snowMelt_km3_",    yr.char, "_mc.csv", sep=""))     
iceMelt.supply.hist   = read.csv(paste("results/Glacier_ice_melt/",  m.char, "_glacier_melt_basins_",   yr.char, "_mc.csv", sep=""))      

# mid century
mods = c("CCSM4", "CanESM2", "NorESM1-M", "CESM1-CAM5", "CNRM-CM5")

for(mod in mods){
  rcp = "rcp85"
  if(mod == "ERA_hist"){
    m.char = mod
    yr.char = "1980_2009"
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  yr.char = "2040_2069"
  snowMelt.use.mid  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_mc.csv", sep=""))     
  iceMelt.use.mid   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_mc.csv", sep=""))      

  snowMelt.supply.mid  = read.csv(paste("results/snowMelt/",   m.char, "_basin_snowMelt_km3_",    yr.char, "_mc.csv", sep=""))     
  iceMelt.supply.mid   = read.csv(paste("results/Glacier_ice_melt/",  m.char, "_glacier_melt_basins_",   yr.char, "_mc.csv", sep=""))      
  
  # Late century
  if(mod == "ERA_hist"){
    m.char = mod
    yr.char = "1980_2009"
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  yr.char = "2070_2099"
  snowMelt.use.late  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_mc.csv", sep=""))     
  iceMelt.use.late   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_mc.csv", sep=""))      
  
  snowMelt.supply.late  = read.csv(paste("results/snowMelt/",   m.char, "_basin_snowMelt_km3_",    yr.char, "_mc.csv", sep=""))     
  iceMelt.supply.late   = read.csv(paste("results/Glacier_ice_melt/",  m.char, "_glacier_melt_basins_",   yr.char, "_mc.csv", sep=""))      
  
  
  for(b in 1:15){
    # create a dataset
    month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    Month = unlist(lapply(month.names, FUN=function(x) rep(x,3*2)))
    period.names = c(rep("H",2), rep("M",2), rep("L",2))
    Period = rep(period.names, 12)
    Metric = rep(c("Supply", "Irr_Use"), 12*3)
    value  = mat.or.vec(nr=12*2*3, nc=1) 
    for(m in 1:12){
      st.row = (m-1)*6 + 1
      value[st.row:(st.row+1)]       = c(iceMelt.supply.hist[b,m+1], iceMelt.use.hist[b,m+1])
      value[(st.row+1):(st.row+2)]   = c(iceMelt.supply.mid[b,m+1], iceMelt.use.mid[b,m+1])
      value[(st.row+3):(st.row+4)]   = c(iceMelt.supply.late[b,m+1], iceMelt.use.late[b,m+1])
    }
    source.data = as.data.frame(cbind(Month, Metric, Period, unlist(value)))
    source.data$Month  <- factor(source.data$Month, levels = month.names)
    source.data$Period <- factor(source.data$Period, levels = c("H", "M", "L"))
    source.data$Metric <- factor(source.data$Metric, levels = c("Supply", "Irr_Use"))
    
    bp = ggplot() + 
      geom_bar(data = source.data, position=position_dodge(), stat="identity", aes(fill=Metric, y=value, x=Period)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette="Paired") +
      facet_grid(~Month, switch='x') +
      labs(y = expression(paste("Glacier Ice Melt Supply and Use (km"^3~"month"^-1~")")), 
           title = paste(basins[b], m.char)) +
      theme_classic() 
    ggsave(filename = paste(m.char, basins[b], "basin_irrigation_Glacier_ice_melt_supply_and_use_mc.png", sep="_"),
           plot = bp,
           device = "png",
           path = "figures/Water_supply_and_use",
           scale = 1, width = 12, height = 4, units = c("in"),
           dpi = 300)
  }
  
  
  for(b in 1:15){
    # create a dataset
    month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    Month = unlist(lapply(month.names, FUN=function(x) rep(x,3*2)))
    period.names = c(rep("H",2), rep("M",2), rep("L",2))
    Period = rep(period.names, 12)
    Metric = rep(c("Supply", "Irr_Use"), 12*3)
    value  = mat.or.vec(nr=12*2*3, nc=1) 
    for(m in 1:12){
      st.row = (m-1)*6 + 1
      value[st.row:(st.row+1)]       = c(snowMelt.supply.hist[b,m+1], snowMelt.use.hist[b,m+1])
      value[(st.row+1):(st.row+2)]   = c(snowMelt.supply.mid[b,m+1], snowMelt.use.mid[b,m+1])
      value[(st.row+3):(st.row+4)]   = c(snowMelt.supply.late[b,m+1], snowMelt.use.late[b,m+1])
    }
    source.data = as.data.frame(cbind(Month, Metric, Period, unlist(value)))
    source.data$Month  <- factor(source.data$Month, levels = month.names)
    source.data$Period <- factor(source.data$Period, levels = c("H", "M", "L"))
    source.data$Metric <- factor(source.data$Metric, levels = c("Supply", "Irr_Use"))
    
    bp = ggplot() + 
      geom_bar(data = source.data, position=position_dodge(), stat="identity", aes(fill=Metric, y=value, x=Period)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette="Paired") +
      facet_grid(~Month, switch='x') +
      labs(y = expression(paste("Snowmelt Supply and Use (km"^3~"month"^-1~")")), 
           title = paste(basins[b], m.char)) +
      theme_classic() 
    
    ggsave(filename = paste(m.char, basins[b], "basin_irrigation_Snowmelt_supply_and_use_mc.png", sep="_"),
           plot = bp,
           device = "png",
           path = "figures/Water_supply_and_use",
           scale = 1, width = 12, height = 4, units = c("in"),
           dpi = 300)
  }
 print(mod)
}




### ANNUAL
# historical
mod ="ERA_hist"
if(mod == "ERA_hist"){
  m.char = mod
  yr.char = "1980_2009"
}else{
  m.char = paste(mod, rcp, sep="_")
}
snowMelt.use.hist  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_yc.csv", sep=""))     
iceMelt.use.hist   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_yc.csv", sep=""))      

snowMelt.supply.hist  = read.csv(paste("results/snowMelt/",   m.char, "_basin_snowMelt_km3_",    yr.char, "_yc.csv", sep=""))     
iceMelt.supply.hist   = read.csv(paste("results/Glacier_ice_melt/",  m.char, "_glacier_melt_basins_",   yr.char, "_yc.csv", sep=""))      

# mid century
mods = c("CCSM4", "CanESM2", "NorESM1-M", "CESM1-CAM5", "CNRM-CM5")

for(mod in mods){
  rcp = "rcp85"
  if(mod == "ERA_hist"){
    m.char = mod
    yr.char = "1980_2009"
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  yr.char = "2040_2069"
  snowMelt.use.mid  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_yc.csv", sep=""))     
  iceMelt.use.mid   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_yc.csv", sep=""))      
  
  snowMelt.supply.mid  = read.csv(paste("results/snowMelt/",   m.char, "_basin_snowMelt_km3_",    yr.char, "_yc.csv", sep=""))     
  iceMelt.supply.mid   = read.csv(paste("results/Glacier_ice_melt/",  m.char, "_glacier_melt_basins_",   yr.char, "_yc.csv", sep=""))      
  
  # Late century
  if(mod == "ERA_hist"){
    m.char = mod
    yr.char = "1980_2009"
  }else{
    m.char = paste(mod, rcp, sep="_")
  }
  yr.char = "2070_2099"
  snowMelt.use.late  = read.csv(paste("results/GrossIrr_mm_ps/",   m.char, "_basin_GrossIrr_mm_ps_km3_",    yr.char, "_yc.csv", sep=""))     
  iceMelt.use.late   = read.csv(paste("results/GrossIrr_mm_pgi/",  m.char, "_basin_GrossIrr_mm_pgi_km3_",   yr.char, "_yc.csv", sep=""))      
  
  snowMelt.supply.late  = read.csv(paste("results/snowMelt/",   m.char, "_basin_snowMelt_km3_",    yr.char, "_yc.csv", sep=""))     
  iceMelt.supply.late   = read.csv(paste("results/Glacier_ice_melt/",  m.char, "_glacier_melt_basins_",   yr.char, "_yc.csv", sep=""))      
  
  
  for(b in 1:15){
    # create a dataset
    period.names = c(rep("H",2), rep("M",2), rep("L",2))
    Period = rep(period.names, 1)
    Metric = rep(c("Supply", "Irr_Use"), 3)
    value  = mat.or.vec(nr=2*3, nc=1) 

    value[1:2]   = c(iceMelt.supply.hist$Mean[b], iceMelt.use.hist$Mean[b])
    value[3:4]   = c(iceMelt.supply.mid$Mean[b], iceMelt.use.mid$Mean[b])
    value[5:6]   = c(iceMelt.supply.late$Mean[b], iceMelt.use.late$Mean[b])

    source.data = as.data.frame(cbind(Metric, Period, unlist(value)))
    source.data$Period <- factor(source.data$Period, levels = c("H", "M", "L"))
    source.data$Metric <- factor(source.data$Metric, levels = c("Supply", "Irr_Use"))
    #colnames(source.data)[3] = "Value"
    bp = ggplot() + 
      geom_bar(data = source.data, position=position_dodge(), 
               stat="identity", aes(fill=Metric, y=value, x=Period)) +
      scale_fill_brewer(palette="Paired") +
      labs(y = expression(paste("Glacier Ice Melt Supply and Use (km"^3~"year"^-1~")")), 
           title = paste(basins[b], m.char)) +
      theme_classic() 
    
    ggsave(filename = paste(m.char, basins[b], "basin_irrigation_Glacier_ice_melt_supply_and_use_yc.png", sep="_"),
           plot = bp,
           device = "png",
           path = "figures/Water_supply_and_use",
           scale = 1, width = 6, height = 4, units = c("in"),
           dpi = 300)
  }
  
  
  for(b in 1:15){
    # create a dataset
    # create a dataset
    period.names = c(rep("H",2), rep("M",2), rep("L",2))
    Period = rep(period.names, 1)
    Metric = rep(c("Supply", "Irr_Use"), 3)
    value  = mat.or.vec(nr=2*3, nc=1) 
    
    value[1:2]   = c(snowMelt.supply.hist$Mean[b], snowMelt.use.hist$Mean[b])
    value[3:4]   = c(snowMelt.supply.mid$Mean[b], snowMelt.use.mid$Mean[b])
    value[5:6]   = c(snowMelt.supply.late$Mean[b], snowMelt.use.late$Mean[b])
    
    source.data = as.data.frame(cbind(Metric, Period, unlist(value)))
    source.data$Period <- factor(source.data$Period, levels = c("H", "M", "L"))
    source.data$Metric <- factor(source.data$Metric, levels = c("Supply", "Irr_Use"))
    
    bp = ggplot() + 
      geom_bar(data = source.data, position=position_dodge(), stat="identity", aes(fill=Metric, y=value, x=Period)) +
      scale_fill_brewer(palette="Paired") +
      labs(y = expression(paste("Snowmelt Supply and Use (km"^3~"year"^-1~")")), 
           title = paste(basins[b], m.char)) +
      theme_classic() 
    ggsave(filename = paste(m.char, basins[b], "basin_irrigation_Snowmelt_supply_and_use_yc.png", sep="_"),
           plot = bp,
           device = "png",
           path = "figures/Water_supply_and_use",
           scale = 1, width = 6, height = 4, units = c("in"),
           dpi = 300)
  }
  print(mod)
}











#######################################################################################################################################
# SANDBOX
#######################################################################################################################################


years = seq(2006, 2099)
# precip
p.files = list.files("results/precip", full.names = T)

hist.precip.yr = read.csv("results/precip/ERA_hist_basin_precip_km3_1980_2009_yearly.csv")
hist.precip.yc = read.csv("results/precip/ERA_hist_basin_precip_km3_1980_2009_yc.csv")
precip.yr = p.files[which(grepl("yearly", c(p.files)))]
precip.yr = precip.yr[which(grepl("rcp", c(precip.yr)))]
precip.m = p.files[which(grepl("monthly", c(p.files)))]
precip.m = precip.m[which(grepl("rcp", c(precip.m)))]


p.yr = lapply(precip.yr, FUN=read.csv)
cols = c('#e6194b', '#3cb44b', '#4363d8', '#f58231', '#911eb4', 
         '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', 
         '#e6beff', '#9a6324', "#A9A9A9", '#800000', '#000075',
         '#808080','#aaffc3', '#808000', '#ffd8b1')
years=seq(2006,2099)
for(b in 1:15){
  plot(years,  as.numeric(p.yr[[1]][b,3:96]), typ="l", ylim=c(600,1600))
  for(p in 1:10){
    lines(years, as.numeric(p.yr[[p]][b,3:96]), col=cols[p])
  }
}

for(p in 1:11){
  py = read.csv(precip.yr[p])
  plot(as.numeric(py[5,3:95]), typ="l", main = precip.yr[p])
}

p = read.csv(precip.yr[6])
plot(as.numeric(p[1,2:ncol(p)]),type='l')
#abline(h=hist.precip.yc$Mean[1], col='blue', lwd=2)
abline(h=(hist.precip.yc$Mean[1]-hist.precip.yc$Stdev[1]), col='blue', lwd=1)
abline(h=(hist.precip.yc$Mean[1]+hist.precip.yc$Stdev[1]), col='blue', lwd=1)




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


# test out spatial aggregation

m = 'MIROC5'
r = "rcp26"
path.base = file.path("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12", m, r)
var='precip'
monthly.agg = lapply(vars, function(var) extract_ts(raster.path = file.path(path.base, "monthly"), 
                                                    shp = basins, 
                                                    years = seq(2020,2040), 
                                                    var, 
                                                    row.nm = as.character(basins$name),
                                                    out.nm = paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")))

test.agg = extract_ts(raster.path = file.path(path.base, "monthly"), 
                      shp = basins, 
                      years = seq(2020,2040), 
                      var, 
                      row.nm = as.character(basins$name),
                      out.nm = paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep=""))

# sum monthly aggregates to yearly
yearly.agg = lapply(vars, function(var) monthly_to_yearly(data.m = read.csv(paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_monthly.csv", sep="")),
                                                          out.nm =          paste(path.out, "/", var, "/", m, "_", r, "_basin_", var, "_km3_",  min(years), "_", max(years), "_yearly.csv", sep="")))

test.y = monthly_to_yearly(data.m)

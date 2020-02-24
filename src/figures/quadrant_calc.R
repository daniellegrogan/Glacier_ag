# quadrant_calc()

library(rgdal)
library(rgeos)
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory

# identify which "quadrant" of a delta ice melt vs delta precip plot each model/rcp/climatology lands in

hist.precip = read.csv("results/precip/ERA_hist_basin_precip_km3_1980_2009_yc.csv")
hist.precip = hist.precip[1:15,]
hist.irr    = read.csv("results/irrigationGross/ERA_hist_basin_irrigationGross_km3_1980_2009_yc.csv")
hist.irr = hist.irr[1:15,]
hist.icemlt = read.csv("results/Glacier_ice_melt/ERA_hist_glacier_melt_basins_1980_2009_yc.csv")

md.yr.str = "2040_2069"
lt.yr.str = "2070_2099"
mods = c("CCSM4", "MIROC5")
rcps = c("rcp45", "rcp85")

diff.precip = data.frame(matrix(nr=nrow(hist.icemlt), nc=length(mods)*length(rcps)*2))
diff.icemlt = data.frame(matrix(nr=nrow(hist.icemlt), nc=length(mods)*length(rcps)*2))
diff.irr    = data.frame(matrix(nr=nrow(hist.icemlt), nc=length(mods)*length(rcps)*2))

m.col = unlist(lapply(mods, FUN = function(x) rep(x, length(rcps)*2)))
r.col = rep(unlist(lapply(rcps, FUN = function(x) rep(x, length(mods)))),2)
t.col = rep(c(rep(c(md.yr.str, lt.yr.str), length(mods))), length(rcps))
colnames(diff.precip) = colnames(diff.icemlt) = paste(m.col, r.col, t.col, sep="_")

for(mod in mods){
  for(rcp in rcps){
    # 2040-2069 = md
    md.precip = read.csv(paste("results/precip/", mod, "_", rcp, "_basin_precip_km3_", md.yr.str,  "_yc.csv", sep=""))
    md.precip = md.precip[1:15,]
    md.irr    = read.csv(paste("results/irrigationGross/", mod, "_", rcp, "_basin_irrigationGross_km3_", md.yr.str,  "_yc.csv", sep=""))
    md.irr = md.irr[1:15,]
    md.icemlt = read.csv(paste("results/Glacier_ice_melt/", mod, "_", rcp, "_glacier_melt_basins_", md.yr.str, "_yc.csv", sep=""))
    
    # 2070-2099 = lt
    lt.precip = read.csv(paste("results/precip/", mod, "_", rcp, "_basin_precip_km3_", lt.yr.str,  "_yc.csv", sep=""))
    lt.precip = lt.precip[1:15,]
    lt.irr    = read.csv(paste("results/irrigationGross/", mod, "_", rcp, "_basin_irrigationGross_km3_", lt.yr.str,  "_yc.csv", sep=""))
    lt.irr    = lt.irr[1:15,]
    lt.icemlt = read.csv(paste("results/Glacier_ice_melt/", mod, "_", rcp, "_glacier_melt_basins_", lt.yr.str, "_yc.csv", sep=""))
    
    ### Compare to historical
    # 2040-2069 = md
    md.p.diff = md.precip$Mean - hist.precip$Mean
    md.g.diff = md.icemlt$Mean - hist.icemlt$Mean
    md.i.diff = md.irr$Mean - hist.irr$Mean
    
    # 2070-2099 = lt
    lt.p.diff = lt.precip$Mean - hist.precip$Mean
    lt.g.diff = lt.icemlt$Mean - hist.icemlt$Mean
    lt.i.diff = lt.irr$Mean - hist.irr$Mean
    
    col.match = which(grepl(mod, colnames(diff.precip)) & grepl(rcp, colnames(diff.precip)))
    diff.precip[,col.match[1]] = md.p.diff
    diff.precip[,col.match[2]] = lt.p.diff
    
    diff.icemlt[,col.match[1]] = md.g.diff
    diff.icemlt[,col.match[2]] = lt.g.diff
    
    diff.irr[,col.match[1]] = md.i.diff
    diff.irr[,col.match[2]] = lt.i.diff
  }
}

total.water.diff = diff.precip + diff.icemlt






# time periods: different fill
# RCPs: different different shapes
# basins: different colors
# yellow: '#ffe119',

cols = c('#e6194b', '#3cb44b', '#4363d8', '#f58231', '#911eb4', 
         '#46f0f0', '#f032e6', '#bcf60c', '#fabebe', '#008080', 
         '#e6beff', '#9a6324', "#A9A9A9", '#800000', '#000075',
         '#808080','#aaffc3', '#808000', '#ffd8b1')


png("figures/Quadrant/2x2_example.png", res=100, height=1000, width=1500)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(diff.precip[b,1], diff.icemlt[b,1], 
         xlim=c(min(diff.precip),max(diff.precip)), 
         ylim=c(min(diff.icemlt), max(diff.icemlt)), 
         xlab = "Change in Precipitation (km3/year)",
         ylab = "Change in Glacier Ice Melt (km3/year)",
         pch=1,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=-1, lty=2)
  }
  points(diff.precip[b,2], diff.icemlt[b,2], pch=19, col=cols[b])  # late, rcp45
  
  points(diff.precip[b,3], diff.icemlt[b,3], pch=2,  col=cols[b])  # mid, rcp85
  points(diff.precip[b,4], diff.icemlt[b,4], pch=17,  col=cols[b]) # late, rcp85
  
  # model 2
  points(diff.precip[b,5], diff.icemlt[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(diff.precip[b,6], diff.icemlt[b,6], pch=19,  col=cols[b]) # late, rcp45
  
  points(diff.precip[b,7], diff.icemlt[b,7], pch=2,  col=cols[b])  # mid, rcp45
  points(diff.precip[b,8], diff.icemlt[b,8], pch=17,  col=cols[b]) # late, rcp45
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.8, inset=c(-0.6,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.8, inset=c(-0.45, 0.8), legend=c("RCP 4.5", "RCP 8.5"), pch=c(19,17), title="Climate Scenario")
legend("topright", bty='n', cex=0.8, inset=c(-0.45, 0.999), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()



# diff total water vs diff irr
png("figures/Quadrant/Diff_water_vs_diff_irr.png", res=100, height=1000, width=1500)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(total.water.diff[b,1], diff.irr[b,1],                      # mid, rcp45
         xlim=c(min(total.water.diff),max(total.water.diff)),  
         ylim=c(min(diff.irr), max(diff.irr)), 
         xlab = "Change in Water Available (km3/year)",
         ylab = "Change in Irr Water Demand (km3/year)",
         pch=1,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=1, lty=2)
  }
  points(total.water.diff[b,2], diff.irr[b,2], pch=19, col=cols[b])  # late, rcp45
  
  points(total.water.diff[b,3], diff.irr[b,3], pch=2,  col=cols[b])  # mid, rcp85
  points(total.water.diff[b,4], diff.irr[b,4], pch=17,  col=cols[b]) # late, rcp85
  
  # model 2
  points(total.water.diff[b,5], diff.irr[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(total.water.diff[b,6], diff.irr[b,6], pch=19,  col=cols[b]) # late, rcp45
  
  points(total.water.diff[b,7], diff.irr[b,7], pch=2,  col=cols[b])  # mid, rcp85
  points(total.water.diff[b,8], diff.irr[b,8], pch=17,  col=cols[b]) # late, rcp85
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.8, inset=c(-0.8,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.8, inset=c(-0.45, 0.8), legend=c("RCP 4.5", "RCP 8.5"), pch=c(19,17), title="Climate Scenario")
legend("topright", bty='n', cex=0.8, inset=c(-0.45, 0.999), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()


#################################################
# one figure per rcp


png("figures/Quadrant/2x2_example_rcp45.png", res=100, height=1000, width=1500)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(diff.precip[b,1], diff.icemlt[b,1],                       # mid, rcp45
         xlim=c(min(diff.precip),max(diff.precip)), 
         ylim=c(min(diff.icemlt), max(diff.icemlt)), 
         xlab = "Change in Precipitation (km3/year)",
         ylab = "Change in Glacier Ice Melt (km3/year)",
         main = "RCP 4.5",
         pch=1,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=-1, lty=2)
  }
  points(diff.precip[b,2], diff.icemlt[b,2], pch=19, col=cols[b])  # late, rcp45
  
  # model 2
  points(diff.precip[b,5], diff.icemlt[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(diff.precip[b,6], diff.icemlt[b,6], pch=19,  col=cols[b]) # late, rcp45
  
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.8, inset=c(-0.8,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.8, inset=c(-0.45, 0.999), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()


# diff total water vs diff irr
png("figures/Quadrant/Diff_water_vs_diff_irr.png", res=100, height=1000, width=1500)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(total.water.diff[b,1], diff.irr[b,1], 
         xlim=c(min(total.water.diff),max(total.water.diff)), 
         ylim=c(min(diff.irr), max(diff.irr)), 
         xlab = "Change in Water Available (km3/year)",
         ylab = "Change in Irr Water Demand (km3/year)",
         main = "RCP 4.5",
         pch=1,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=1, lty=2)
  }
  points(total.water.diff[b,2], diff.irr[b,2], pch=19, col=cols[b])  # late, rcp45
  
  # model 2
  points(total.water.diff[b,5], diff.irr[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(total.water.diff[b,6], diff.irr[b,6], pch=19,  col=cols[b]) # late, rcp45
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.8, inset=c(-0.8,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.8, inset=c(-0.45, 0.999), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()

################################################################################################

# normalize basins: convert km3 to mm

basins = readOGR("data/basins_hma", "basins_hma")  # shapefile contains basin areas
area.km2 = area(basins)*1e-6  # area of basins in km2

diff.precip.mm       = 1e6*(diff.precip/area.km2)
diff.icemlt.mm       = 1e6*(diff.icemlt/area.km2)
total.water.diff.mm  = 1e6*(total.water.diff/area.km2)
diff.irr.mm          = 1e6*(diff.irr/area.km2)


png("figures/Quadrant/Diff_precip_vs_diff_glacier_mm.png", res=300, units="in", width=8, height=5)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(diff.precip.mm[b,1], diff.icemlt.mm[b,1], 
         xlim=c(min(diff.precip.mm),max(diff.precip.mm)), 
         ylim=c(min(diff.icemlt.mm), max(diff.icemlt.mm)), 
         xlab = "Change in Precipitation (mm/year)",
         ylab = "Change in Glacier Ice Melt (mm/year)",
         pch=1,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=-1, lty=2)
  }
  points(diff.precip.mm[b,2], diff.icemlt.mm[b,2], pch=19, col=cols[b])  # late, rcp45
  
  points(diff.precip.mm[b,3], diff.icemlt.mm[b,3], pch=2,  col=cols[b])  # mid, rcp85
  points(diff.precip.mm[b,4], diff.icemlt.mm[b,4], pch=17,  col=cols[b]) # late, rcp85
  
  # model 2
  points(diff.precip.mm[b,5], diff.icemlt.mm[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(diff.precip.mm[b,6], diff.icemlt.mm[b,6], pch=19,  col=cols[b]) # late, rcp45
  
  points(diff.precip.mm[b,7], diff.icemlt.mm[b,7], pch=2,  col=cols[b])  # mid, rcp45
  points(diff.precip.mm[b,8], diff.icemlt.mm[b,8], pch=17,  col=cols[b]) # late, rcp45
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.75, inset=c(-0.65,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.75, inset=c(-0.35, 0.8), legend=c("RCP 4.5", "RCP 8.5"), pch=c(19,17), title="Climate Scenario")
legend("topright", bty='n', cex=0.75, inset=c(-0.35, 0.999), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()



# diff total water vs diff irr
png("figures/Quadrant/Diff_water_vs_diff_irr_mm.png", res=100, height=1000, width=1500)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(total.water.diff.mm[b,1], diff.irr.mm[b,1],                      # mid, rcp45
         xlim=c(min(total.water.diff.mm),max(total.water.diff.mm)),  
         ylim=c(min(diff.irr.mm), max(diff.irr.mm)), 
         xlab = "Change in Water Available (mm/year)",
         ylab = "Change in Irr Water Demand (mm/year)",
         pch=1,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=1, lty=2)
  }
  points(total.water.diff.mm[b,2], diff.irr.mm[b,2], pch=19, col=cols[b])  # late, rcp45
  
  points(total.water.diff.mm[b,3], diff.irr.mm[b,3], pch=2,  col=cols[b])  # mid, rcp85
  points(total.water.diff.mm[b,4], diff.irr.mm[b,4], pch=17,  col=cols[b]) # late, rcp85
  
  # model 2
  points(total.water.diff.mm[b,5], diff.irr.mm[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(total.water.diff.mm[b,6], diff.irr.mm[b,6], pch=19,  col=cols[b]) # late, rcp45
  
  points(total.water.diff.mm[b,7], diff.irr.mm[b,7], pch=2,  col=cols[b])  # mid, rcp85
  points(total.water.diff.mm[b,8], diff.irr.mm[b,8], pch=17,  col=cols[b]) # late, rcp85
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.8, inset=c(-0.6,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.8, inset=c(-0.4, 0.8), legend=c("RCP 4.5", "RCP 8.5"), pch=c(19,17), title="Climate Scenario")
legend("topright", bty='n', cex=0.8, inset=c(-0.4, 0.999), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()



######################################################################################################

# one plot per RCP

# RCP 45
png("figures/Quadrant/Diff_precip_vs_diff_glacier_RCP45_mm.png", res=300, units="in", width=8, height=5)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(diff.precip.mm[b,1], diff.icemlt.mm[b,1], 
         xlim=c(min(diff.precip.mm),max(diff.precip.mm)), 
         ylim=c(min(diff.icemlt.mm), max(diff.icemlt.mm)), 
         xlab = "Change in Precipitation (mm/year)",
         ylab = "Change in Glacier Ice Melt (mm/year)",
         main = "RCP 4.5",
         pch=1,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=-1, lty=2)
  }
  points(diff.precip.mm[b,2], diff.icemlt.mm[b,2], pch=19, col=cols[b])  # late, rcp45
  
  # model 2
  points(diff.precip.mm[b,5], diff.icemlt.mm[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(diff.precip.mm[b,6], diff.icemlt.mm[b,6], pch=19,  col=cols[b]) # late, rcp45
  
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.75, inset=c(-0.6,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.75, inset=c(-0.35, 0.8), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()


# rcp85
png("figures/Quadrant/Diff_precip_vs_diff_glacier_RCP85_mm.png", res=300, units="in", width=8, height=5)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(diff.precip.mm[b,1], diff.icemlt.mm[b,3], 
         xlim=c(min(diff.precip.mm),max(diff.precip.mm)), 
         ylim=c(min(diff.icemlt.mm), max(diff.icemlt.mm)), 
         xlab = "Change in Precipitation (mm/year)",
         ylab = "Change in Glacier Ice Melt (mm/year)",
         main = "RCP 8.5",
         pch=2,  col=cols[b]) # mid, rcp85
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=-1, lty=2)
  }
  points(diff.precip.mm[b,2], diff.icemlt.mm[b,4], pch=17, col=cols[b])  # late, rcp85
  
  # model 2
  points(diff.precip.mm[b,5], diff.icemlt.mm[b,7], pch=2,  col=cols[b])  # mid, rcp85
  points(diff.precip.mm[b,6], diff.icemlt.mm[b,8], pch=17,  col=cols[b]) # late, rcp85
  
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.75, inset=c(-0.6,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.75, inset=c(-0.35, 0.8), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()



######################################################################################################

# one plot per time period

png("figures/Quadrant/Diff_precip_vs_diff_glacier_mm_2040_2069.png", res=300, units="in", width=8, height=5)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(diff.precip.mm[b,1], diff.icemlt.mm[b,1], 
         xlim=c(min(diff.precip.mm),max(diff.precip.mm)), 
         ylim=c(min(diff.icemlt.mm), max(diff.icemlt.mm)), 
         xlab = "Change in Precipitation (mm/year)",
         ylab = "Change in Glacier Ice Melt (mm/year)",
         main = "Mid Century 2040-2069",
         pch=19,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=-1, lty=2)
  }
  
  points(diff.precip.mm[b,3], diff.icemlt.mm[b,3], pch=17,  col=cols[b])  # mid, rcp85
  
  # model 2
  points(diff.precip.mm[b,5], diff.icemlt.mm[b,5], pch=19,  col=cols[b])  # mid, rcp45
  
  points(diff.precip.mm[b,7], diff.icemlt.mm[b,7], pch=17,  col=cols[b])  # mid, rcp85
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.75, inset=c(-0.5,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.75, inset=c(-0.25, 0.8), legend=c("RCP 4.5", "RCP 8.5"), pch=c(19,17), title="Climate Scenario")
dev.off()

png("figures/Quadrant/Diff_precip_vs_diff_glacier_mm_2070_2099.png", res=300, units="in", width=8, height=5)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(diff.precip.mm[b,2], diff.icemlt.mm[b,1], 
         xlim=c(min(diff.precip.mm),max(diff.precip.mm)), 
         ylim=c(min(diff.icemlt.mm), max(diff.icemlt.mm)), 
         xlab = "Change in Precipitation (mm/year)",
         ylab = "Change in Glacier Ice Melt (mm/year)",
         main = "Late Century 2070-2099",
         pch=19,  col=cols[b]) # late, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=-1, lty=2)
  }
  
  points(diff.precip.mm[b,4], diff.icemlt.mm[b,4], pch=17,  col=cols[b]) # late, rcp85
  
  # model 2
  points(diff.precip.mm[b,6], diff.icemlt.mm[b,6], pch=19,  col=cols[b]) # late, rcp45
  
  points(diff.precip.mm[b,8], diff.icemlt.mm[b,8], pch=17,  col=cols[b]) # late, rcp85
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.75, inset=c(-0.5,-0.05), legend=as.character(hist.icemlt$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.75, inset=c(-0.25, 0.8), legend=c("RCP 4.5", "RCP 8.5"), pch=c(19,17), title="Climate Scenario")
dev.off()

######################################################################################################

# one plot per time basin

for(b in 1:15){
  #png(paste("figures/Quadrant/Diff_precip_vs_diff_glacier_mm_", basins$name[b],".png", sep=""), res=300, units="in", width=8, height=5)
  par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
  plot(diff.precip.mm[b,1], diff.icemlt.mm[b,1], 
       xlim=c(min(diff.precip.mm),max(diff.precip.mm)), 
       ylim=c(min(diff.icemlt.mm), max(diff.icemlt.mm)), 
       xlab = "Change in Precipitation (mm/year)",
       ylab = "Change in Glacier Ice Melt (mm/year)",
       main = basins$name[b],
       pch=1,  col=cols[b]) # mid, rcp45
  
  abline(h=0)
  abline(v=0)
  abline(a=0, b=-1, lty=2)
  
  points(diff.precip.mm[b,2], diff.icemlt.mm[b,2], pch=19, col=cols[b])  # late, rcp45
  
  points(diff.precip.mm[b,3], diff.icemlt.mm[b,3], pch=2,  col=cols[b])  # mid, rcp85
  points(diff.precip.mm[b,4], diff.icemlt.mm[b,4], pch=17,  col=cols[b]) # late, rcp85
  
  # model 2
  points(diff.precip.mm[b,5], diff.icemlt.mm[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(diff.precip.mm[b,6], diff.icemlt.mm[b,6], pch=19,  col=cols[b]) # late, rcp45
  
  points(diff.precip.mm[b,7], diff.icemlt.mm[b,7], pch=2,  col=cols[b])  # mid, rcp45
  points(diff.precip.mm[b,8], diff.icemlt.mm[b,8], pch=17,  col=cols[b]) # late, rcp45
  
  par(xpd=TRUE)
  #legend("topright", bty='n', cex=0.75, inset=c(-0.65,-0.05), legend=as.character(hist.precip$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
  legend("topright", bty='n', cex=0.75, inset=c(-0.35, 0.8), legend=c("RCP 4.5", "RCP 8.5"), pch=c(19,17), title="Climate Scenario")
  legend("topright", bty='n', cex=0.75, inset=c(-0.35, 0.999), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
  #dev.off()
  
}



# diff total water vs diff irr
png("figures/Quadrant/Diff_water_vs_diff_irr_mm.png", res=100, height=1000, width=1500)
par(mar=c(5.1, 4.1, 4.1, 15.1), xpd=FALSE)
for(b in 1:15){
  if(b==1){
    plot(total.water.diff.mm[b,1], diff.irr.mm[b,1],                      # mid, rcp45
         xlim=c(min(total.water.diff.mm),max(total.water.diff.mm)),  
         ylim=c(min(diff.irr.mm), max(diff.irr.mm)), 
         xlab = "Change in Water Available (mm/year)",
         ylab = "Change in Irr Water Demand (mm/year)",
         pch=1,  col=cols[b]) # mid, rcp45
    
    abline(h=0)
    abline(v=0)
    abline(a=0, b=1, lty=2)
  }
  points(total.water.diff.mm[b,2], diff.irr.mm[b,2], pch=19, col=cols[b])  # late, rcp45
  
  points(total.water.diff.mm[b,3], diff.irr.mm[b,3], pch=2,  col=cols[b])  # mid, rcp85
  points(total.water.diff.mm[b,4], diff.irr.mm[b,4], pch=17,  col=cols[b]) # late, rcp85
  
  # model 2
  points(total.water.diff.mm[b,5], diff.irr.mm[b,5], pch=1,  col=cols[b])  # mid, rcp45
  points(total.water.diff.mm[b,6], diff.irr.mm[b,6], pch=19,  col=cols[b]) # late, rcp45
  
  points(total.water.diff.mm[b,7], diff.irr.mm[b,7], pch=2,  col=cols[b])  # mid, rcp85
  points(total.water.diff.mm[b,8], diff.irr.mm[b,8], pch=17,  col=cols[b]) # late, rcp85
}
par(xpd=TRUE)
legend("topright", bty='n', cex=0.8, inset=c(-0.6,-0.05), legend=as.character(hist.precip$Basin), pch=rep(19,15), title="Basin", col=cols[1:15])
legend("topright", bty='n', cex=0.8, inset=c(-0.4, 0.8), legend=c("RCP 4.5", "RCP 8.5"), pch=c(19,17), title="Climate Scenario")
legend("topright", bty='n', cex=0.8, inset=c(-0.4, 0.999), legend=c("Mid Century", "Late Century"), pch=c(1,19), title="Time Period")
dev.off()

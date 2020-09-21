# month_max_water()

# identify month of max for water sources

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(RColorBrewer)
##############################################################################

# rain
rain.mc = read.csv("results/rainFall/ERA_hist_basin_rainFall_km3_1980_2009_mc.csv")

# snowmelt
snow.mc = read.csv("results/snowMelt/ERA_hist_basin_snowMelt_km3_1980_2009_mc.csv")

# rain + snow
rainsnow.mc = cbind(rain.mc[,1], (rain.mc[,2:13] + snow.mc[,2:13]))
colnames(rainsnow.mc)[1] = "Basin"
rainsnow.max = apply(rainsnow.mc[,2:13], c(1), FUN=max)

rainsnow.max.mo = data.frame(matrix(nr=nrow(rainsnow.mc), nc=2))
rainsnow.max.mo[,1] = rainsnow.mc[,1]

for(i in 1:nrow(rainsnow.mc)){
  rainsnow.max.mo[i,2] = (which(rainsnow.mc[i,] == rainsnow.max[i]) - 1)
}
colnames(rainsnow.max.mo) = c("Basin", "month_of_max_rainsnow")

# glacier ice melt
glMelt.mc = read.csv("results/glMelt/ERA_hist_basin_glMelt_km3_1980_2009_mc.csv")[,1:13]

glMelt.max = apply(glMelt.mc[,2:13], c(1), FUN=max)

glMelt.max.mo = data.frame(matrix(nr=nrow(glMelt.mc), nc=2))
glMelt.max.mo[,1] = glMelt.mc[,1]

for(i in 5:nrow(glMelt.mc)){
  glMelt.max.mo[i,2] = (which(glMelt.mc[i,] == glMelt.max[i]) - 1)
}
colnames(glMelt.max.mo) = c("Basin", "month_of_max_glMelt")

mo.diff = rainsnow.max.mo$month_of_max_rainsnow - glMelt.max.mo$month_of_max_glMelt


basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
basins$month.diff = mo.diff[1:15]

#spplot(irrGross, zcol=seq(2,13), sp.layout = list(basin.layer, coastline.layer), col.regions = red.pal, col = "transparent", as.table=TRUE)

basin.layer = list("sp.polygons", basins, col = "grey", first=FALSE)
coastline = readOGR("data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basins))
coastline.layer = list("sp.polygons", coastline, col = "darkgrey", lwd=1.5, first=TRUE)


png("figures/ERA_hist_MaxMonthDiff.png", width = 1500, height=1000, res=130)
spplot(basins, zcol = "month.diff", col='grey', sp.layout = list(basin.layer, coastline.layer))
dev.off()


# gross irrigation
# glacier ice melt
grossIrr.mc = read.csv("results/irrigationGross/ERA_hist_basin_irrigationGross_km3_1980_2009_mc.csv")[,1:13]

grossIrr.max = apply(grossIrr.mc[,2:13], c(1), FUN=max)

grossIrr.max.mo = data.frame(matrix(nr=nrow(grossIrr.mc), nc=2))
grossIrr.max.mo[,1] = grossIrr.mc[,1]

for(i in 1:nrow(grossIrr.mc)){
  grossIrr.max.mo[i,2] = (which(grossIrr.mc[i,] == grossIrr.max[i]) - 1)
}
colnames(grossIrr.max.mo) = c("Basin", "month_of_max_grossIrr")


test = cbind(grossIrr.max.mo, glMelt.max.mo[,2], rainsnow.max.mo[,2])

####################################################################################
##### glacier ice melt as a % of total water available by month (mc)

# glacier ice melt as a % of total water available by month (mc)

water.res.mc = cbind(rain.mc[,1], (rainsnow.mc[,2:13] + glMelt.mc[,2:13]))

glMelt.percent = cbind(glMelt.mc[,1], 100*(glMelt.mc[,2:13]/water.res.mc[,2:13]))
for(i in 1:15){
  if(i == 1){
    plot(as.numeric(glMelt.percent[i,2:13]), type='l', ylim=c(0,20))
  }else{
    lines(as.numeric(glMelt.percent[i,2:13]), col=i)
  }
 
}

glMelt.percent.max = cbind(as.character(glMelt.mc[,1]), apply(glMelt.percent[,2:13], c(1), FUN=max))


irrGross_pgi_y_stats  = read.csv("results/Irrigation/irrigationGross/ERA_hist_basin_GrossIrr_pgi_1980_2009_yearly_stats.csv")


png("figures/Basin_Qpgi_vs_glMeltResources_scatter.png",width = 1000, height=1000, res=160)
plot(irrGross_pgi_y_stats$GrossIrr_percent_pgi[1:15]  ~ as.numeric(glMelt.percent.max[,2])[1:15],
     xlab = "Glacier ice melt as percent of water resources",
     ylab = "Glacier ice melt as percent of gross irrigation water")
text(irrGross_pgi_y_stats$GrossIrr_percent_pgi[1:15]  ~ as.numeric(glMelt.percent.max[,2])[1:15], 
     labels=glMelt.percent.max[1:15,1],  pos=4, cex=0.8)
dev.off()

abline(lm(irrGross_pgi_y_stats$GrossIrr_percent_pgi[1:15]  ~ as.numeric(glMelt.percent.max[,2])[1:15]))
summary(lm(irrGross_pgi_y_stats$GrossIrr_percent_pgi[1:15]  ~ as.numeric(glMelt.percent.max[,2])[1:15]))


irrGross.sub = subset(irrGross_pgi_y_stats, rownames(irrGross_pgi_y_stats) != "Luni_ext" & rownames(irrGross_pgi_y_stats) != "Amu_Darya")
glMelt.sub = subset(glMelt.percent.max, glMelt.percent.max[,1]  != "Luni_ext" & glMelt.percent.max[,1] != "Amu_Darya")

summary(lm(irrGross.sub$GrossIrr_percent_pgi[1:13]  ~ as.numeric(glMelt.sub[,2][1:13])))
plot(irrGross.sub$GrossIrr_percent_pgi[1:13]  ~ as.numeric(glMelt.sub[,2])[1:13])
abline(lm(irrGross.sub$GrossIrr_percent_pgi[1:13]  ~ as.numeric(glMelt.sub[,2][1:13])))

       
basins$pmax = as.numeric(glMelt.percent.max[1:15,2])

# saturate color scale so Amu Darya doesn't stick out so much
basins$pmax.adj = basins$pmax  
basins$pmax.adj[6] = 11

basin.layer = list("sp.polygons", basins, col = "grey", first=FALSE)
coastline = readOGR("data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basins))
coastline.layer = list("sp.polygons", coastline, col = "darkgrey", lwd=1.5, first=TRUE)

or.pal = colorRampPalette(brewer.pal(n = 7, name = "Oranges"))(20)
bupu.pal = colorRampPalette(brewer.pal(n = 7, name = "BuPu"))(20)

#spplot(irrGross, zcol=seq(2,13), , col.regions = red.pal, col = "transparent", as.table=TRUE)
png("figures/Basin_glMelt_percent_water_sources_map.png",width = 1500, height=1000, res=130)
spplot(basins, zcol = "pmax.adj", sp.layout = list(basin.layer, coastline.layer), col='grey', col.regions = bupu.pal)
dev.off()


basins$irr_pgi = irrGross_pgi_y_stats$GrossIrr_percent_pgi[1:15]
png("figures/Basin_Qpgi_percent_map.png",width = 1500, height=1000, res=130)
spplot(basins, zcol = "irr_pgi", sp.layout = list(basin.layer, coastline.layer), col='grey', col.regions = or.pal)
dev.off()

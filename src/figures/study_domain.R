# Study region figure

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(RColorBrewer)

# shapefiles for spatial agg
basins = readOGR("data/basins_hma", "basins_hma")  # basins to aggregate over
coastline = readOGR("data/land-polygons-generalized-3857/", layer = "land_polygons_z4")
coastline = spTransform(coastline, crs(basins))
coastline.shadow = raster::shift(x=coastline, dx= 0.15, dy=-0.08)
countries = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/FAOCountryBoundaries2012/", "Adm012p")
countries = spTransform(countries, crs(basins))

# cropland
cropland = raster("/net/nfs/bog/raid/data/dgrogan/MIRCA_data/WBM_format/crop_area_fraction.nc")
cropland[cropland == 0] = NA

# crop production
prod.files = list.files("/net/nfs/squam/raid/data/GLEAM/HarvardDataverse_GAEZ+_2015/GAEZ+_2015_crop_production/", pattern =".tif", full.names=T)
data.brk = do.call(stack,
                   lapply(prod.files, 
                          raster)
)
prod.sum = stackApply(data.brk, indices=rep(1, length(prod.files)), fun = sum)
writeRaster(prod.sum, "data/GAEZ_2015_crop_production.nc", varname = "production", varunit = "1000_tonnes", overwrite=T)
prod.sum.hma = mask(prod.sum, basins)

# irrigated crop production
irr.files = list.files("/net/nfs/squam/raid/data/GLEAM/HarvardDataverse_GAEZ+_2015/GAEZ+_2015_crop_production/", pattern ="Irrigated.tif", full.names=T)
data.brk = do.call(stack,
                   lapply(irr.files, 
                          raster)
)
irr.sum = stackApply(data.brk, indices=rep(1, length(irr.files)), fun = sum)
writeRaster(irr.sum, "data/GAEZ_2015_Irrigated_crop_production.nc", varname = "production", varunit = "1000_tonnes", overwrite=T)
irr.sum.hma = mask(irr.sum, basins)

# glacier area
glacier.area = raster("/net/nfs/yukon/raid5/data/RGI6/global_rgi6_glaciers_1km.tif")
glacier.area = crop(glacier.area, extent(basins))
glacier.area = mask(glacier.area, basins)

# Political boundaries
China        = countries[as.character(countries$ADM0_NAME) == "China",]
India        = countries[as.character(countries$ADM0_NAME) == "India",]
Nepal        = countries[as.character(countries$ADM0_NAME) == "Nepal",]
Pakistan     = countries[as.character(countries$ADM0_NAME) == "Pakistan",]
Afghanistan  = countries[as.character(countries$ADM0_NAME) == "Afghanistan",]
Turkmenistan = countries[as.character(countries$ADM0_NAME) == "Turkmenistan",]
Tajikistan   = countries[as.character(countries$ADM0_NAME) == "Tajikistan",]
Kyrgyzstan   = countries[as.character(countries$ADM0_NAME) == "Kyrgyzstan",]
Kazakhstan   = countries[as.character(countries$ADM0_NAME) == "Kazakhstan",]
Bangladesh   = countries[as.character(countries$ADM0_NAME) == "Bangladesh",]
Bhutan       = countries[as.character(countries$ADM0_NAME) == "Bhutan",]
Myanmar      = countries[as.character(countries$ADM0_NAME) == "Myanmar",]
Laos         = countries[as.character(countries$ADM0_NAME) == "Laos",]
Cambodia     = countries[as.character(countries$ADM0_NAME) == "Cambodia",]
Vietnam      = countries[as.character(countries$ADM0_NAME) == "Vietnam",]
Thailand     = countries[as.character(countries$ADM0_NAME) == "Thailand",]
Iran         = countries[as.character(countries$ADM0_NAME) == "Iran",]

# basins
Yangtze     = basins[basins$name =="Yangtze",]
Indus       = basins[basins$name =="Indus",]
Ganges      = basins[basins$name =="Ganges",]
Yellow      = basins[basins$name =="Yellow",]
Mekong      = basins[basins$name =="Mekong",]
Amu_Darya   = basins[basins$name =="Amu_Darya",]
Brahmaputra = basins[basins$name =="Brahmaputra",]
Syr_Darya   = basins[basins$name =="Syr_Darya",]
Salween     = basins[basins$name =="Salween",]
Irawaddy    = basins[basins$name =="Irawaddy",]
Tarim       = basins[basins$name =="Tarim",]
Ili         = basins[basins$name =="Ili",]
Luni_ext   = basins[basins$name =="Luni_ext",]
Inner_Tibetan_Plateau       = basins[basins$name =="Inner_Tibetan_Plateau",]
Inner_Tibetan_Plateau_extended       = basins[basins$name =="Inner_Tibetan_Plateau_extended",]


# spatial extent
xl = c(59, 120)
yl = c(9, 49)

plot(coastline.shadow,  xlim = xl, ylim = yl, border='grey90', lwd=5, axes=T)
#plot(coastline,         xlim = xl, ylim = yl, border='grey70', col='white',  lwd=1, add=T)
plot(countries,         xlim = xl, ylim = yl, add=T)
plot(Indus,       col=adjustcolor("grey",alpha.f=0.6), border='grey', add=T)
plot(Ganges,      col=adjustcolor("mediumturquoise",alpha.f=0.6), border='grey', add=T)
plot(Brahmaputra, col=adjustcolor("blue",alpha.f=0.4), border='grey', add=T)
plot(Amu_Darya,   col=adjustcolor("red",alpha.f=0.6), border='grey', add=T)
plot(Syr_Darya,   col=adjustcolor("green3",alpha.f=0.6), border='grey', add=T)
plot(Yangtze,     col=adjustcolor("orange",alpha.f=0.6), border='grey', add=T)
plot(Salween,     col=adjustcolor("maroon2",alpha.f=0.6), border='grey', add=T)
plot(Mekong,      col=adjustcolor("olivedrab4",alpha.f=0.6), border='grey', add=T)
plot(Yellow,      col=adjustcolor("yellow",alpha.f=0.6), border='grey', add=T)
plot(Tarim,       col=adjustcolor("chocolate",alpha.f=0.6), border='grey', add=T)
plot(Ili,         col=adjustcolor("indianred4",alpha.f=0.6), border='grey', add=T)
plot(Luni_ext,    col=adjustcolor("lightgreen",alpha.f=0.6), border='grey', add=T)
plot(Inner_Tibetan_Plateau,            col=adjustcolor("red3",alpha.f=0.6), border='grey', add=T)
plot(Inner_Tibetan_Plateau_extended,   col=adjustcolor("pink",alpha.f=0.6), border='grey', add=T)

Indus.loc       = getSpPPolygonsLabptSlots(Indus) - c(0,0)
Ganges.loc      = getSpPPolygonsLabptSlots(Ganges)
Brahmaputra.loc = getSpPPolygonsLabptSlots(Brahmaputra) + c(-0.1,1.3)
Amu_Darya.loc    = getSpPPolygonsLabptSlots(Amu_Darya) - c(0,1.2)
Syr_Darya.loc    = getSpPPolygonsLabptSlots(Syr_Darya) + c(0,1)
Yangtze.loc     = getSpPPolygonsLabptSlots(Yangtze)
Salween.loc     = getSpPPolygonsLabptSlots(Salween) - c(0.0, 3.5)
Mekong.loc      = getSpPPolygonsLabptSlots(Mekong) - c(-1.8,3.6)
Yellow.loc      = getSpPPolygonsLabptSlots(Yellow)
Irawaddy.loc  = getSpPPolygonsLabptSlots(Irawaddy) + c(0,2)
Tarim.loc      = getSpPPolygonsLabptSlots(Tarim)
Ili.loc      = getSpPPolygonsLabptSlots(Ili)
Luni_ext.loc      = getSpPPolygonsLabptSlots(Luni_ext)
Inner_Tibetan_Plateau.loc      = getSpPPolygonsLabptSlots(Inner_Tibetan_Plateau)
Inner_Tibetan_Plateau_extended.loc      = getSpPPolygonsLabptSlots(Inner_Tibetan_Plateau_extended) + c(0,1)


greens<-colorRampPalette(c(brewer.pal(n=9,name='Greens')))(100)
g1 = greens[10:100]
blues2<-colorRampPalette(c("cadetblue2", "dodgerblue1", "dodgerblue2", "dodgerblue3", "dodgerblue4"))(100)

prod.sum.hma[prod.sum.hma==0]=NA
irr.sum.hma[irr.sum.hma==0]=NA
glacier.area[glacier.area==0]=NA
sz=0.6

png("figures/Study_area.png", height=6, width=7, units = 'in', res=300)
par(mar=c(7, 4, 0.5, 0.5))
plot(countries, xlim = xl, ylim = yl, border='grey72', axes=T, cex.axis = 0.9)
plot(irr.sum.hma,  col = g1, add = T, legend=F)
plot(countries, add = T, border='grey72', axes=T, cex.axis = 0.9)
plot(glacier.area>0, col="dodgerblue3", add=T, legend=F)
plot(basins, lwd=0.6,   add = T)

plot(prod.sum.hma, col = g1, add=T, box=F, axes=T, las=1, legend.only=T,  horizontal=T,
     legend.args=list(text='Irrigated Crop Production (1000 Tonnes)', side=3, font=1, line=0.05, cex = 0.8))
dev.off()



png("figures/Study_area_v2.png", height=6, width=7, units = 'in', res=300)
par(mar=c(7, 4, 0.5, 0.5))
plot(coastline.shadow,  xlim = xl, ylim = yl, border='grey90', lwd=4, axes=T, lwd.axes = 1)
plot(coastline,         xlim = xl, ylim = yl, border='grey70', col='white',  lwd=1, add=T)
plot(irr.sum.hma,  col = g1, add = T, legend=F)
plot(glacier.area>0, col="dodgerblue3", add=T, legend=F)
plot(basins, lwd=0.6,   add = T)

text(Indus.loc,       labels="Indus",       cex=sz)
text(Ganges.loc,      labels="Ganges",      cex=sz)
text(Brahmaputra.loc, labels="Brahmaputra", cex=sz)
text(Amu_Darya.loc,    labels="Amu Darya",   cex=sz)
text(Syr_Darya.loc,    labels="Syr Darya",   cex=sz)
text(Yangtze.loc,     labels="Yangtze",     cex=sz)
text(Salween.loc,     labels="Salween",     cex=sz)
text(Mekong.loc,      labels="Mekong",      cex=sz)
text(Tarim.loc,      labels="Tarim",      cex=sz)
text(Yellow.loc,      labels="Yellow",      cex=sz)
text(Ili.loc,     labels="Ili",     cex=sz)
text(Irawaddy.loc,     labels="Irawaddy",     cex=sz)
text(Inner_Tibetan_Plateau.loc,      labels="ITP",      cex=sz)
text(Inner_Tibetan_Plateau_extended.loc,      labels="ITP ext.",      cex=sz)

plot(prod.sum.hma, col = g1, add=T, box=F, axes=T, las=1, legend.only=T,  horizontal=T,
     legend.args=list(text='Irrigated Crop Production (1000 Tonnes)', side=3, font=1, line=0.05, cex = 0.8))
dev.off()

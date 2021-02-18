# pygem_climatology()
# Generate climatology grids of PyGEM model output for use in spinup test run
# project: NASA HiMAT
# Danielle S Grogan

# Libraries
library(raster)
library(rgdal)
library(lubridate)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory

##################################################################
out.dir = "/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset/"

# MONTHLY FILES
### NB: glacier files start in 1979-10-00
# make a list of the layer names within the monthly glacier file that correspond to the time series 1981-01-00 to 2010-12-00
month.subset = seq(from = ymd("1981-01-15"), to = ymd("2010-12-15"), by="month")
month.subset.nm = paste("X", month.subset, sep="")
month.subset.nm = sub("-", ".", month.subset.nm)
month.subset.nm = sub("-", ".", month.subset.nm)

# Runoff
gl.runoff.m = brick("/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset/ERA-Interim_c2_ba1_100sets_1980_2017_m.nc",
                    varname = "runoff")
gl.runoff.m_1981_2010 = subset(gl.runoff.m, which(names(gl.runoff.m) %in% month.subset.nm) )

brk.data = gl.runoff.m_1981_2010
ind = rep(seq(1:12), nlayers(brk.data)/12)

brk.agg = stackApply(brk.data, indices = ind, fun = mean,
                     filename = file.path(out.dir, "glacier_runoff_ERA-Interim_c2_ba1_100sets_1981_2010_mc.nc"), 
                     varname  = "runoff",
                     varunit  = "m3/month",
                     longname = "Glacier runoff",
                     zname    = "time",
                     zunit    = "month",
                     format   ="CDF",
                     overwrite=T)


# Ice Melt
gl.IceMelt.m = brick("/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset/ERA-Interim_c2_ba1_100sets_1980_2017_m.nc", 
                       varname = "melt") 

gl.IceMelt.m_1981_2010 = subset(gl.IceMelt.m, which(names(gl.IceMelt.m) %in% month.subset.nm) )

brk.data = gl.IceMelt.m_1981_2010
ind = rep(seq(1:12), nlayers(brk.data)/12)

brk.agg = stackApply(brk.data, indices = ind, fun = mean,
                     filename = file.path(out.dir, "glacier_iceMelt_ERA-Interim_c2_ba1_100sets_1981_2010_mc.nc"), 
                     varname  = "melt",
                     varunit  = "m3/month",
                     longname = "Glacier ice melt",
                     zname    = "time",
                     zunit    = "month",
                     format   ="CDF",
                     overwrite=T)



# YEARLY FILES
# make a list of the layer names within the monthly glacier file that correspond to the time series 1981-01-00 to 2010-12-00
year.subset = seq(from = ymd("1981-07-01"), to = ymd("2010-07-01"), by="year")
year.subset.nm = paste("X", year.subset, sep="")
year.subset.nm = sub("-", ".", year.subset.nm)
year.subset.nm = sub("-", ".", year.subset.nm)

# Area
gl.area.y = brick("/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset/ERA-Interim_c2_ba1_100sets_1980_2017_y.nc", 
                  varname = "area_frac")
gl.area.y_1981_2010 = subset(gl.area.y, which(names(gl.area.y) %in% year.subset.nm))
stackApply(gl.area.y_1981_2010, indices=rep(1, nlayers(gl.area.y_1981_2010)), fun = mean, 
           filename = file.path(out.dir, "glacier_area_ERA-Interim_c2_ba1_100sets_1981_2010_yc.nc"),
           varname  = "area_frac",
           varunit  = "fraction",
           longname = "Glacier area fraction",
           format   ="CDF",
           overwrite=T)

# Volume
gl.volume.y = brick("/net/nfs/merrimack/raid2/data/glaciers_6.0/HiMAT_full_210_Subset/ERA-Interim_c2_ba1_100sets_1980_2017_y.nc", 
                  varname = "volume")
gl.volume.y_1981_2010 = subset(gl.volume.y, which(names(gl.volume.y) %in% year.subset.nm))
stackApply(gl.volume.y_1981_2010, indices=rep(1, nlayers(gl.volume.y_1981_2010)), fun = mean, 
           filename = file.path(out.dir, "glacier_volume_ERA-Interim_c2_ba1_100sets_1981_2010_yc.nc"),
           varname = "volume", 
           varunit = "km3", 
           longname = "Glacier Volume",
           format   ="CDF",
           overwrite=T)


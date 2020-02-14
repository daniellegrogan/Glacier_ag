# sandbox, check problems

library(raster)
library(rgdal)

mod.hist = "ERA_hist"
irrGross_pgi_y_stats  = read.csv(file.path("results", mod.hist, paste(mod.hist, "_basin_GrossIrr_pgi_1980_2016_yearly_stats.csv", sep="")), sep=",")
irrFlow_pgi_y_stats   = read.csv(file.path("results", mod.hist, paste(mod.hist, "_basin_IrrFlow_pgi_1980_2016_yearly_stats.csv", sep="")), sep=",")
irrGrwt_pgi_y_stats  = read.csv(file.path("results", mod.hist, paste(mod.hist, "_basin_IrrGrwt_pgi_1980_2016_yearly_stats.csv", sep="")), sep=",")


irrGross_pgi_y = read.csv("results/ERA_hist/ERA_hist_basin_GrossIrr_km3_pgi_1980_2016_yearly.csv")
irrFlow_pgi_y  = read.csv("results/ERA_hist/ERA_hist_basin_IrrFlow_km3_pgi_1980_2016_yearly.csv")
irrGrwt_pgi_y  = read.csv("results/ERA_hist/ERA_hist_basin_IrrGrwt_km3_pgi_1980_2016_yearly.csv")

# test 1980
irrGross_pgi = irrGross_pgi_y[,1:2]
irrFlow_pgi  = irrFlow_pgi_y[,1:2]
irrGrwt_pgi  = irrGrwt_pgi_y[,1:2]

test.sum = irrFlow_pgi[,2] + irrGrwt_pgi[,2]

diff = irrGross_pgi[,2] - test.sum
diff.percent = 100*(diff/irrGross_pgi[,2])



# grid cell level
irr.gross.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgi/wbm_1980.nc")
irr.flow.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pgi/wbm_1980.nc")
irr.grwt.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pgi/wbm_1980.nc")

test.sum = irr.flow.pgi*3645 + irr.grwt.pgi*365
test.diff = irr.gross.pgi*365 - test.sum
test.diff.percent = 100*(test.diff/irr.gross.pgi)

plot(test.diff * (test.diff > 50))



irr.gross = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGross/wbm_1980.nc")
irr.flow = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationFlow/wbm_1980.nc")
irr.grwt = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGrwt/wbm_1980.nc")
irr.ugw  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationExtra/wbm_1980.nc")

test.diff2 = irr.gross - (irr.flow + irr.grwt + irr.ugw)


# test sum of components

# yearly
irr.flow.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pgi/wbm_1980.nc")
irr.flow.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pgn/wbm_1980.nc")
irr.flow.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pr/wbm_1980.nc")
irr.flow.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_ps/wbm_1980.nc")
irr.flow.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrFlow_mm_pu/wbm_1980.nc")

irr.flow = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationFlow/wbm_1980.nc")

test.flow = irr.flow*365 - (irr.flow.pgi + irr.flow.pgn + irr.flow.pr + irr.flow.ps + irr.flow.pu)*365


irr.gw.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pgi/wbm_1980.nc")
irr.gw.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pgn/wbm_1980.nc")
irr.gw.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pr/wbm_1980.nc")
irr.gw.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_ps/wbm_1980.nc")
irr.gw.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/IrrGrwt_mm_pu/wbm_1980.nc")

irr.gw = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGrwt/wbm_1980.nc")

test.gw = irr.gw*365 - (irr.gw.pgi + irr.gw.pgn + irr.gw.pr + irr.gw.ps + irr.gw.pu)*365
test[test==0]=NA
plot(test>50)


irr.gross.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgi/wbm_1980.nc")
irr.gross.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgn/wbm_1980.nc")
irr.gross.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pr/wbm_1980.nc")
irr.gross.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_ps/wbm_1980.nc")
irr.gross.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pu/wbm_1980.nc")

irr.gross = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGross/wbm_1980.nc")

test.gross = irr.gross*365 - (irr.gross.pgi + irr.gross.pgn + irr.gross.pr + irr.gross.ps + irr.gross.pu)*365
test[test==0]=NA
plot(test)


# frac
irr.gross.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgi/wbm_1980.nc")
irr.gross.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgn/wbm_1980.nc")
irr.gross.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pr/wbm_1980.nc")
irr.gross.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_ps/wbm_1980.nc")
irr.gross.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pu/wbm_1980.nc")

sum.frac = irr.gross.pgi + irr.gross.pgn + irr.gross.pr + irr.gross.ps + irr.gross.pu

# daily




# test fraction
irr.gross = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGross/wbm_1980.nc")
irr.flow = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationFlow/wbm_1980.nc")
irr.grwt = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGrwt/wbm_1980.nc")
irr.ugw  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationExtra/wbm_1980.nc")

irr.gross.pgi.frac = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgi/wbm_1980.nc")

irr.gross.pgi.mm = irr.gross * irr.gross.pgi.frac



test = irr.gross.pgi.mm - irr.gross.pgi

test.diff2 = irr.gross.pgi.mm - (irr.flow.pgi + irr.grwt.pgi)




##################

# compare reported mm vs calculated mm

irr.gross = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/irrigationGross/wbm_1980.nc")

# pgi
irr.gross.mm.pgi  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgi/wbm_1980.nc")
irr.gros.frac.pgi = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgi/wbm_1980.nc")

irr.gross.mm.pgi.calc = irr.gros.frac.pgi*irr.gross
diff.pgi = irr.gross.mm.pgi.calc - irr.gross.mm.pgi

# pgn
irr.gross.mm.pgn  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pgn/wbm_1980.nc")
irr.gros.frac.pgn = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pgn/wbm_1980.nc")

irr.gross.mm.pgn.calc = irr.gros.frac.pgn*irr.gross
diff.pgn = irr.gross.mm.pgn.calc - irr.gross.mm.pgn

# ps
irr.gross.mm.ps  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_ps/wbm_1980.nc")
irr.gros.frac.ps = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_ps/wbm_1980.nc")

irr.gross.mm.ps.calc = irr.gros.frac.ps*irr.gross
diff.ps = irr.gross.mm.ps.calc - irr.gross.mm.ps

# pr
irr.gross.mm.pr  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pr/wbm_1980.nc")
irr.gros.frac.pr = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pr/wbm_1980.nc")

irr.gross.mm.pr.calc = irr.gros.frac.pr*irr.gross
diff.pr = irr.gross.mm.pr.calc - irr.gross.mm.pr

# pu
irr.gross.mm.pu  = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_mm_pu/wbm_1980.nc")
irr.gros.frac.pu = raster("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/yearly/GrossIrr_pu/wbm_1980.nc")

irr.gross.mm.pu.calc = irr.gros.frac.pu*irr.gross
diff.pu = irr.gross.mm.pu.calc - irr.gross.mm.pu

### check fraction and mm sums
frac.sum = irr.gros.frac.pgi + irr.gros.frac.pgn + irr.gros.frac.pr + irr.gros.frac.ps + irr.gros.frac.pu


### monthly? will load Jan
irr.gross = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname = "irrigationGross")

# pgi
irr.gross.mm.pgi  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_pgi")
irr.gros.frac.pgi = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_pgi")

irr.gross.mm.pgi.calc = irr.gros.frac.pgi*irr.gross
diff.pgi = irr.gross.mm.pgi.calc - irr.gross.mm.pgi
diff.pgi[diff.pgi==0]=NA
plot(diff.pgi * abs(diff.pgi>1))

# pgn
irr.gross.mm.pgn  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_pgn")
irr.gros.frac.pgn = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_pgn")

irr.gross.mm.pgn.calc = irr.gros.frac.pgn*irr.gross
diff.pgn = irr.gross.mm.pgn.calc - irr.gross.mm.pgn
diff.pgn[diff.pgn==0]=NA
plot(diff.pgn * abs(diff.pgn>1))

# ps
irr.gross.mm.ps  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_ps")
irr.gros.frac.ps = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_ps")

irr.gross.mm.ps.calc = irr.gros.frac.ps*irr.gross
diff.ps = irr.gross.mm.ps.calc - irr.gross.mm.ps
diff.ps[diff.ps==0]=NA
plot(diff.ps * (abs(diff.ps)>1))

# pr
irr.gross.mm.pr  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_pr")
irr.gros.frac.pr = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_pr")

irr.gross.mm.pr.calc = irr.gros.frac.pr*irr.gross
diff.pr = irr.gross.mm.pr.calc - irr.gross.mm.pr
diff.pr[diff.pr==0]=NA
plot(diff.pr * (abs(diff.pr)>1))

# pu
irr.gross.mm.pu  = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_mm_pu")
irr.gros.frac.pu = brick("/net/nfs/squam/raid/data/WBM_TrANS/HiMAT/2019_12/ERA_hist/monthly/wbm_1980.nc", varname="GrossIrr_pu")

irr.gross.mm.pu.calc = irr.gros.frac.pu*irr.gross
diff.pu = irr.gross.mm.pu.calc - irr.gross.mm.pu
diff.pu[diff.pu==0]=NA
plot(diff.pu * (abs(diff.pu)>1))

### check fraction and mm sums
frac.sum = irr.gros.frac.pgi + irr.gros.frac.pgn + irr.gros.frac.pr + irr.gros.frac.ps + irr.gros.frac.pu

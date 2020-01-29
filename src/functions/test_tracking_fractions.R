# test_tracting_fractions()

# Glacier contributions to agriculture paper: 
#    test tracking function for fraction variables
# project: NASA HiMAT
# Danielle S Grogan

### R Libraries
library(RCurl)  # enables sourcing R code from github
library(raster)
library(rgdal)
library(rgeos)

### Source functions from other github repos:
# wbm_load()
wbm_load.script = getURL("https://raw.githubusercontent.com/daniellegrogan/WBMr/master/wbm_load.R", ssl.verifypeer=F)
eval(parse(text=wbm_load.script))



########################################################################################
test_tracking_fractions = function(path, frac.vars, years){
  read.vars = lapply(frac.vars, FUN = function(x) wbm_load(path, x, years))
  
  for(i in 1:nlayers(read.vars[[1]])){
    test = subset(read.vars[[1]],i) + subset(read.vars[[2]],i) + subset(read.vars[[3]],i) + subset(read.vars[[4]],i) + subset(read.vars[[5]],i)
    test[test==0]=NA
    a = round(min(as.matrix(test), na.rm=T), 6)
    b = round(max(as.matrix(test), na.rm=T), 6)
    if(a != 1 | b != 1){
      print(paste("FAIL: sum != 1 on layer", i))
    }else{
      print(paste("layer", i, "good"))
    }
  }
}
########################################################################################
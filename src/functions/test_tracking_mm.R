# test_tracting_mm()

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
test_tracking_mm = function(path, sum.var, component.vars, years){
  read.vars = lapply(component.vars, FUN = function(x) wbm_load(path, x, years))
  read.sum = wbm_load(path, sum.var, years)
  
  for(i in 1:nlayers(read.vars[[1]])){
    component.sum = subset(read.vars[[1]],i) + subset(read.vars[[2]],i)
    check.diff = subset(read.sum, i) - component.sum
    
    a = round(min(as.matrix(check.diff), na.rm=T), 6)
    b = round(max(as.matrix(check.diff), na.rm=T), 6)
    
    if(a != 0 | b != 0){
      print(paste("FAIL: diff != 0 on layer", i, " min =", a, "; max = ", b))
    }else{
      print(paste("layer", i, "good"))
    }
  }
}
########################################################################################
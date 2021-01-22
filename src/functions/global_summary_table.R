# global_summary_table()
  
global_summary_table = function(yc.files,    # list of yearly climatology WBM output files (xxxx_yc.nc)
                                cell.area,   # raster or ascii file, gridded cell area in km2
                                path.out,    # where to write output
                                name.out){   # file name for output (should end in .csv)
  
  # get variable names from files
  var.names = lapply(yc.files, FUN = function(x) strsplit(x, "/")[[1]][12])
  var.names = sub("wbm_", "", unlist(var.names))
  var.names = sub("_yc.nc", "", var.names)
  
  # define data frame 
  global.summary = data.frame(matrix(nr=length(yc.files), nc=2))
  global.summary[,1] = var.names
  colnames(global.summary) = c("variable", "global_sum_km3Yr_2015")
  
  # loop through files, calculate global sum
  for(y in yc.files){
    r = which(yc.files == y)
    var.data = brick(y)*365
    var.km3  = overlay(var.data, cell.area, fun=function(x,y){x*y*mm_to_km})
    global.summary[r,2] = cellStats(var.km3, stat='sum',  na.rm=TRUE)
    
    print(y)
  }
  
  write.csv(global.summary, file.path(out.path, name.out), row.names = F)
  return(global.summary)
}

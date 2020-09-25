# identify month of max (basin level) 

### R Libraries
library(RCurl)  # enables sourcing R code from github

################################################################################################################################
month_max = function(data.mc){
  month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  # identify max value (nb: first column is basin names)
  data.max = apply(data.mc[,2:13], c(1), FUN=max, na.rm=T)
  
  # set up table to get month of max
  data.max.mo = data.frame(matrix(nr=nrow(data.mc), nc=5))
  data.max.mo[,1] = data.mc[,1]
  for(i in 1:nrow(data.mc)){
    data.max.mo[i,2] = (which(data.mc[i,] == data.max[i]) - 1)
    data.max.mo[i,3] = month.names[data.max.mo[i,2]]
    data.max.mo[i,4] = data.max[i]
    data.max.mo[i,5] = data.mc[i, (data.max.mo[i,2]+12) ]   # include the stdev of the value
  }

  colnames(data.max.mo) = c("Basin", "month_of_max", "month_name", "max_value", "max_value_stdev")
  data.max.mo
}
################################################################################################################################

# max glacier ice melt in gross irr by basin: km3
data.mc = read.csv("results/GrossIrr_mm_pgi/ERA_hist_basin_GrossIrr_mm_pgi_km3_1980_2009_mc.csv")
max_mo_GrossIrr_pgi.km3 = month_max(data.mc)
write.csv(max_mo_GrossIrr_pgi.km3, "results/Tables/ERA_hist_IrrGross_pgi_Month_of_Max_km3.csv", row.names=F)

# max glacier ice melt in gross irr by basin: percent
GrossIrr_pgi_km3_m = read.csv("results/GrossIrr_mm_pgi/ERA_hist_basin_GrossIrr_mm_pgi_km3_1980_2009_monthly.csv")
GrossIrr_km3_m = read.csv("results/irrigationGross/ERA_hist_basin_irrigationGross_km3_1980_2009_monthly.csv")
GrssIrr_pgi_percent_m = cbind(GrossIrr_pgi_km3_m[,1],
                           100*(GrossIrr_pgi_km3_m[,2:ncol(GrossIrr_pgi_km3_m)]/GrossIrr_km3_m[,2:ncol(GrossIrr_km3_m)]))
data.mc = monthly_to_mc(GrssIrr_pgi_percent_m, years = seq(1980,2009))
max_mo_GrossIrr_pgi.percent = month_max(data.mc)
write.csv(max_mo_GrossIrr_pgi.percent, "results/Tables/ERA_hist_IrrGross_pgi_Month_of_Max_percent.csv", row.names=F)

# monthly_to_mc()

# Calculate monthly climatologies from a time series of monthly values.  Monthly values taken from spatial aggregation
# Input data format:
  # One column per month
  # column name of format: XYYYY.MM.DD

monthly_to_mc = function(data.months){
  month.num = as.numeric(lapply(X = colnames(data.months), FUN = function(x){unlist(strsplit(x, "\\."))[2]})) 

  month.mean  = data.frame(matrix(nr=nrow(data.months), nc=12))
  month.stdev = data.frame(matrix(nr=nrow(data.months), nc=12))
  
  for(m in 1:12){
    data.m = data.months[, month.num==m]
    month.mean[,m]  = rowMeans(data.m)
    month.stdev[,m] = apply(data.m, c(1), sd)
  }
  
  month.names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(month.mean)  = paste(month.names, "mean",  sep="_")
  colnames(month.stdev) = paste(month.names, "stdev", sep="_")
  out = as.data.frame(cbind(month.mean, month.stdev))
  out
}



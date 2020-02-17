# agg_contribution()
# Calculate contribution of tracked water components to irrigation
# Requires monthly spatial aggregation of all irrigation variables to be complete for the input model and rcp (output from file basin_aggregations.R)

# project: NASA HiMAT
# Danielle S Grogan

#######################################################################################################################################
calc_percent = function(main.var, comp.vars){

  main     = read.csv(paste("results/", main.var, "/", mod.char, "_basin_", main.var, "_km3_", yr.char, "_monthly.csv", sep=""))
  
  for(v in comp.vars){
    irr.comp = read.csv(paste("results/", v, "/",  mod.char, "_basin_", v, "_km3_", yr.char, "_monthly.csv", sep=""), row.names=F)
    irr.percent = 100*(irr.comp[, 2:ncol(irr.comp)]/main[, 2:ncol(main)])
    irr.percent = cbind(as.character(irr.comp[,1]), irr.percent) # 1st column = basin names
    
    # write monthly percent
    write.table(paste("results/", v, "/",  mod.char, "_basin_", v, "_percent_", yr.char, "_monthly.csv", sep=""), row.names=F)
    
    # calculate yearly values
    monthly_to_yearly(data.m = irr.percent, out.nm = paste("results/", v, "/",  mod.char, "_basin_", v, "_percent_", yr.char, "_yearly.csv", sep=""))
  }
}

#######################################################################################################################################
agg_contribution = function(mod,    # character string: climate model 
                            rcp,    # character string: rcp.  set to "NA" for ERA_hist
                            years  # vector of years
){     
  
    if(mod == "ERA_hist"){
      mod.char = "ERA_hist"
    }else{
      mod.char = paste(mod, rcp, sep="_")
    }
  
    yr.char = paste(min(years), max(years), sep="_")
    
    # 1. Gross irrigation
    main.var = "irrigationGross"
    comp.vars = c("GrossIrr_mm_pgi", "GrossIrr_mm_pgn", "GrossIrr_mm_ps", "GrossIrr_mm_pr", "GrossIrr_mm_pu")
    calc_percent(main.var, comp.vars)
    
    # 2. Surface water irrigation
    main.var = "irrigationFlow"
    comp.vars = c("IrrFlow_mm_pgi", "IrrFlow_mm_pgn", "IrrFlow_mm_ps", "IrrFlow_mm_pr", "IrrFlow_mm_pu")
    calc_percent(main.var, comp.vars)
    
  
    # 3. Groundwater irrigation
    main.var = "irrigationGrwt" 
    comp.vars = c("IrrGrwt_mm_pgi", "IrrGrwt_mm_pgn", "IrrGrwt_mm_ps", "IrrGrwt_mm_pr", "IrrGrwt_mm_pu")
    calc_percent(main.var, comp.vars)
}
#######################################################################################################################################

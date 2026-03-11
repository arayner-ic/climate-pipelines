
# Created 02.04.2025

# Clean the ERA5 meteorology data

#-------------------------------------------------------------------------------

# set wd
# rm(list = ls())
# setwd([[path_name_here]])

setwd(paste0(getwd(), "/Output"))

# libraries
library(tidyverse)
library(terra)


# function to retrieve daily statistic
DailyStat <- function(start, stop, datenam, metric, stat, d = d){
  
  # updated to "drop = FALSE" for when you are sumarising across a single hour (happens with some time zones)
  # include weight (#of hours in that day accounted for) to allow for accurate summary calculations
  
  if(stat == "mean"){
    d_stat <- cbind(d[,c(1:2)], d[,-c(1:2)][,start:stop, drop = FALSE] %>% apply(., 1, mean, na.rm = TRUE), "n_hours" = length(start:stop))
    colnames(d_stat)[3] <- paste(metric, stat, sep = "_")
  }
  
  if(stat == "min"){
    d_stat <- cbind(d[,c(1:2)], d[,-c(1:2)][,start:stop, drop = FALSE] %>% apply(., 1, min, na.rm = TRUE), "n_hours" = length(start:stop))
    colnames(d_stat)[3] <- paste(metric, stat, sep = "_")
  }
  
  if(stat == "max"){
    d_stat <- cbind(d[,c(1:2)], d[,-c(1:2)][,start:stop, drop = FALSE] %>% apply(., 1, max, na.rm = TRUE), "n_hours" = length(start:stop))
    colnames(d_stat)[3] <- paste(metric, stat, sep = "_")
  }
  
  if(stat == "sum"){
    d_stat <- cbind(d[,c(1:2)], d[,-c(1:2)][,start:stop, drop = FALSE] %>% apply(., 1, sum, na.rm = TRUE), "n_hours" = length(start:stop))
    colnames(d_stat)[3] <- paste(metric, stat, sep = "_")
  }
  
  d_stat$date <- datenam
  
  return(d_stat)
}

# function to extract the data
ExtractDailyStat <- function(Z, stat, metric, dailystat = TRUE){
  
  # get the x-y coordinates
  d <- as.data.frame(Z, xy=TRUE)
  d[,-c(1:2)] <- d[,-c(1:2)] - 273.15
  
  hour_tr <- as.POSIXct(sub(".*=", "", colnames(d)[-c(1:2)]) %>% as.numeric(), origin = "1970-01-01")
  hour_tr <- format(hour_tr, format='%Y-%m-%d', tz = "Europe/Madrid")
  
  if(dailystat == TRUE){
    # define the start/end points of each date
    dat <- as.data.frame(table(hour_tr))
    
    print(paste("unique time:", nrow(dat)))
    
    start <- numeric(nrow(dat))
    stop <- numeric(nrow(dat))
    
    start[1] <- 1
    stop[1] <- dat$Freq[1]
    
    
    for(i in 2:nrow(dat)){
      start[i] <- stop[i-1] + 1
      stop[i] <- start[i] + dat$Freq[i] - 1
    }
    
    dat$start <- start
    dat$stop <- stop
    
    # run the DailyStat across the data
    GetStat <- 
      apply(dat, 1, function(X){
        # k <- 33
        # start = dat[k,3]; stop = dat[k,4]; datenam = dat[k,1]; stat = stat; d = d; metric = metric
        return(DailyStat(start = X[3], stop = X[4], datenam = X[1], stat = stat, d = d, metric = metric))
      } 
      ) 
    
    GetStat <- do.call(rbind, GetStat)
  }else{
    GetStat <- d
  }
  
  return(GetStat)
}


# # define the metric
# metric <- "2m_temperature"
# # # c("2m_temperature", "total_precipitation")
# # define the statistic
# stat <- "max"
# # mean, min, max and sum


metric_loop <- "2m_temperature" 
stat_loop <- c("mean", "max") #, "min", "max"
i <- j <- 1

for(i in 1:length(metric_loop)){
  
  print(metric_loop[i])
  
  # read the metric files
  files2read <- list.files()[list.files() %>% startsWith(.,metric_loop[i])]
  meteo_extract <- lapply(files2read, terra::rast) 
  
  print(paste0(length(meteo_extract), " temprature rastas read"))
  
  for(j in 1:length(stat_loop)){
    
    print(stat_loop[j])
    
    # run the function
    # Z = meteo_extract[[1]]; stat = stat_loop[j]; metric = metric_loop[i]
    res <- lapply(meteo_extract, function(Z) ExtractDailyStat(Z = Z, stat = stat_loop[j], metric = metric_loop[i]))
    res <- do.call(rbind, res) 
    
    # Store the result
    saveRDS(res, file = paste0("Summary_", metric_loop[i], "_", stat_loop[j], ".rds"))
  }
}


rm(list = ls())
gc()








bkgrd_mean <- function(dat, mean = F){
  
  blks <- dat$no[dat$type == "BLK"]#save index numbers of blank measurements to blks
  
  out <- dat$rawdata #set up an output of the same size as the rawdata list containing data.frames
  blkMean <- data.frame(mn = 1)
  for(col in names(out[[2]])[3:10]){ #go through all columns of the averaged blank
    blkMean[col] <- meanList(dat$rawdata[blks], col = col)
  }
  
  for(i in 1:length(dat$rawdata)){ #go through all blank values except the last one
    
    for(col in names(blkMean)[-1]){ #go through all columns of the averaged blank
      out[[i]][,col] <- out[[i]][,col] - blkMean[col] #... and substract this from the measurement that was done after the blank measurement
    }
  }
  
  out[dat$type == "BLK"] <- NA #the list of background corrected data should contain NA for a blank measurement

  return(out) # return the resulting list out
}

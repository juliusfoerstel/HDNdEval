



bkgrd <- function(dat, mean = F){
  
  blks <- dat$no[dat$type == "BLK"]#save index numbers of blank measurements to blks
  
  out <- dat$rawdata #set up an output of the same size as the rawdata list containing data.frames
  

  for(i in 1:(length(blks)-1)){ #go through all blank values except the last one
    
    blkID <- blks[i] #take the i-th blank
    
    if(is.element("blanks", names(dat))){ #if blanks were already calculated
      blkMean <- dat$blanks[blkID,2:9] #take the already measured blanks
    } else {
      blkMean <- apply(X = dat$rawdata[[blkID]][,3:10], MARGIN = 2,FUN = mean, na.rm=T) #take the average of all blank measurements with the ID of blkID
    }

    for(j in (blks[i]+1):(blks[i+1]-1)){ #all samples between this and the next blank
      for(col in names(blkMean)){ #go through all columns of the averaged blank
        out[[j]][,col] <- out[[j]][,col] - blkMean[,col] #... and substract this from the measurement that was done after the blank measurement
      }
    }
    
  }
  
  out[dat$type == "BLK"] <- NA #the list of background corrected data should contain NA for a blank measurement

  return(out) # return the resulting list out
}

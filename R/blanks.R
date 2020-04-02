



blanks <- function(dat){
  blks <- dat$no[dat$type == "BLK"]
  
  out <- data.frame(matrix(NA, ncol = dim(dat$rawdata[[1]])[2], nrow = length(dat$type)))
  names(out) <- names(dat$rawdata[[1]])
  
  for(i in 1:(length(blks))){
    blkID <- blks[i]
    out[blkID,3:10] <- apply(X = apply(X = data$rawdata[[blkID]][,3:10], 
                                       MARGIN = 2,
                                       FUN = clear.outliers, use.quartile = F, a = 2, fill.gaps = F),
                             MARGIN = 2,
                             FUN = mean, na.rm = T)
  }
  out <- out[,-2]
  out$Cycle <- dat$no
  return(out)
}

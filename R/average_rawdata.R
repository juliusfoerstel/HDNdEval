

average.rawdata <- function(dat, clear.outliers = T){
  
  
  
  out <- data.frame(matrix(NA, ncol = dim(dat$rawdata[[1]])[2], nrow = length(dat$type)))
  names(out) <- names(dat$rawdata[[1]])
  
  for(i in dat$no){
    if(clear.outliers){
      out[i,2] <- as.character(data$rawdata[[i]][1,2])
      out[i,3:10] <- apply(X = apply(X = data$rawdata[[i]][,3:10], 
                                     MARGIN = 2,
                                     FUN = clear.outliers, use.quartile = F, a = 2, fill.gaps = F),
                           MARGIN = 2,
                           FUN = mean, na.rm = T)
    } else {
      out[i,3:10] <- apply(X = data$rawdata[[i]][,3:10],
                           MARGIN = 2,
                           FUN = mean, na.rm = T)
    }

  }
  out$Cycle <- dat$no
  return(out)
}

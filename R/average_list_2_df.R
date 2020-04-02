

average_list2df <- function(dat, list = "rawdata", clear.outliers = T, sd = T){
  
  
  maxcol <- dim(dat[[list]][[2]])[2]
  out <- data.frame(matrix(NA, ncol = maxcol, nrow = length(dat$type)))
  names(out) <- names(dat[[list]][[2]])
  
  sdout <- data.frame(matrix(NA, ncol = maxcol, nrow = length(dat$type)))
  names(sdout) <- paste0(names(dat[[list]][[2]]),".sd")
  
  maxcol <- dim(dat[[list]][[2]])[2]
  
  for(i in dat$no){
    if(!is.na(dat[[list]][[i]])){
      if(clear.outliers){
        out[i,2] <- as.character(data[[list]][[i]][1,2])
        out[i,3:maxcol] <- apply(X = apply(X = data[[list]][[i]][,3:maxcol], 
                                       MARGIN = 2,
                                       FUN = clear.outliers, use.quartile = F, a = 2, fill.gaps = F),
                             MARGIN = 2,
                             FUN = mean, na.rm = T)
        
        sdout[i,2] <- as.character(data[[list]][[i]][1,2])
        sdout[i,3:maxcol] <- apply(X = apply(X = data[[list]][[i]][,3:maxcol], 
                                         MARGIN = 2,
                                         FUN = clear.outliers, use.quartile = F, a = 2, fill.gaps = F),
                               MARGIN = 2,
                               FUN = sd, na.rm = T)/sqrt(dim(data[[list]][[i]][,3:maxcol])[1])
      } else {
        out[i,2] <- as.character(data[[list]][[i]][1,2])
        out[i,3:maxcol] <- apply(X = data[[list]][[i]][,3:maxcol],
                             MARGIN = 2,
                             FUN = mean, na.rm = T)
        sdout[i,2] <- as.character(data[[list]][[i]][1,2])
        sdout[i,3:maxcol] <- apply(X = data[[list]][[i]][,3:maxcol],
                               MARGIN = 2,
                               FUN = sd, na.rm = T)/sqrt(dim(data[[list]][[i]][,3:maxcol])[1])
      }
    }
    
  }
  out$Cycle <- dat$no
  names(out)[1] <- "Measurement"
  both <- cbind(out,sdout)[-dim(dat[[list]][[2]])[2]-c(1,2)]
  both  <- both[!is.na(both$Time),]
  return(both)
}

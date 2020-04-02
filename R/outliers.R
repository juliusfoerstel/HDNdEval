#' Clear Outliers
#'
#' This function deletes the entries which deviate more than a specific sigma range from the mean.
#' @param x Input data
#'        a Sigma range
#'        use.quartile Boolean if quartile range (TRUE,default) or standard deviation (FALSE) should be used.
#'        fill.gaps Boolean if gaps should be filled with the mean value of neighbouring data points
#' @keywords outlier, sigma, range
#' @export
#' @examples
#' clear.outliers(x = data)
#' clear.outliers(x = data, a = 2, use.quartile = T)


clear.outliers <- function(x, a=1.5, use.quartile = T, fill.gaps = F){
  
  if(use.quartile){ # use.quartile is true
    # all values that vary less than or equal to the interquartile range times parameter a 
    # from the median value are valid:
    valid <-  abs(x-median(x,na.rm = T)) <= a*IQR(x,na.rm = T) 
  }else{
    # all values that vary less than or equal to the standard deviation times parameter a 
    # from the mean value are valid:
    valid <-  abs(x-mean(x,na.rm = T)) <= a*sd(x,na.rm = T)
  }
  x[!valid] <- NA # set all non-valid values to NA
  
  
  if(fill.gaps){ # this is only executed if you do not want the output containing NA
    pos <- 1:length(x)
    pos <- pos[is.na(x)]
    
    for(i in pos){
      o <- 1
      while(is.element(el = i+o, set = pos)){
        o <- o+1
      }
      u <- 1
      while(is.element(el = i-u, set = pos)){
        u <- u+1
      }
      d <- u+o
      res <- x[i-u]+ u/d * (x[i+o]-x[i-u])
      if(length(res)==0 || is.na(res)){
        x[i] = 0
      }else{
        x[i] <- res
      }
    }
  }
  
  return(x)
}

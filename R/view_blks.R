

#View blanks
view_blks <- function(dat,column){
  for(i in dat$no[data$type=="BLK"]){
    if(i==1){
      plot(x = 1:15,
           y = dat$rawdata[[i]][,column], pch = 20, 
           ylim = c(minList(dat$rawdata[dat$type=="BLK"],col = column),maxList(dat$rawdata[dat$type=="BLK"],col = column)), 
           col = rgb(i/max(dat$no),0,1-i/max(dat$no)),
           ylab = "Signal [V]", xlab = "Cycle", main = paste(column,"Blank Signals"))
      lines(dat$rawdata[[i]][,column], col = rgb(i/max(dat$no),0,1-i/max(dat$no)))
    } else {
      points(dat$rawdata[[i]][,column], col = rgb(i/max(dat$no),0,1-i/max(dat$no)), pch = 20)
    }
  }
  
  lines(dat$rawdata[[i]][,column], col = rgb(i/max(dat$no),0,1-i/max(dat$no)))
  legend("topright", col = c(4,2,4,2), legend = c("early", "late", "first", "last"), pch = c(20,20,NA,NA), lwd = c(NA,NA,1,1))
}


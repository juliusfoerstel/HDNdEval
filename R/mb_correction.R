

#' methods 
#'     exponential law   -> "exp"
#'     power law         -> "pow"
#'     linear law        -> "lin"


mb_df <- function(df, method = "exp", ref.ratio = c("146Nd","144Nd")){
  out <- df
  for(i in 1:nrow(df)){
    out[i,] <- mb_linecorrection(data_line = df[i,], method = method, ref.ratio = ref.ratio)
  }
  return(out)
}

mb_linecorrection <- function(data_line, method = "exp", ref.ratio = c("146Nd","144Nd")){
  massLib <- data.frame(isotopes = c("140Ce", "142Nd","143Nd","144Nd","145Nd","146Nd","147Sm","148Nd"),
                        mass = c(140,141.9077233,142.9098143,143.9100873,144.9125736,145.9131169,147,147.916893),
                        element = as.factor(c("Ce", "Nd","Nd","Nd","Nd","Nd","Sm","Nd"))) 
  ratioLib <- data.frame(num =   c("146Nd"),
                         den =   c("144Nd"),
                         ratio = c(0.7219))

  out <- data_line
    meas_ref_ratio <- data_line[,ref.ratio[1]]/data_line[,ref.ratio[2]]
    switch(method,
           #exponential law
           exp = {
             betas <- (log(meas_ref_ratio/ratioLib$ratio[ref.ratio[1] == ratioLib$num & ref.ratio[2] == ratioLib$den])
                       / log(massLib$mass[ref.ratio[1] == massLib$isotopes]/massLib$mass[ref.ratio[2] == massLib$isotopes]))
             corr <- function(measR,m1,m2,beta = betas){
               return(measR * (m2/m1) ^ beta)
             }
           },
           #power law
           pow = {
             gs <- (meas_ref_ratio/ratioLib$ratio[ref.ratio[1] == ratioLib$num & ref.ratio[2] == ratioLib$den])^(1/(massLib$mass[ref.ratio[1] == massLib$isotopes] - massLib$mass[ref.ratio[2] == massLib$isotopes]))
             corr <- function(measR,m1,m2,g = gs){
               return(measR * g ^ (m2-m1))
             }
           },
           #linear law
           lin = {
             mus <- (meas_ref_ratio/ratioLib$ratio[ref.ratio[1] == ratioLib$num & ref.ratio[2] == ratioLib$den] - 1) / (massLib$mass[ref.ratio[1] == massLib$isotopes] - massLib$mass[ref.ratio[2] == massLib$isotopes])
             corr <- function(measR,m1,m2,mu = mus){
               return(measR / (1 + mu*(m1-m2) ))
             }
           }, 
           stop("\"",method,"\" is not a valid method!")
    )
    
    
    
    #fixed denominator values
    den_vals <- data_line[,ref.ratio[2]]
    M2 <- massLib$mass[massLib$isotopes == ref.ratio[2]]
    
    for(j in massLib$isotopes){
      if(is.element(j,names(data_line))){
        this <- data_line[,j]
        M1 <- massLib$mass[massLib$isotopes == j]
      } else {
        next
      }
      meas_ratio <- this/den_vals
      out[,j] <- corr(measR = meas_ratio,m1 = M1, m2 = M2) * den_vals
    }
  
  return(out)
}

mb_correction <- function(dat, method = "exp", ref.ratio = c("146Nd","144Nd")){
  massLib <- data.frame(isotopes = c("140Ce", "142Nd","143Nd","144Nd","145Nd","146Nd","147Sm","148Nd"),
                        mass = c(140,141.9077233,142.9098143,143.9100873,144.9125736,145.9131169,147,147.916893),
                        element = as.factor(c("Ce", "Nd","Nd","Nd","Nd","Nd","Sm","Nd"))) 
  ratioLib <- data.frame(num =   c("146Nd"),
                         den =   c("144Nd"),
                         ratio = c(0.7219))
  index <- dat$no[dat$type != "BLK"]
  
  out <- dat$bg
  for(i in index){
    meas_ref_ratio <- dat$bg[[i]][,ref.ratio[1]]/dat$bg[[i]][,ref.ratio[2]]
    switch(method,
           #exponential law
           exp = {
             betas <- (log(meas_ref_ratio/ratioLib$ratio[ref.ratio[1] == ratioLib$num & ref.ratio[2] == ratioLib$den])
                       / log(massLib$mass[ref.ratio[1] == massLib$isotopes]/massLib$mass[ref.ratio[2] == massLib$isotopes]))
             corr <- function(measR,m1,m2,beta = betas){
               return(measR * (m2/m1) ^ beta)
             }
           },
           #power law
           pow = {
             gs <- (meas_ref_ratio/ratioLib$ratio[ref.ratio[1] == ratioLib$num & ref.ratio[2] == ratioLib$den])^(1/(massLib$mass[ref.ratio[1] == massLib$isotopes] - massLib$mass[ref.ratio[2] == massLib$isotopes]))
             corr <- function(measR,m1,m2,g = gs){
               return(measR * g ^ (m2-m1))
             }
           },
           #linear law
           lin = {
             mus <- (meas_ref_ratio/ratioLib$ratio[ref.ratio[1] == ratioLib$num & ref.ratio[2] == ratioLib$den] - 1) / (massLib$mass[ref.ratio[1] == massLib$isotopes] - massLib$mass[ref.ratio[2] == massLib$isotopes])
             corr <- function(measR,m1,m2,mu = mus){
               return(measR / (1 + mu*(m1-m2) ))
             }
           }, 
           stop("\"",method,"\" is not a valid method!")
    )
    
    
    
    #fixed denominator values
    den_vals <- dat$bg[[i]][,ref.ratio[2]]
    M2 <- massLib$mass[massLib$isotopes == ref.ratio[2]]
    
    for(j in massLib$isotopes){
      if(is.element(j,names(dat$bg[[i]]))){
        this <- dat$bg[[i]][,j]
        M1 <- massLib$mass[massLib$isotopes == j]
        
      } else {
        next
      }
      meas_ratio <- this/den_vals
      out[[i]][,j] <- corr(measR = meas_ratio,m1 = M1, m2 = M2) * den_vals
      }
  }
  return(out)
}
















#' These function return the maximum value contained in a specific column in list of dataframes

maxList <- function(l,col,...){
  inter <- c() #intermediate storage vector
  for(i in 1:length(l)){ #first go through all lists
    inter[i] <- max(l[[i]][,col],...) #save the max of each lists column on position i
  }
  return(max(inter,...)) #return the maximum of the intermediate values, i.e. the maximum of all lists
}


minList <- function(l,col,...){
  inter <- c()#intermediate storage vector
  for(i in 1:length(l)){#first go through all lists
    inter[i] <- min(l[[i]][,col],...) #save the min of each lists column on position i
  }
  return(min(inter,...))#return the minimum of the intermediate values, i.e. the maximum of all lists
}


meanList <- function(l,col,...){
  inter <- c()#intermediate storage vector
  for(i in 1:length(l)){#first go through all lists
    inter[i] <- mean(l[[i]][,col],...) #save the mean of each lists column on position i
  }
  return(mean(inter,...))#return the minimum of the intermediate values, i.e. the maximum of all lists
}

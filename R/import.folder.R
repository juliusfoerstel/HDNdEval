


import.files <- function(){

  files <-  if(grepl(pattern = "Linux", x = Sys.info()[1])){
    library(tcltk)
    paths <- tk_choose.files(filters = matrix(c("exp-Files","*.exp"), nrow = 1, ncol = 2)) #choose the files to evaluate. the prompt is not nice, but it works on all systems (I hope)
  } else {
    files <- choose.files(filters = matrix(c("exp-Files","*.exp"), nrow = 1, ncol = 2))
  }#list of all .exp files



  sampleID <-c()
  sampleType <- c()
  data <- list()

  c <- 0 #set counter to 0 before starting the import
  ndots <- 100 # set length of progress bar
  cat("importing",paste0(rep("_",ndots-9),collapse = ""),"\n",sep = "") #print the top part of progress bar

  for(i in 1:length(paths)){ #go through every individual path
    f <- paths[i] # current path to file
    ls <- readLines(con = f,n = 15 ) #read the first 15 lines of the file
    sampleID[i] <- substr(ls[4],12,regexpr(pattern = "\t", ls[4])[1]-1) #in Line 4 is the sample name
    sampleType[i] <- substr(ls[15],13,regexpr(pattern = "\t", ls[15])[1]-1 ) #in Line 15 is the sample type

    data[[i]] <- read.exp(file = f)[,1:10] #read the exp file and take the first 10 columns

    #progress bar
    while(round(i/length(paths)*ndots) > c){ #as long as c is smaller than the current progression
      cat(">", sep = "") #print ">"
      c <- c+1 #add 1 to c
    }
  }

  #' output is going to be a list of
  #'    numbers of files index (integer vector)
  #'    sample ID given in the sequence editor (string vector)
  #'    sample type (i.e. Standard, Sample or Blank) (factor vector)
  #'    list of raw data values (list of dataframes)
  out <- list(no = 1:length(paths), id = sampleID, type = as.factor(sampleType), rawdata = data)

  return(out)
}



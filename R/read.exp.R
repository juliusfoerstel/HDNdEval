#' Function to read exp-files
#'
#' This function allows you to load data from exp-files and put them into a data.frame.
#' @keywords read
#' @param file path to an existing exp-file.
#' @export
#' @examples
#' read.exp()


read.exp <- function(file){

  lines <- readLines(file) #read the file line by line


  start.reading <- seq(1,length(lines),by = 1)[grepl(pattern = "Cycle",x = lines)] #find starting line
  options(warn=-1) #turn warnings off
  stop.reading <- tail(seq(start.reading,length(lines),by = 1)[!is.na(as.integer(substring(lines[start.reading:length(lines)],1,1)))],n=1) #find end line
  options(warn=0) #turn warnings back on


  #There might be empty lines, that cause a problem (mainly Laser measurements)
  if(max(grepl(pattern = '\t\t\t',x = lines[start.reading:stop.reading]))){
    for(line in start.reading:stop.reading){
      if(grepl(pattern = '\t\t\t',x = lines[line])){  #look for empty lines
        stop.reading <- line
        break
      }
    }
  }


  cycles <- stop.reading-start.reading #calculate the number of cycles measured

  data <- read.table(file,skip = start.reading ,header = F, nrows = cycles) #read only the data that is relevant
  names(data) <- c("Cycle","Time","140Ce",	"142Nd",	"143Nd",	"144Nd",	"145Nd",	"146Nd",	"147Sm",	"148Nd")
  
  return(data)
}

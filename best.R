best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(state %in% unique(data[,7])==FALSE) {(stop("invalid state"))}
  if(outcome!='heart attack' & outcome!='pneumonia' & outcome!='heart failure') {(stop("invalid outcome"))}
  
  x<-subset(data,data[,7]==state)
  
  if(outcome=='heart attack'){
  y<-suppressWarnings(as.character(format(min(as.numeric(subset(x[,11],!is.na(as.numeric(x[,11]))))),nsmall=1)))
  answer<-sort(subset(x[,2],x[,11]==y))
    }else if(outcome=='pneumonia'){
    y<-suppressWarnings(as.character(format(min(as.numeric(subset(x[,23],!is.na(as.numeric(x[,23]))))),nsmall=1)))
    answer<-sort(subset(x[,2],x[,23]==y))
      }else {
      y<-suppressWarnings(as.character(format(min(as.numeric(subset(x[,17],!is.na(as.numeric(x[,17]))))),nsmall=1)))
      answer<-sort(subset(x[,2],x[,17]==y))}
  
  answer[1]
  
}
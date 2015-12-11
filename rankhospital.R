rankhospital <- function(state, outcome, num="best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(state %in% unique(data[,7])==FALSE) {(stop("invalid state"))}
  if(outcome!='heart attack' & outcome!='pneumonia' & outcome!='heart failure') {(stop("invalid outcome"))}
  if(is.character(num)){
    if(num!='best'&num!='worst'){stop("NA")}
  }else{if(num>suppressWarnings(nrow(subset(data,data[,7]==state&!is.na(as.numeric(data[,11])))))|
            num>suppressWarnings(nrow(subset(data,data[,7]==state&!is.na(as.numeric(data[,23])))))|
              num>suppressWarnings(nrow(subset(data,data[,7]==state&!is.na(as.numeric(data[,17])))))){"NA"}}
  
  x<-subset(data,data[,7]==state)
  
  if(num=='best'){
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
  }else if(num=='worst'){
    if(outcome=='heart attack'){
      y<-suppressWarnings(as.character(format(max(as.numeric(subset(x[,11],!is.na(as.numeric(x[,11]))))),nsmall=1)))
      answer<-sort(subset(x[,2],x[,11]==y))
    }else if(outcome=='pneumonia'){
      y<-suppressWarnings(as.character(format(max(as.numeric(subset(x[,23],!is.na(as.numeric(x[,23]))))),nsmall=1)))
      answer<-sort(subset(x[,2],x[,23]==y))
    }else {
      y<-suppressWarnings(as.character(format(max(as.numeric(subset(x[,17],!is.na(as.numeric(x[,17]))))),nsmall=1)))
      answer<-sort(subset(x[,2],x[,17]==y))}
    answer[1]
  }else{
    if(outcome=='heart attack'){
      x[,11]<-suppressWarnings(as.numeric(x[,11]))
      answer<-x[order(x[,11],x[,2]),]
      answer[num,2]
    }else if(outcome=='pneumonia'){
      x[,23]<-suppressWarnings(as.numeric(x[,23]))
      answer<-x[order(x[,23],x[,2]),]
      answer[num,2]
    }else {
      x[,17]<-suppressWarnings(as.numeric(x[,17]))
      answer<-x[order(x[,17],x[,2]),]
      answer[num,2]
  }}}



get_ratio <- function(tp, y) {
  
  a <- tp[which(y==0)]
  b <- tp[which(y==1)]
  
  ratio <- round((a/b), 4)
  
  return(ratio)
  
}

get_pc_change <- function(tp, y) {
    
  
  a <- tp[which(y==0 | y==7)]
  b <- tp[which(y==1 | y==14)]
  
  ratio <- round((a/b-1)*100, 1)
  
  return(ratio)
  
}

se <- function(x) sd(x) / sqrt(length(x))
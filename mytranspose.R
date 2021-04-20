mytranspose <- function(x) {
  # for matrices
  if (class(x) == 'matrix'){
    if ((nrow(x) == 0) || (ncol(x) == 0)) {
      y = matrix(nrow=0, ncol=0)
    }
    else {
      y = matrix(1, nrow=ncol(x), ncol = nrow(x))
      for(i in 1:nrow(x)) {
        for(j in 1:ncol(x)) {
          y[j,i] <- x[i,j]
        }
      }
    }
  }
  # for vectors
  else if((class(x) == 'numeric') || (class(x) == 'character')){
    y = matrix(1, nrow=1, ncol = length(x))
    for (i in 1:length(x)){
      y[i] = x[i]
    }
  }
  # for booleans
  else if (class(x) == 'logical'){
    y=c()
    for (i in 1:length(x)){
      y=c(y,x[i])
    }
    y=matrix(y, nrow=1, ncol = length(x))
  }
  # for dataframes
  else if(class(x) == 'data.frame'){
    y = matrix(1, nrow=ncol(x), ncol = nrow(x))
    for(i in 1:nrow(x)) {
      for(j in 1:ncol(x)) {
        y[j,i] = as.matrix(x)[i,j]
      }
    }
    rownames(y) = colnames(x)
    # for missing values
  }
  else if ((class(x)) == 'NULL'){
    y=c()
  }
  else if (is.na(x)){
    y = NA
  }
  return(y)
}
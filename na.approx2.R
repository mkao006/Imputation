########################################################################
## Title: Function to recover na.approx if less than two points are
##        available.
## Date: 2013-05-09
########################################################################

na.approx2 = function(x, na.rm = FALSE){
  if(length(na.omit(x)) < 2){
    tmp = x
  } else {
    tmp = na.approx(x, na.rm = na.rm)
  }
  c(tmp)
}

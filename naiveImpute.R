########################################################################
## Title: The naive imputation of linear interpolation followed by
##        last/first observation carried over.
## Date: 2013-05-29
########################################################################

naiveImp = function(x){
  nobserved = length(na.omit(x))
  n = length(x)
  type = ifelse(nobserved == 0, "none",
    ifelse(nobserved == 1, "repeat", "naive"))
  switch(type,
         "none" = {tmp = rep(NA, n)},
         "repeat" = {tmp = rep(na.omit(x), n)},
         "naive" = {tmp = na.locf(na.locf(na.approx(x, na.rm = FALSE),
            na.rm = FALSE), na.rm = FALSE, fromLast = TRUE)}
         )
  as.numeric(tmp)
}


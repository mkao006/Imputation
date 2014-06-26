##' Naive imputation
##'
##' The naive imputation is simply linear interpolation with last
##' observation carried forawrd and backwards.
##'
##' @param x The numeric vector to be imputed.
##'
##' @export
##' 
naiveImputation = function(x){
  require(zoo)
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

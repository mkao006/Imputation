##' This function computes the naive imputation
##'
##' The function performs imputation by linear interpolation followed
##' by last observation carry forward and backwards.
##'
##' @param x The series to put imputed
##' @export
##'
##' @examples
##' rand = rnorm(20)
##' (rand[sample(1:length(rand), size = 10)] = NA)
##' naiveImp(rand)
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


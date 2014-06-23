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

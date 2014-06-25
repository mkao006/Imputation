##' Function to determin whether countries contains any information.
containInfo = function(data, productionSymb, productionValue){
    ifelse(all(data[, productionSymb, with = FALSE] == "M") |
           sum(data[, productionValue, with = FALSE], na.rm = TRUE) == 0,
           FALSE, TRUE)
}

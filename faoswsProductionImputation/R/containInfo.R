##' Function to determine whether the data contains any information.
##'
##' If the data contains only a missing value or zero then it is
##' marked as no information.
##'
##' @param value A numeric vector to be checked
##' @param flag The character vector representing the observation flag
##' corresponding to the value.
##' @param naFlag The observation flag which corresponds to missing
##' value.
##'
##' @export

containInfo = function (value, flag, processingParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredProductionData") || !ensuredProductionData)
        ensureProductionInputs(data = data,
                               processingParameters = processingParameters)
    stopifnot(length(value) == length(flag))
    stopifnot(is(value, "numeric"))
    stopifnot(is(flag, "character"))
    
    ifelse(all(flag == processingParameters$naFlag) |
               sum(value, na.rm = TRUE) == 0,
           FALSE, TRUE)
}

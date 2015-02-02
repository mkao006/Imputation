##' The function removes conflicting zeroes between production and
##' area harvested.
##'
##' @param data The data table objest
##' @param processingParameters A list of the parameters for the production
##' processing algorithms.  See defaultProductionParameters() for a starting
##' point.
##'
##' @return No value is returned.  However, the object "data" which was passed
##' to this function is modified.
##' 
##' @examples data = okrapd[1:10,]
##' data[,areaHarvestedValue := c(rep(0, 5), rep(100, 5))]
##' data[,productionValue := c(0, 0, 100, 100, 100, 0, 0, 100, 100, 100)]
##' removeZeroConflict(columnNames = defaultColumnNames(), data = data)
##' data[,.(areaHarvestedValue, areaHarvestedFlag, productionValue,
##'     productionFlag, yieldValue, yieldFlag)]
##'
##' @export
##' 

removeZeroConflict = function(data, processingParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredProductionData") || !ensuredProductionData)
        ensureProductionInputs(data = data,
                               processingParameters = processingParameters)
    
    ### Abbreviate "processingParameters" with "p" to reduce clutter
    p = processingParameters
    
    ### Identify points where area = 0 and production != 0 (or vice versa)
    filter1 = data[get(p$productionValue) == 0 &
                   get(p$areaHarvestedValue) != 0,]
    filter2 = data[get(p$productionValue) != 0 &
                   get(p$areaHarvestedValue) == 0,]
    
    ### For problematic observations, set both vals to NA and flags to missing
    data[filter1 | filter2, c(p$areaHarvestedValue, p$yieldValue,
                              p$areaHarvestObservationFlag,
                              p$yieldObservationFlag) :=
             as.list(rep(c(NA, processingParameters$naFlag), each = 2))]
}

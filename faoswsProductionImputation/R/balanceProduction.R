##' Function to compute production when new area harvested and yield
##' are given.
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point. If
##' NULL, the parameters should have already been assigned (otherwise an error
##' will occur).
##'
##' @export
##' 

balanceProduction = function(data, imputationParameters = NULL){

    ### Data Quality Checks
    if(!exists("parametersAssigned") || !parametersAssigned){
        stopifnot(!is.null(imputationParameters))
        assignParameters(imputationParameters)
    }
    if(!ensuredData)
        ensureData(data = data)
    if(!ensuredFlagTable)
        ensureFlagTable(flagTable = flagTable, data = data)
    
    origName = c(productionValue, productionObservationFlag,
        productionMethodFlag, areaHarvestedValue,
        areaHarvestedObservationFlag, 
        yieldValue, yieldObservationFlag)
    tmpName = c("pValue", "pObsFlag", "pMetFlag", "aValue", "aObsFlag",
        "yValue", "yObsFlag")
    setnames(data, old = origName, new = tmpName)
    
    # If data$pMetFlag is all NA's, it may be logical.  This could issue a
    # warning later but it's not really a problem.  To avoid confusion, 
    # coerce to a character here.
    if(is(data$pMetFlag, "logical"))
        data[, pMetFlag := as.character(data$pMetFlag)]
    data[!is.na(aValue) & is.na(pValue) & !is.na(yValue),
         c("pValue", "pObsFlag", "pMetFlag") :=
         list(aValue * yValue,
              aggregateObservationFlag(aObsFlag, yObsFlag,
                                       flagTable = flagTable),
              newMethodFlag)
         ]
    setnames(data, old = tmpName, new = origName)
}
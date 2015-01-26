##' Function to compute production when new area harvested and yield
##' are given.
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##'
##' @export
##' 

balanceProduction = function(data, imputationParameters, processingParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    
    origName = c(processingParameters$productionValue,
                 processingParameters$productionObservationFlag,
                 processingParameters$productionMethodFlag,
                 processingParameters$areaHarvestedValue,
                 processingParameters$areaHarvestedObservationFlag,
                 processingParameters$yieldValue,
                 processingParameters$yieldObservationFlag)
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
                                       flagTable = imputationParameters$flagTable),
              imputationParameters$newMethodFlag)
         ]
    setnames(data, old = tmpName, new = origName)
}

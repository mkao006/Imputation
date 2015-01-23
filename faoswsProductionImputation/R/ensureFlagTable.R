##' Ensure Flag Table
##' 
##' This function performs several checks to ensure that the provided flagTable
##' is valid.  First, it verifies that the column names are
##' "flagObservationStatus" and "flagObservationWeights".  Second, it verifies
##' that all observation flags in data are symbols in the flag table.
##'
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##'
##' @return No value is returned.  However, an error is raised if the flagTable
##' fails the checks done by this function.
##'
##' @export
##' 

ensureFlagTable = function(flagTable, data){
    
    ### Before running tests, ensure all necessary variables exist
    requiredVariables = c("ensuredFlagTable", "parametersAssigned",
                          "ensuredData", "imputationFlagColumn")
    missingVariables = requiredVariables[!sapply(requiredVariables, exists)]
    if(length(missingVariables) > 0)
        stop("Data cannot be ensured without the existence of these variables:\n\t",
             paste(missingVariables, collapse = "\n\t"),
             "\nMaybe try running assignParameters with an argument of either ",
             "defaultImputationParameters() or defaultProcessingParameters()?")
    
    ## Check data is valid
    if(!ensuredData)
        ensureImputationData(data)
    
    ## Check structure of flagTable
    stopifnot(colnames(flagTable) ==
                  c("flagObservationStatus", "flagObservationWeights"))
    stopifnot(is(flagTable, "data.frame"))

    ## Check that all flags are in the flagTable:
    flags = data[[imputationFlagColumn]]
    flags = unique(flags)
    missingFlags = flags[!flags %in% flagTable$flagObservationStatus]
    if(length(missingFlags) > 0){
        stop("Some observation flags are not in the flag table!  Missing:\n",
            paste0("'", missingFlags, "'", collapse="\n"))
    }
    
    ## Ensure flags of flagTable are valid
    stopifnot(checkObservationFlag(flagTable[["flagObservationStatus"]]))
    
    ### Globally assign ensuredFlagTable so flagTable will not be ensured again
    reassignGlobalVariable("ensuredFlagTable", TRUE)
}
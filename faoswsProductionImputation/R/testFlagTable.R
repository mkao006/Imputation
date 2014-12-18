##' Test Flag Table
##' 
##' This function performs several checks to ensure that the provided flagTable
##' is valid.  First, it verifies that the column names are
##' "flagObservationStatus" and "flagObservationWeights".  Second, it verifies
##' that all observation flags in data are symbols in the flag table.
##'
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##' @param columnNames See this argument at ?imputeProductionDomain.
##'
##' @return No value is returned.  However, an error is raised if the flagTable
##' fails the checks done by this function.
##'
##' @export
##' 

testFlagTable = function(flagTable, data, columnNames){
    stopifnot( all( colnames(flagTable) ==
        c("flagObservationStatus", "flagObservationWeights") ) )
    assignColumnNames(columnNames = columnNames, data = data,
        environment = environment() )
    # Check that all flags are in the flagTable:
    flags = data[[productionObservationFlag]]
    flags = c(flags, data[[areaHarvestedObservationFlag]])
    flags = c(flags, data[[yieldObservationFlag]])
    flags = unique(flags)
    missingFlags = flags[!flags %in% flagTable$flagObservationStatus]
    if( length(missingFlags) > 0 ){
        stop(paste("Some observation flags are not in the flag table!  Missing:\n",
            paste0("'", missingFlags, "'", collapse="\n ") ) )
    }
}
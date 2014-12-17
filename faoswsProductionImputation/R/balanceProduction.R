##' Function to compute production when new area harvested and yield
##' are given.
##'
##' @param columnNames See this argument at ?imputeProductionDomain.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##'
##' @export
##' 

balanceProduction = function(columnNames,
    newMethodFlag, flagTable = faoswsFlagTable, data){

    ### Ensure inputs are as expected:
    stopifnot( is(data, "data.table") )
    testColumnNames( columnNames = columnNames, data = data )

    assignColumnNames( columnNames = columnNames, data = data,
        environment = environment() )
    # Check that all flags are in the flagTable:
    flags = data[,get(productionObservationFlag)]
    flags = c(flags, data[,get(areaHarvestedObservationFlag)])
    flags = c(flags, data[,get(yieldObservationFlag)])
    flags = unique(flags)
    missingFlags = flags[!flags %in% flagTable$flagObservationStatus]
    if( length(missingFlags) > 0 ){
        stop(paste("Some observation flags are not in the flag table!  Missing:\n",
            paste0("'", missingFlags, "'", collapse="\n ") ) )
    }
    
    origName = c(productionValue, productionObservationFlag,
        productionMethodFlag, areaHarvestedValue,
        areaHarvestedObservationFlag, 
        yieldValue, yieldObservationFlag)
    tmpName = c("pValue", "pObsFlag", "pMetFlag", "aValue", "aObsFlag",
        "yValue", "yObsFlag")
    setnames(data, old = origName, new = tmpName)
    
    data[!is.na(aValue) & is.na(pValue) & !is.na(yValue),
         c("pValue", "pObsFlag", "pMetFlag") :=
         list(aValue * yValue,
              aggregateObservationFlag(aObsFlag, yObsFlag,
                                       flagTable = flagTable),
              newMethodFlag)
         ]
    setnames(data, old = tmpName, new = origName)
}

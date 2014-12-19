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

    ### Ensure inputs are as expected (and assign columnNames variables)
    stopifnot( is(data, "data.table") )
    testColumnNames( columnNames = columnNames, data = data )
    assignColumnNames( columnNames = columnNames, environment = environment() )
    testFlagTable( flagTable = flagTable, data = data,
        columnNames = columnNames )
    
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
    if( is( data$pMetFlag, "logical" ) )
        data[,pMetFlag := as.character( data$pMetFlag )]
    data[!is.na(aValue) & is.na(pValue) & !is.na(yValue),
         c("pValue", "pObsFlag", "pMetFlag") :=
         list(aValue * yValue,
              aggregateObservationFlag(aObsFlag, yObsFlag,
                                       flagTable = flagTable),
              newMethodFlag)
         ]
    setnames(data, old = tmpName, new = origName)
}
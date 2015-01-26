##' Default Processing Parameters
##' 
##' This function can be used to generate the input parameters for the
##' data pre-processing code.  This is a good way to get a list of the required
##' parameters and then modify parameters to match your particular
##' configuration.
##' 
##' @return Returns a list of the default parameters used in the data
##' pre-processing algorithm.
##' 
##' @export
##' 

defaultProcessingParameters = function(){
    list(productionValue = "productionValue",
         productionObservationFlag = "productionFlag",
         productionMethodFlag = "productionFlag2",
         yieldValue = "yieldValue",
         yieldObservationFlag = "yieldFlag",
         yieldMethodFlag = "yieldFlag2",
         areaHarvestedValue = "areaHarvestedValue",
         areaHarvestedObservationFlag = "areaHarvestedFlag",
         areaHarvestedMethodFlag = "areaHarvestedFlag2",
         yearValue = "year",
         byKey = "areaCode",
         removePriorImputation = TRUE,
         removeConflictValues = TRUE,
         imputedFlag = "E",
         naFlag = "M")
}
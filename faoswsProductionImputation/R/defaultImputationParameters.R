##' Default Imputation Parameters
##' 
##' This function can be used to generate the input parameters for the
##' ensemble imputation code.  This is a good way to get a list of the required
##' parameters and then modify parameters to match your particular
##' configuration.
##' 
##' @return Returns a list of the default parameters used in the ensemble
##' imputation algorithms.
##' 
##' @note Currently, three parameters are not directly used for the
##' pre-processing: ensuredData, ensuredFlagTable, and parametersAssigned.
##' The first two of these will become global variables, and they will be
##' changed to TRUE once the objects data and flagTable pass a series of
##' checks.  Once they are true, the checks will not be ran again (unless a
##' function causes them to be set to FALSE and ensureData() or
##' ensureFlagTable() is called again).  parametersAssigned indicates that the
##' parameters in this function have already been assigned to the global
##' environment and that they do not need to be assigned again.
##' 
##' @export
##' 

defaultImputationParameters = function(){
    list(
#          productionValue = "productionValue",
#          productionObservationFlag = "productionFlag",
#          productionMethodFlag = "productionFlag2",
#          yieldValue = "yieldValue",
#          yieldObservationFlag = "yieldFlag",
#          yieldMethodFlag = "yieldFlag2",
#          areaHarvestedValue = "areaHarvestedValue",
#          areaHarvestedObservationFlag = "areaHarvestedFlag",
#          areaHarvestedMethodFlag = "areaHarvestedFlag2",
         yearValue = "timePointYears",
         byKey = "geographicAreaM49",
         variable = "production",
         ensembleModels = allDefaultModels(),
         restrictWeights = TRUE,
         maximumWeights = 0.7,
         plotImputation = TRUE,
         errorType = "loocv",
         errorFunction = function(x) mean(x^2),
         groupCount = 10,
         missingFlag = "M",
         imputationFlag = "I",
         newMethodFlag = "E",
         flagTable = faoswsFlagTable,
         ensuredData = FALSE,
         ensuredFlagTable = FALSE,
         parametersAssigned = FALSE)
}
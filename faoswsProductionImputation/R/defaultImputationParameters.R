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
         flagTable = faoswsFlagTable)
}
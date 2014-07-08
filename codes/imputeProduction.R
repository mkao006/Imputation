##' Function to impute production
##'
##' This is a wrapper of the ensemble imputation for the production
##' domain.
##'
##' @param data The data
##' @param productionVar The name of the production column
##' @param index The keys to split the data for performing the
##' imputation separately.
##'
##' @export
##' 
imputeProduction = function(data, productionVar, index){
    dataCopy = copy(data)
    setnames(dataCopy, old = productionVar, new = "productionVar")

    unlist(dataCopy[, ensembleImpute(productionVar), by = index]$V1)
}

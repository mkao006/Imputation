##' Function to impute production
##'
##' This is a wrapper of the ensemble imputation for the production
##' domain.
##'
##' @param data The data
##' @param productionVar The production statistics
##' @param index The keys to split the data for performing the
##' imputation separately.
##'
##' @export
##' 
imputeProduction = function(data, productionVar, index){
    data[eval(parse(text = paste0(productionVar, " := ensembleImpute(",
                        productionVar, ")"))),
         by = index]
    data
}

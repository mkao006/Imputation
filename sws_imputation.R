##' The function to perform imputation for area, production and yield
##' simultaneously.
##'
##' This function is the implementation which is illustrated in the
##' vignette to perform imputation for the three related time series
##' of FAO production domain.
##'
##' @param Data The data.frame or data.table containing the data.
##' @param area The column containing the area time series.
##' @param prod The column containing the production time series.
##' @param yield The column containing the yield time series.
##' @param country The column representing the country name, if in
##' code then convert to factor.
##' @param region The column representing the regional classification,
##' if in code then convert to factor.
##' @param year The column containing the time information.
##' @param n.iter The number of iteration for lmeIputation.
##' @param tol Tolerance, the stopping rule for the Likelihood.
##'
##' @seealso \code{\link{lmeImpute}}
##' @export


sws_imputation = function(data, area, prod, yield, country,
  region, year, n.iter = 1000, tol = 1e-8){

  dataCopy = copy(data.table(data))

  ## Initiate the yield formula
  yieldFormula = formula(paste0(yield, "~ 1"))

  splitData = splitNACountry(value = yield, country = country,
      data = dataCopy)
  nonEmptyYield = splitData$nonEmptyData
  
  ## Linear Mixed Model for yield
  yield.fit = lmerEMImpute(formula = yieldFormula,
      groupVariable = c(region, year), countryVar = country,
      data = nonEmptyYield, n.iter = n.iter, tol = tol)

  ## Impute yield
  imputedYield.dt = data.table(nonEmptyYield,
      groupedMean = yield.fit$groupedMean)
  imputedYield.dt[, imputedYield :=
           predict(yield.fit$model, newdata = imputedYield.dt)]

  if(NROW(splitData$emptyData) >= 1){
      final.dt = rbind(imputedYield.dt,
          data.table(splitData$emptyData, groupedMean = as.numeric(NA),
                     imputedYield = as.numeric(NA)))
  } else {
      final.dt = imputedYield.dt
  }
  
  ## Impute area and produciton if available
  final.dt[, eval(parse(text = paste0("imputedArea := ", area)))]
  final.dt[, eval(parse(text = paste0("imputedProd := ", prod)))]
  final.dt[is.na(imputedArea) &
                  !is.na(imputedProd) &
                  !is.na(imputedYield),
                  imputedArea := imputedProd/imputedYield]
  final.dt[is.na(imputedProd) &
                  !is.na(imputedArea) &
                  !is.na(imputedYield),
                  imputedProd := imputedArea * imputedYield]
  
  ## Impute area with naive imputation
  final.dt[, imputedArea := naiveImp(imputedArea),
                  by = country]

  ## Impute production where area and yield are available
  final.dt[is.na(imputedProd),
              imputedProd := imputedArea * imputedYield]

  ## Remove yield where area or produciton are zero
  final.dt[imputedProd == 0 | imputedArea == 0,
              imputedYield := as.numeric(NA)]
  
  list(imputed = final.dt, model = yield.fit)
}

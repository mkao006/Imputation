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


FAOProductionImpute = function(Data, area, prod, yield, country,
  region, year, n.iter = 1000, tol = 1e-8){

  dataCopy = data.table(Data)

  ## Initiate the formula
  fixed = formula(paste0(yield, " ~ 1"))
  random = formula(paste("~", year, "|", country))
  groupVar = c(region, year)


  ## Impute yield
  impYield.lst = lmeImpute(fixed = fixed, random = random,
    groupVar = groupVar, Data = dataCopy)

  impYield.dt = data.table(impYield.lst$imputed)
  print(paste("yield methodology: ", impYield.lst$method))
  impYield.dt[, yieldMethodology := impYield.lst$method]
  setnames(impYield.dt, old = "impValue", new = "imputedYield")
  
  ## Impute area and produciton if available
  impYield.dt[, eval(parse(text = paste0("imputedArea := ", area)))]
  impYield.dt[, eval(parse(text = paste0("imputedProd := ", prod)))]
  impYield.dt[is.na(imputedArea) & !is.na(imputedProd) &
              !is.na(imputedYield),
              imputedArea := imputedProd/imputedYield]
  impYield.dt[is.na(imputedProd) & !is.na(imputedArea) &
              !is.na(imputedYield),
           imputedProd := imputedArea * imputedYield]

  ## Impute area with naive imputation
  impYield.dt[, imputedArea := naiveImp(imputedArea),
              by = country]

  ## Impute production where area and yield are available
  impYield.dt[is.na(imputedProd),
              imputedProd := imputedArea * imputedYield]

  ## Remove yield where area or produciton are zero
  impYield.dt[imputedProd == 0 | imputedArea == 0,
              imputedYield := as.numeric(NA)]
  
  ## setnames(impYield.dt,
  ##          new = c(area, prod, yield, country, region, year),
  ##          old = c("area", "prod", "yield", "country", "region", "year"))

  list(imputed = impYield.dt, model = impYield.lst$model)
}

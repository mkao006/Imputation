########################################################################
## Title: Function to perform the full imputation
## Date: 2013-05-27
########################################################################

fullImputation = function(Data, area, prod, yield, country, region, year,
  n.iter = 1000, tol = 1e-8){
  dataCopy = data.table(Data)

  ## Initiate the formula
  fixed = formula(paste0(yield, " ~ 0"))
  random = formula(paste("~", year, "|", country))
  group = formula(paste("~", region, "*", year))

  ## Impute yield
  impYield.lst = lmeImpute(fixed = fixed, random = random,
    group = group, Data = dataCopy, silent = FALSE)

  impYield.dt = data.table(impYield.lst$imputed)
  print(paste("yield methodology: ", impYield.lst$method))
  impYield.dt[, yieldMethodology := impYield.lst$method]
  setnames(impYield.dt, old = "impValue", new = "imputedYield")
  
  ## Impute area and produciton if available
  impYield.dt[, eval(parse(text = paste0("imputedArea := ", area)))]
  impYield.dt[, eval(parse(text = paste0("imputedProd := ", prod)))]
  impYield.dt[is.na(imputedArea) & !is.na(imputedProd) & !is.na(imputedYield),
           imputedArea := imputedProd/imputedYield]
  impYield.dt[is.na(imputedProd) & !is.na(imputedArea) & !is.na(imputedYield),
           imputedProd := imputedArea * imputedYield]

  ## Impute area with naive imputation
  impYield.dt[, imputedArea := naiveImp(imputedArea),
              by = country]

  ## Impute production where area and yield are available
  impYield.dt[is.na(imputedProd), imputedProd := imputedArea * imputedYield]

  ## Remove yield where area or produciton are zero
  impYield.dt[imputedProd == 0 | imputedArea == 0, imputedYield := as.numeric(NA)]
  
  ## setnames(impYield.dt,
  ##          new = c(area, prod, yield, country, region, year),
  ##          old = c("area", "prod", "yield", "country", "region", "year"))
  
  impYield.dt
}

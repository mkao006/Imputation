########################################################################
## Title: Script to impute wheat data
## Date: 2013-06-12
########################################################################

source("wheatDataManipulation.R")
source("FAOProductionImpute.R")
source("lmeImpute.R")
source("naiveImpute.R")

## Imputation
## ---------------------------------------------------------------------
imputed.dt = FAOProductionImpute(wheatPrep.dt, area = "valueArea",
  prod = "valueProd", yield = "valueYield", country = "FAOST_CODE",
  region = "UNSD_SUB_REG", year = "Year")


## TODO (Michael): Move the following in to the fullImputation
##                 function, in addition should find a way to account
##                 for non-existing country.

## Percentage of missing value imputed
NROW(imputed.dt[is.na(valueProd) & !is.na(imputedProd), ])/
  NROW(imputed.dt[is.na(valueProd), ])

NROW(imputed.dt[is.na(valueArea) & !is.na(imputedArea), ])/
  NROW(imputed.dt[is.na(valueArea), ])


## Diagnosis and plots
## ---------------------------------------------------------------------

## Check coefficients and standard deviation
cbind(coef = round(fit$coefficients$fixed, 4),
      sd = sqrt(diag(fit$varFix)))


## Check all individual imputation
pdf(file = "checkWheatImputation.pdf", width = 10)
for(i in unique(imputed.dt$FAOST_CODE)){
  tmp = imputed.dt[FAOST_CODE == i, ]

  myCountry =
    FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE == i),
                      "FAO_TABLE_NAME"]
  myItem = with(FAOmetaTable, unique(itemTable[itemTable$itemCode ==
    unique(imputed.dt$itemCode), "itemName"]))
  par(mfrow = c(4, 1), mar = c(2.1, 4.1, 3.1, 2.1))
  try({
    ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Production",
                   main = paste0(myCountry, " (", i, ") - ",
                     myItem, " (", unique(imputed.dt$itemCode), ")"),
                   cex = 2))
    with(tmp[is.na(valueProd), ],
         points(Year, imputedProd, col = "blue", pch = 19))
  })
  
  try({
    ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
    with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Area",
                   cex = 2))
    with(tmp[is.na(valueArea),],
         points(Year, imputedArea, col = "blue", pch = 19))
  })
  
  
  try({
    ymax = max(tmp[, list(valueYield, imputedYield, groupAverage)],
      na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Yield",
                    cex = 2))
    with(tmp[!is.na(valueYield), ],
         points(Year, imputedYield, col = "red", pch = 19))
    with(tmp[is.na(valueYield), ],
         points(Year, imputedYield, col = "blue", pch = 19))
  })
  try({
    with(tmp, plot(Year, groupAverage, ylab = "Average Yield",
                   ylim = c(0, ymax), type = "b"))
  })
  
}
graphics.off()
system("evince checkWheatImputation.pdf&")



## Check the sub-regional fit
pdf(file = "wheatYieldSubregion.pdf", width = 11)
print(ggplot(data = imputed.dt,
             aes(x = Year, y = valueYield)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(imputed.dt$FAOST_CODE)))) +
      geom_line(aes(x = Year, y = groupAverage), col = "steelblue",
                alpha = 0.5) +
      facet_wrap(~UNSD_SUB_REG, ncol = 4) +
      labs(x = NULL, y = NULL,
      title = "Yield of Wheat by sub-region with estimated average") + 
      theme(legend.position = "none"))
graphics.off()
system("evince wheatYieldSubregion.pdf&")




## Illustration of the impuation process.
## pdf(file = "wheatImputationStep.pdf", width = 10)
## i = 2
## tmp = imputed.dt[FAOST_CODE == i, ]

## ## Step (1): Raw data
## myCountry = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
##   i), "FAO_TABLE_NAME"]
## myItem = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
##   unique(imputed.dt$itemCode), "itemName"])
## par(mfrow = c(4, 1), mar = c(2.1, 4.1, 3.1, 2.1))
## title = "Step (1): Raw data - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
## })

## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
## })


## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "n",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "n"))
## })



## ## Step (2): Compute Implied Yield
## title = "Step (2) Compute implied yield - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
## })

## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
## })


## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "n"))
## })


## ## Step (3): Impute the yield
## title = "Step (3) Impute the yield - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
## })
## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
## })
## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
##   with(tmp[is.na(valueYield), ],
##        points(Year, imputedYield, col = "blue", pch = 19))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "b"))
## })



## ## Step (4): Compute Area and production where available
## title = "Step (4): Compute area and production where available"
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
##   with(tmp[is.na(valueProd) & !is.na(valueArea), ],
##        points(Year, imputedProd, col = "blue", pch = 19))
## })
## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
##   with(tmp[is.na(valueArea) & !is.na(valueProd),],
##        points(Year, imputedArea, col = "blue", pch = 19))
## })
## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
##   with(tmp[is.na(valueYield), ],
##        points(Year, imputedYield, col = "red", pch = 19))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "b"))
## })


## ## Step (5): Impute Area with naive imputation
## title = "Step (5): Impute area with naive imputation - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
##   with(tmp[is.na(valueProd) & !is.na(valueArea), ],
##        points(Year, imputedProd, col = "blue", pch = 19))
## })
## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
##   with(tmp[is.na(valueArea) & !is.na(valueProd),],
##        points(Year, imputedArea, col = "red", pch = 19))
##   with(tmp[is.na(valueArea) & is.na(valueProd),],
##        points(Year, imputedArea, col = "blue", pch = 19))  
## })
## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
##   with(tmp[is.na(valueYield), ],
##        points(Year, imputedYield, col = "red", pch = 19))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "b"))
## })


## ## Step (6): Impute the final production
## title = "Step (6): Impute the production - "
## try({
##   ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Production",
##                  main = paste0(title, myCountry, " (", i, ") - ",
##                    myItem, " (", unique(imputed.dt$itemCode), ")"),
##                  cex = 2))
##   with(tmp[is.na(valueProd), ],
##        points(Year, imputedProd, col = "blue", pch = 19))
## })
## try({
##   ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
##   with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Area",
##                  cex = 2))
##   with(tmp[is.na(valueArea) & !is.na(valueProd),],
##        points(Year, imputedArea, col = "red", pch = 19))
##   with(tmp[is.na(valueArea) & is.na(valueProd),],
##        points(Year, imputedArea, col = "red", pch = 19))  
## })
## try({
##   ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
##     na.rm = TRUE) * 1.2
##   with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
##                  col = "black", xlab = "", ylab = "Yield",
##                  cex = 2))
##   with(tmp[is.na(valueYield), ],
##        points(Year, imputedYield, col = "red", pch = 19))
## })
## try({
##   with(tmp, plot(Year, avgValue, ylab = "Average Yield",
##                  ylim = c(0, ymax), type = "b"))
## })

## graphics.off()
## system("evince wheatImputationStep.pdf&")



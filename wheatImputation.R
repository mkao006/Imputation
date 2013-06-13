########################################################################
## Title: Script to impute wheat data
## Date: 2013-06-12
########################################################################

source("wheatDataManipulation.R")

## Imputation
## ---------------------------------------------------------------------
imputed.dt = fullImputation(wheatPrep.dt, area = "valueArea", prod = "valueProd",
  yield = "valueYield", country = "FAOST_CODE",
  group = "UNSD_SUB_REG", year = "Year",  commodity = "itemCode")


## Percentage of missing value imputed
NROW(imputed.dt[is.na(valueProd) & !is.na(imputedProd), ])/
  NROW(imputed.dt[is.na(valueProd), ])

NROW(imputed.dt[is.na(valueArea) & !is.na(imputedArea), ])/
  NROW(imputed.dt[is.na(valueArea), ])

## Remove yield where area or produciton are zero
imputed.dt[imputedProd == 0 | imputedArea == 0, imputedYield := as.numeric(NA)]


## Examination and plots
## ---------------------------------------------------------------------

pdf(file = "checkWheatImputation.pdf", width = 10)
for(i in unique(imputed.dt$FAOST_CODE)){
  tmp = imputed.dt[FAOST_CODE == i, ]

  myCountry = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
    i), "FAO_TABLE_NAME"]
  myItem = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
    unique(imputed.dt$itemCode), "itemName"])
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
    ymax = max(tmp[, list(valueYield, imputedYield, avgValue)],
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
    with(tmp, plot(Year, avgValue, ylab = "Average Yield",
                   ylim = c(0, ymax), type = "b"))
  })
  
}
graphics.off()
system("evince checkWheatImputation.pdf&")



pdf(file = "wheatYieldSubregion.pdf", width = 11)
print(ggplot(data = imputed.dt,
             aes(x = Year, y = valueYield)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(final.dt$FAOST_CODE)))) +
      geom_line(aes(x = Year, y = avgValue), col = "steelblue", alpha = 0.5) +
      facet_wrap(~UNSD_SUB_REG, ncol = 4) +
      labs(x = NULL, y = NULL,
      title = "Yield of Wheat by sub-region with average and least square line") + 
      theme(legend.position = "none"))
graphics.off()
system("evince wheatYieldSubregion.pdf&")



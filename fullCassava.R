########################################################################
## Title: Test the full series of Cassava production for case study
## Date: 2013-04-30
########################################################################

library(data.table)
library(reshape2)
library(FAOSTAT)
library(nlme)
library(zoo)

fc.df = read.csv(file = "fullCassava.csv", header = TRUE,
  stringsAsFactors = FALSE)


mfc.df = melt(fc.df, id.var = c("Area.Code", "Area.Name",
                       "Item.Code", "Element.Code"))
ind = sapply(regexpr("_", mfc.df$variable), function(x) x[[1]])
mfc.df$Year = substring(mfc.df$variable, ind + 1)
mfc.df$type = substring(mfc.df$variable, 1, ind - 1)
mfc.df$variable = NULL

mfc.df$Element.Code = ifelse(mfc.df$Element.Code == 31, "valueArea",
  "valueProd")

cmfc.df = dcast(mfc.df, Area.Code + Area.Name + Item.Code + Year ~ Element.Code +
  type, value.var = "value")
colnames(cmfc.df) = c("FAOST_CODE", "FAOST_NAME", "itemCode", "Year",
          "valueArea", "symbArea", "valueProd", "symbProd")


cmfc.df$Year = as.integer(cmfc.df$Year)
cmfc.df$valueArea = as.numeric(cmfc.df$valueArea)
cmfc.df$valueProd = as.numeric(cmfc.df$valueProd)

## Only use data from 1980
## cmfc.df = cmfc.df[cmfc.df$Year >= 1980, ]

## Save the original value
cmfc.df$ovalueArea = cmfc.df$valueArea
cmfc.df$ovalueProd = cmfc.df$valueProd

## Replace values with symbol T or duplicated F with NA
cmfc.df[which(cmfc.df$symbArea == "T"), "valueArea"] = NA
cmfc.df[which(cmfc.df$symbProd == "T"), "valueProd"] = NA
cmfc.df[which(cmfc.df$symbArea == "E"), "valueArea"] = NA
cmfc.df[which(cmfc.df$symbProd == "E"), "valueProd"] = NA

cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbArea",
                                 "valueArea")]) & cmfc.df$symbArea == "F"),
         "valueArea"] = NA
cmfc.df[which(duplicated(cmfc.df[, c("FAOST_CODE", "itemCode", "symbProd",
                                 "valueProd")]) & cmfc.df$symbProd == "F"),
         "valueProd"] = NA

cmfc.df[which(cmfc.df$valueArea == 0 & cmfc.df$valueProd != 0),
           "valueArea"] = NA
cmfc.df[which(cmfc.df$valueArea != 0 & cmfc.df$valueProd == 0),
           "valueProd"] = NA

## A function to compute yield to account for zeros in area and
## production. To account fo identification problem, we compute yield
## as NA if one of area or production is zero or NA.
computeYield = function(production, area){
  if(length(production) != length(area))
    stop("Length of prodduction is not the same as area")
  ifelse(production != 0 & area != 0, production/area, NA)
}

cmfc.df$valueYield = with(cmfc.df, computeYield(valueProd, valueArea))
cmfc.df$ovalueYield = with(cmfc.df, computeYield(ovalueProd, ovalueArea))


## Merge with regional and subregional
cmfc.df = arrange(merge(cmfc.df,
  FAOregionProfile[, c("FAOST_CODE", "UNSD_SUB_REG_CODE",
  "UNSD_MACRO_REG_CODE")], all.x = TRUE), FAOST_CODE, itemCode, Year)
## This is a hack for the region until Fillipo fixes the countryprofile.
cmfc.df = merge(cmfc.df,
  na.omit(unique(FAOregionProfile[, c("UNSD_MACRO_REG_CODE", "UNSD_MACRO_REG")])),
  all.x = TRUE)
cmfc.df = merge(cmfc.df,
  na.omit(unique(FAOregionProfile[, c("UNSD_SUB_REG_CODE", "UNSD_SUB_REG")])),
  all.x = TRUE)
cmfc.df$UNSD_SUB_REG_CODE = NULL
cmfc.df$UNSD_MACRO_REG_CODE = NULL

## These had no areas
cmfc.df[which(cmfc.df$FAOST_CODE == 164), "UNSD_MACRO_REG"] = "Oceania"
cmfc.df[which(cmfc.df$FAOST_CODE == 164), "UNSD_SUB_REG"] = "Polynesia"

## Western Asia is removed because there is no information
final.dt = data.table(cmfc.df[cmfc.df$UNSD_SUB_REG != "Western Asia", ])
setkeyv(final.dt, c("FAOST_CODE", "FAOST_NAME", "Year"))

## Number of official and semi officail data
final.dt[symbArea %in% c(" ", "*"), sum(!is.na(ovalueArea))]
final.dt[symbProd %in% c(" ", "*"), sum(!is.na(ovalueProd))]


## Function to check the sparsity of the data
checkSparsity = function(Data){
  image(data.matrix(is.na(Data)))
  text(rep(0.5, NCOL(Data)), seq(0, 1, length = NCOL(Data)),
       labels = paste(colnames(Data), " (", round(sapply(X = Data,
         FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))
}

checkSparsity(final.dt)




## Function to carry out linear interpolation
na.approx2 = function(x, na.rm = FALSE){
  if(length(na.omit(x)) < 2){
    tmp = x
  } else {
    tmp = na.approx(x, na.rm = na.rm)
  }
  c(tmp)
}

## Function to compute changes from year to year
diffv = function(x){
    T = length(x)
    if(sum(!is.na(x)) >= 2){
      tmp = c(x[2:T]/x[1:(T - 1)])
      tmp[is.nan(tmp) | tmp == Inf | tmp == -Inf] = NA
    } else {
      tmp = rep(NA, length(x) - 1)
    }
    tmp
}


## Function to impute with LME
lmeImpute = function(Data, value, country, group, year, commodity){
  setnames(Data, old = c(value, country, group, year, commodity),
           new = c("value", "country", "group", "year", "commodity"))
  Data[, group := factor(group)]
  for(i in unique(Data$commodity)){
    ## Data[commodity == i, valueCh := diffv(value), by = "country"]
    ## Data[commodity == i, groupValueCh := mean(valueCh, na.rm = TRUE),
    ##      by = c("year", "group")]
    ## Data[commodity == i & is.na(groupValueCh), groupValueCh := 0]
    Data[commodity == i, avgYield := mean(value, na.rm = TRUE),
         by = c("group", "year")]
    ## Data[commodity == i & is.na(avgYield),
    ##      check := .SD[Data, mean(avgYield, na.rm = TRUE)],
    ##      by = "group"]
    Data[, groupAvgYield := mean(value, na.rm = TRUE), by = "group"]
    Data[is.na(avgYield), avgYield := groupAvgYield]
    Data[, groupAvgYield := NULL]
    
    fit <<- try(lme(value ~ year * group, random= ~avgYield + year|country,
      na.action = na.omit, data = Data[commodity == i, ]))
    
    if(!inherits(fit, "try-error")){
      Data[commodity == i & is.na(value),
           imputedValue := predict(fit, Data[commodity == i & is.na(value), ])]
      Data[commodity == i & !is.na(value),
           fittedValue := predict(fit, Data[commodity == i & !is.na(value), ])]
      ## Data[commodity == i, valueCh := NULL]
      ## Data[commodity == i, groupValueCh := NULL]
    }
  }
  setnames(Data, old = c("value", "country", "group", "year", "commodity"),
           new = c(value, country, group, year, commodity))
}

## ## Function to impute with LME
## lmeImpute = function(Data, value, country, group, year, commodity){
##   setnames(Data, old = c(value, country, group, year, commodity),
##            new = c("value", "country", "group", "year", "commodity"))
##   for(i in unique(Data$commodity)){
##     Data[commodity == i, valueCh := diffv(value), by = "country"]
##     Data[commodity == i, groupValueCh := mean(valueCh, na.rm = TRUE),
##          by = c("year", "group")]
##     Data[commodity == i & is.na(groupValueCh), groupValueCh := 0]
##     ## fit <<- try(lme(value ~ year * group + groupValueCh, random = ~year|country,
##     ##   na.action = na.omit, data = Data[commodity == i, ]))
##     fit <<- try(lme(value ~ year * group, random = ~ groupValueCh|country,
##       na.action = na.omit, data = Data[commodity == i, ]))    
##     if(!inherits(fit, "try-error")){
##       Data[commodity == i & is.na(value),
##            imputedValue := predict(fit, Data[commodity == i & is.na(value), ])]
##       Data[commodity == i & !is.na(value),
##            fittedValue := fitted(fit)]
##       Data[commodity == i, valueCh := NULL]
##       Data[commodity == i, groupValueCh := NULL]
##     }
##   }
##   setnames(Data, old = c("value", "country", "group", "year", "commodity"),
##            new = c(value, country, group, year, commodity))
## }

lmeImpute(Data = final.dt, value = "valueYield", country = "FAOST_CODE",
          group = "UNSD_SUB_REG", year = "Year", commodity = "itemCode")
setnames(final.dt, old = "imputedValue", new = "imputedYield")


## ## Function to impute with LME
## lmeEMImpute = function(Data, value, country, group, year, commodity,
##   n.iter = 1000, tol = 1e-6){
##   setnames(Data, old = c(value, country, group, year, commodity),
##            new = c("value", "country", "group", "year", "commodity"))
##   for(i in unique(Data$commodity)){
##     print(i)
##     ll = double(n.iter)
##     ll[1] = -Inf
##     missInd = is.na(Data[, value])    
##     Data[, estValue := value]
    
##     for(j in 2:n.iter){      
##       ## Data[, avgValue := mean(estValue, na.rm = TRUE), by = c("year", "group")]
##       ## fit = try(lme(estValue ~ year * group + avgValue, random= ~year|country,
##       ##   na.action = na.omit, data = Data[commodity == i, ]))
##       Data[, avgYield := mean(estValue, na.rm = TRUE),
##            by = c("group", "year")]
##       Data[, groupAvgYield := mean(estValue, na.rm = TRUE), by = "group"]
##       Data[is.na(avgYield), avgYield := groupAvgYield]
##       Data[, groupAvgYield := NULL]
      
##       fit <<- try(lme(value ~ year * group, random= ~avgYield|country,
##                       na.action = na.omit, data = Data[commodity == i, ]))
            
##       fit.ll = logLik(fit)
##       print(fit.ll)
##       if(!inherits(fit, "try-error")){
##         if(fit.ll - ll[j - 1] > tol){
##           Data[commodity == i & missInd,
##                estValue := predict(fit, Data[commodity == i & missInd, ])]
##           Data[commodity == i & !is.na(value),
##                fittedValue := predict(fit, Data[commodity == i & !missInd, ])]
##           ll[j] = fit.ll
##           ## Data[commodity == i, valueCh := NULL]
##           ## Data[commodity == i, groupValueCh := NULL]
##         } else {
##           break
##         }
##       } else {
##         break
##       }
##     }
##   }
##   setnames(Data, old = c("value", "country", "group", "year", "commodity"),
##            new = c(value, country, group, year, commodity))
## }


## lmeEMImpute(final.dt, "valueYield", "FAOST_CODE", "UNSD_SUB_REG", "Year",
##             "itemCode")
## setnames(final.dt, old = "estValue", new = "imputedYield")



## Create the impute column
## final.dt[!is.na(valueYield), imputedYield := valueYield]
final.dt[!is.na(valueYield), imputedYield := as.numeric(NA)]
checkSparsity(final.dt)




final.dt[, imputedArea := valueArea]
final.dt[, imputedProd := valueProd]

## Impute area and production if the other one exist
final.dt[is.na(imputedArea) & !is.na(imputedProd),
         imputedArea := imputedProd/imputedYield]
final.dt[!is.na(imputedArea) & is.na(imputedProd),
         imputedProd := imputedArea * imputedYield]


## Impute area with linear interpolation and last observation carry
## forward.
final.dt[, imputedArea := na.locf(na.locf(na.approx2(imputedArea), na.rm = FALSE),
             fromLast = TRUE, na.rm = FALSE),
         by = c("FAOST_CODE", "itemCode")]


## Impute the remaining production
final.dt[is.na(imputedProd), imputedProd := imputedArea * imputedYield]

checkSparsity(final.dt)

## Percentage of missing value imputed
NROW(final.dt[is.na(valueProd) & !is.na(imputedProd), ])/
  NROW(final.dt[is.na(valueProd), ])

NROW(final.dt[is.na(valueArea) & !is.na(imputedArea), ])/
  NROW(final.dt[is.na(valueArea), ])

final.dt[, avgYield := mean(valueYield, na.rm = TRUE),
         by = c("Year", "UNSD_SUB_REG")]
final.dt[imputedProd == 0 & imputedArea == 0, imputedYield := as.numeric(NA)]

## Examination and plots
## ---------------------------------------------------------------------

pdf(file = "cassvaBreakDown.pdf", width = 15)
print(ggplot(data = final.dt[UNSD_SUB_REG != "Western Asia", ],
             aes(x = Year, y = valueYield)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(final.dt$FAOST_CODE)))) +
      geom_line(aes(x = Year, y = avgYield), col = "black", alpha = 0.5) +
      ## geom_line(aes(x = Year, y = avgValue), col = "blue", alpha = 0.5) +      
      facet_wrap(~UNSD_SUB_REG, ncol = 4, scales = "free_y") + 
      theme(legend.position = "none"))
graphics.off()
system("evince cassvaBreakDown.pdf&")

pdf(file = "checkCassavaImputation.pdf", width = 10)
## for(i in c(32, 45, 46, 61, 74, 144, 181, 215, 251)){
for(i in unique(final.dt$FAOST_CODE)){
  tmp = final.dt[FAOST_CODE == i, ]

  myCountry = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
    i), "LAB_NAME"]
  myItem = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
    unique(final.dt$itemCode), "itemName"])
  par(mfrow = c(3, 1), mar = c(2.1, 4.1, 3.1, 2.1))
  try({
    ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Production",
                   main = paste0(myCountry, " (", i, ") - ",
                     myItem, " (", unique(final.dt$itemCode), ")"),
                   cex = 2))
    with(tmp, points(Year, imputedProd, col = "red", pch = 19))
  })
  
  try({
    ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE) * 1.2  
    with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Area",
                   cex = 2))
    with(tmp, points(Year, imputedArea, col = "red", pch = 19))
  })
  
  
  try({
    ymax = max(tmp[, list(valueYield, imputedYield)], na.rm = TRUE) * 1.2
    with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
                   col = "black", xlab = "", ylab = "Yield",
                   cex = 2))
    with(tmp, points(Year, imputedYield, col = "red", pch = 19))
    with(tmp, points(Year, fittedValue, col = "blue", pch = 19))    
  })
  
}
graphics.off()
system("evince checkCassavaImputation.pdf&")



pdf(file = "cassavaYieldSubregion.pdf", width = 11)
print(ggplot(data = final.dt[!UNSD_SUB_REG %in% c("Western Asia", "Micronesia",
               "Northern Africa"), ],
             aes(x = Year, y = valueYield)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(final.dt$FAOST_CODE)))) +
      geom_line(aes(x = Year, y = avgYield), col = "black", alpha = 0.5) +      
      ## geom_line(aes(x = Year, y = avgValue), col = "steelblue", alpha = 0.5) +
      geom_smooth(method = "lm") + 
      ## geom_line(aes(x = Year, y = avgValue), col = "blue", alpha = 0.5) +      
      facet_wrap(~UNSD_SUB_REG, ncol = 3) +
      labs(x = NULL, y = NULL,
           title = "Yield of Cassava by sub-region with average and least square line") + 
      theme(legend.position = "none"))
graphics.off()




print(ggplot(data = final.dt[!UNSD_SUB_REG %in% c("Western Asia", "Micronesia"), ],
             aes(x = Year, y = valueArea)) +
      geom_line(aes(col = factor(FAOST_CODE))) +
      geom_point(aes(col = factor(FAOST_CODE)), size = 1) +  
      scale_color_manual(values = rep("gold",
                           length(unique(final.dt$FAOST_CODE)))) +
      geom_smooth(method = "lm") + 
      facet_wrap(~UNSD_SUB_REG, ncol = 4, scales = "free_y") +
      labs(x = NULL, y = NULL, title = "Yield of Cassava by sub-region") + 
      theme(legend.position = "none"))






plot.df = melt(final.dt[symbArea %in% c(" ", "*") & symbProd %in% c(" ", "*"),
  list(FAOST_CODE, FAOST_NAME, Year, UNSD_SUB_REG,
  valueArea, valueProd, valueYield)],
  id.var = c("FAOST_CODE", "FAOST_NAME", "Year", "UNSD_SUB_REG"))
plot.df$variable = factor(gsub("value", "", plot.df$variable),
  levels = c("Prod", "Area", "Yield"))

tmp = data.table(plot.df[plot.df$variable == "Prod", ])
tmp[ ,avgValue := mean(value, na.rm = TRUE), by = c("Year", "UNSD_SUB_REG")]

ggplot(data = tmp,
       aes(x = Year, y = value)) +
  geom_line(aes(col = factor(FAOST_CODE)), alpha = 0.3) +
  scale_color_manual(values = rep("black", length(unique(plot.df$FAOST_CODE)))) +
  geom_line(aes(x = Year, y = avgValue), col = "blue") +
  facet_wrap(~UNSD_SUB_REG, ncol = 4, scales = "free_y") + 
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL,
       title = "Area and Yield series of Cassava on original scale")

pdf(file = "cassavaIdentityBreakDown.pdf", width = 10, height = 5)
ggplot(data = plot.df[which(plot.df$value > 0), ],
       aes(x = Year, y = log(value))) +
  geom_line(aes(col = factor(FAOST_CODE)), alpha = 0.3) +
  scale_color_manual(values = rep("black", length(unique(plot.df$FAOST_CODE)))) +
  facet_wrap(~variable, ncol = 1) +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL,
       title = "Relation of Cassava production, area and yield on log scale")
graphics.off()

pdf(file = "cassavaAreaYield.pdf", width = 10, height = 5)
ggplot(data = plot.df[plot.df$variable != "Prod", ],
       aes(x = Year, y = value)) +
  geom_line(aes(col = factor(FAOST_CODE)), alpha = 0.3) +
  scale_color_manual(values = rep("black", length(unique(plot.df$FAOST_CODE)))) +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL,
       title = "Area and Yield series of Cassava on original scale")
graphics.off()

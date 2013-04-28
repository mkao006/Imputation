########################################################################
## Title: The test version of the full imputation
## Date: 2013-04-16
########################################################################

## Run the data manipulation
source("dataManipulation.R")

## Function to compute changes from year to year
diffv = function(x){
    T = length(x)
    if(sum(!is.na(x)) >= 2){
      tmp = c(x[2:T]/x[1:(T - 1)])
      tmp[is.nan(tmp) | tmp == Inf | tmp == -Inf] = NA
    } else {
      tmp = rep(NA, length(x) - 1)
    }
    tmp - 1
}

## Function to impute with LME
lmeImpute = function(Data, value, country, group, year, commodity){
  setnames(Data, old = c(value, country, group, year, commodity),
           new = c("value", "country", "group", "year", "commodity"))
  for(i in unique(Data$commodity)){
    Data[commodity == i, valueCh := diffv(value), by = "country"]
    Data[commodity == i, groupValueCh := mean(valueCh, na.rm = TRUE),
         by = c("year", "group")]
    Data[commodity == i & is.na(groupValueCh), groupValueCh := 0]
    print(i)
    fit = try(lme(value ~ year + groupValueCh, random= ~1|country,
      na.action = na.omit, data = Data[commodity == i, ]))
    if(!inherits(fit, "try-error")){
      Data[commodity == i & is.na(value),
           imputedValue := predict(fit, Data[commodity == i & is.na(value), ])]
      Data[commodity == i, valueCh := NULL]
      Data[commodity == i, groupValueCh := NULL]
    }
  }
  setnames(Data, old = c("value", "country", "group", "year", "commodity"),
           new = c(value, country, group, year, commodity))
}

lmeImpute(final.dt, "valueYield", "FAOST_CODE", "UNSD_SUB_REG", "Year",
            "itemCode")
setnames(final.dt, old = "imputedValue", new = "imputedYield")

## Create the impute column
final.dt[!is.na(valueYield), imputedYield := valueYield]
final.dt[, imputedArea := valueArea]
final.dt[, imputedProd := valueProd]

## Impute area and production if the other one exist
final.dt[is.na(imputedArea) & !is.na(imputedProd),
         imputedArea := imputedProd/imputedYield]
final.dt[!is.na(imputedArea) & is.na(imputedProd),
         imputedProd := imputedArea * imputedYield]

if(check)
  checkSparsity(final.dt)


## Looks like there is a lot of mismatch between area and production,
## let us impute area now.

## Function to carry out linear interpolation
na.approx2 = function(x, na.rm = FALSE){
  if(length(na.omit(x)) < 2){
    tmp = x
  } else {
    tmp = na.approx(x, na.rm = na.rm)
  }
  c(tmp)
}

## Impute area with linear interpolation and last observation carry
## forward.
final.dt[, imputedArea := na.locf(na.locf(na.approx2(imputedArea), na.rm = FALSE),
             fromLast = TRUE, na.rm = FALSE),
         by = c("FAOST_CODE", "itemCode")]


## LME can not be used cause of excessive zeroes
## lmeImpute(final.dt, "imputedArea", "FAOST_CODE", "UNSD_SUB_REG", "Year",
##           "itemCode")

## Impute the remaining production
final.dt[is.na(imputedProd), imputedProd := imputedArea * imputedYield]

checkSparsity(final.dt)

## Percentage of missing value imputed
NROW(final.dt[is.na(valueProd) & !is.na(imputedProd), ])/
  NROW(final.dt[is.na(valueProd), ])

NROW(final.dt[is.na(valueArea) & !is.na(imputedArea), ])/
  NROW(final.dt[is.na(valueArea), ])


keys = unique(final.dt[is.na(valueProd) | is.na(valueArea),
  list(FAOST_CODE, itemCode)])

pdf(file = "checkImputation.pdf")
for(i in 1:NROW(keys)){
  tmp = final.dt[FAOST_CODE == keys[i, FAOST_CODE] &
    itemCode == keys[i, itemCode]]

  myCountry = FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
    keys[i, FAOST_CODE]), "LAB_NAME"]
  myItem = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
    keys[i, itemCode], "itemName"])
  par(mfrow = c(3, 1))
  try({
    ymax = max(tmp[, list(valueProd, imputedProd)], na.rm = TRUE)  
    with(tmp, plot(Year, valueProd, ylim = c(0, ymax), type = "b",
                   col = "black",
                   main = paste0(myCountry, " (", keys[i, FAOST_CODE], ") - ",
                     myItem, " (", keys[i, itemCode], ")")))
    with(tmp, points(Year, imputedProd, col = "red", pch = 19))
  })
  
  try({
    ymax = max(tmp[, list(valueArea, imputedArea)], na.rm = TRUE)  
    with(tmp, plot(Year, valueArea, ylim = c(0, ymax), type = "b",
                   col = "black"))
    with(tmp, points(Year, imputedArea, col = "red", pch = 19))
  })
  
  
  try({
    ymax = max(tmp[, list(valueYield, imputedYield)], na.rm = TRUE)  
    with(tmp, plot(Year, valueYield, ylim = c(0, ymax), type = "b",
                   col = "black"))
    with(tmp, points(Year, imputedYield, col = "red", pch = 19))
  })
  
}
graphics.off()
system("evince checkImputation.pdf&")


## Old codes
## ---------------------------------------------------------------------


## Function to carry out linear interpolation
na.approx2 = function(x, na.rm = FALSE){
  if(length(na.omit(x)) < 2){
    tmp = x
  } else {
    tmp = na.approx(x, na.rm = na.rm)
  }
  c(tmp)
}

final.dt[, valueArea := na.approx2(valueArea), by = c("FAOST_CODE", "itemCode")]
checkSparsity(final.dt)

## Assuming linear interpolation has been carried out
auto.arima.forecast = function(ts){
  if(length(na.omit(ts)) >= 5){
    notNA = which(!is.na(ts))
    ## print(ts)
    firstObs = notNA[1]
    lastObs = notNA[length(notNA)]
    n = length(ts)
    tmp.fit = try(auto.arima(ts[firstObs:lastObs]))
    if(!inherits(tmp.fit, "try-error")){
      if(lastObs != n){
        imp = forecast(tmp.fit, h = n - lastObs)
        final = c(rep(NA, firstObs - 1), ts[firstObs:lastObs], imp$mean)
      } else {
        final = c(rep(NA, firstObs - 1), ts[firstObs:lastObs])
      }
    } else {
      final = ts
    }
  }else {
    final = ts
  }
  c(final)
}

final.dt[, valueArea := auto.arima.forecast(valueArea),
         by = c("FAOST_CODE", "itemCode")]

final.dt[!is.na(valueArea) & !is.na(valueProd) & is.na(valueYield),
         valueYield := valueProd/valueArea]


final.dt[!is.na(valueArea) & is.na(valueProd) & !is.na(valueYield),
         valueProd := valueArea * valueYield]
checkSparsity(final.dt)


final.dt[, valueYield := auto.arima.forecast(valueYield),
         by = c("FAOST_CODE", "itemCode")]
checkSparsity(final.dt)

final.dt[, mvalueYield := median(valueYield, na.rm = TRUE),
         by = c("FAOST_CODE", "itemCode")]

omitMissYield.dt = final.dt[!is.na(mvalueYield), ]

omitMissYield.dt[is.na(valueYield), valueYield := mvalueYield]

omitMissYield.dt[is.na(valueArea) & !is.na(valueProd),
                 valueArea := valueProd/valueYield]

omitMissYield.dt[is.na(valueProd) & !is.na(valueArea),
                 valueProd := valueArea * valueYield]

## All the unimputed data are of "E", "F", and "T"s.
table(omitMissYield.dt[is.na(valueProd), symbArea])
table(omitMissYield.dt[is.na(valueProd), symbProd])


## Use linear interpolation for area
omitMissYield.dt[, valueArea := na.approx2(valueArea, na.rm = FALSE),
                 by = c("FAOST_CODE", "itemCode")]

omitMissYield.dt[is.na(valueProd), valueProd := valueArea * valueYield]

## Function to check the sparsity of the data
checkSparsity = function(Data){
  image(data.matrix(is.na(Data)))
  text(rep(0.5, NCOL(Data)), seq(0, 1, length = NCOL(Data)),
       labels = paste(colnames(Data), " (", round(sapply(X = Data,
         FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))
}

checkSparsity(omitMissYield.dt)

keys = unique(omitMissYield.dt[, list(FAOST_CODE, itemCode)])

pdf(file = "checkAllImputedSeries.pdf", width = 10)
for(i in 1:NROW(keys)){
  tmp = subset(omitMissYield.dt, FAOST_CODE == keys[i, FAOST_CODE] &
    itemCode == keys[i, itemCode])
  with(tmp, plot(Year, valueProd, ylim = c(0, max(valueProd,
                                         na.rm = TRUE)), type = "l",
  main = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
         keys[i, itemCode], "itemName"]), "(", keys[i, itemCode], ") - ",
         FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
                                 keys[i, FAOST_CODE]), "LAB_NAME"], sep = "")))
  with(tmp, lines(Year, valueArea, col = "green"))
  with(tmp, lines(Year, valueYield, col = "steelblue"))
}  
graphics.off()
system("evince checkAllImputedSeries.pdf&")






## Examine codes
## ---------------------------------------------------------------------

pdf(file = "yieldTS.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"))
  ## tmp[, upper := predict(rq(valueYield ~ Year, tau = 0.95), data.frame(Year))]
  ## tmp[, med := predict(rq(valueYield ~ Year, tau = 0.5), data.frame(Year))]
  ## tmp[, lower := predict(rq(valueYield ~ Year, tau = 0.05), data.frame(Year))]
  try(print(ggplot(data = tmp, aes(x = Year, y = valueYield)) +
            geom_line(aes(col = factor(FAOST_CODE))) +
            scale_color_manual(values = c(rep(rgb(0, 0, 0, alpha = 0.5),
                               length(unique(tmp$FAOST_CODE))), "blue", "red")) +
            theme(legend.position = "none") +
            ## geom_line(aes(x = Year, y = upper, col = "red",
            ##               linetype = "dashed", lwd = 1)) +
            ## geom_line(aes(x = Year, y = med, col = "blue",
            ##               linetype = "dashed", lwd = 1)) +
            ## geom_line(aes(x = Year, y = lower, col = "red",
            ##               linetype = "dashed", lwd = 1)) +
            labs(y = paste0("Yield series for ",
                   FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                          "itemName"], "(", i, ")"),
                 x = NULL)
            ))
}
graphics.off()
system("evince yieldTS.pdf&")




pdf(file = "areaTS.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"))
  ## tmp[, upper := predict(rq(valueArea ~ Year, tau = 0.95), data.frame(Year))]
  ## tmp[, med := predict(rq(valueArea ~ Year, tau = 0.5), data.frame(Year))]
  ## tmp[, lower := predict(rq(valueArea ~ Year, tau = 0.05), data.frame(Year))]
  try(print(ggplot(data = tmp, aes(x = Year, y = valueArea)) +
            geom_line(aes(col = factor(FAOST_CODE))) +
            scale_color_manual(values = c(rep(rgb(0, 0, 0, alpha = 0.5),
                               length(unique(tmp$FAOST_CODE))), "blue", "red")) +
            theme(legend.position = "none") +
            ## geom_line(aes(x = Year, y = upper, col = "red",
            ##               linetype = "dashed", lwd = 1)) +
            ## geom_line(aes(x = Year, y = med, col = "blue",
            ##               linetype = "dashed", lwd = 1)) +
            ## geom_line(aes(x = Year, y = lower, col = "red",
            ##               linetype = "dashed", lwd = 1)) +
            labs(y = paste0("Area series for ",
                   FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                          "itemName"], "(", i, ")"),
                 x = NULL)
            ))
}
graphics.off()
system("evince areaTS.pdf&")


pdf(file = "areaLogTS.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"))
  try(print(ggplot(data = tmp, aes(x = Year, y = log(valueArea))) +
            geom_line(aes(col = factor(FAOST_CODE))) +
            scale_color_manual(values = c(rep(rgb(0, 0, 0, alpha = 0.5),
                               length(unique(tmp$FAOST_CODE))), "blue", "red")) +
            theme(legend.position = "none") +
            labs(y = paste0("Area series for ",
                   FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
                                          "itemName"], "(", i, ")"),
                 x = NULL)
            ))
}
graphics.off()
system("evince areaLogTS.pdf&")




## Investigate shocks in production
pdf(file = "examineShocks.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp.dt = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"),
    select = c("FAOST_CODE", "itemCode", "Year", "valueArea", "valueProd",
      "valueYield"))
  mtmp.df = melt(tmp.dt, id.var = c("FAOST_CODE", "itemCode", "Year"))
  mtmp.df$variable = factor(gsub("value", "", mtmp.df$variable),
    levels = c("Prod", "Area", "Yield"))
    try(print(ggplot(data = mtmp.df, aes(x = Year, y = log(value))) +
    geom_line(aes(col = factor(FAOST_CODE))) +
      facet_wrap(~variable, ncol = 1) +
        scale_color_manual(values = rep(rgb(0, 0, 0, alpha = 0.3),
                             length(unique(mtmp.df$FAOST_CODE)))) +
                               theme(legend.position = "none") +
  labs(y = paste("Log of ",
         unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
              i, "itemName"]), " (", i, ")", sep = ""))
            ))
}
graphics.off()
system("evince examineShocks.pdf&")

final.dt[, varArea := var(valueArea, na.rm = TRUE),
         by = c("FAOST_CODE", "itemCode")]

final.dt[, varYield := var(valueYield, na.rm = TRUE),
         by = c("FAOST_CODE", "itemCode")]

cor2 = function(x, y){
  tmp = try(cor(x, y, use = "complete.obs"))
  if(inherits(tmp, "try-error"))
    tmp = as.numeric(NA)
  tmp
}

final.dt[, areaYieldCor := cor2(valueYield, valueArea),
         by = c("FAOST_CODE", "itemCode")]
hist(final.dt$areaYieldCor, breaks = 100)

with(final.dt, plot(log(varArea), log(varYield)))
with(final.dt, plot(log(valueArea), log(valueYield)), )



pdf(file = "yieldAreaRelationship.pdf")
for(i in unique(final.dt$itemCode)){
  tmp.dt = subset(final.dt, itemCode == i &
    symbArea %in% c("", "*") & symbProd %in% c("", "*"))
    print(ggplot(data = tmp.dt,
                 aes(x = log(valueArea), y = log(valueYield))) +
          geom_point(alpha = 0.1))
}    
graphics.off()
system("evince yieldAreaRelationship.pdf&")

pdf(file = "checkAllLogImputedSeries.pdf", width = 10)
for(i in 1:NROW(keys)){
  tmp = subset(omitMissYield.dt, FAOST_CODE == keys[i, FAOST_CODE] &
    itemCode == keys[i, itemCode])
  with(tmp, plot(Year, log(valueProd), ylim = c(0, max(log(valueProd),
                                         na.rm = TRUE)), type = "l",
  main = paste(unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode ==
         keys[i, itemCode], "itemName"]), "(", keys[i, itemCode], ") - ",
         FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
                                 keys[i, FAOST_CODE]), "LAB_NAME"], sep = "")))
  with(tmp, text(Year, log(valueProd), labels = symbProd))
  with(tmp, lines(Year, log(valueArea), col = "green"))
  with(tmp, text(Year, log(valueArea), labels = symbArea))
  with(tmp, lines(Year, log(valueYield), col = "steelblue"))
}  
graphics.off()
system("evince checkAllLogImputedSeries.pdf&")



omitMissYield.dt$checkYield = with(omitMissYield.dt, valueProd/valueArea)
with(omitMissYield.dt, plot(valueYield, checkYield))





## Examine forecast under different methods






## Test of the linear mixed model
wy.dt = final.dt[itemCode == 116, list(FAOST_CODE, itemCode, Year, valueYield)]
mwy.dt = merge(wy.dt, FAOregionProfile[, c("FAOST_CODE", "UNSD_SUB_REG_CODE")],
  all.x = TRUE, by = "FAOST_CODE")

mwy.dt[, gvalueYield := mean(valueYield, na.rm = TRUE),
       by = c("UNSD_SUB_REG_CODE", "Year")]
mwy.dt[, UNSD_SUB_REG_CODE := factor(UNSD_SUB_REG_CODE)]

lme.fit = lme(valueYield ~ Year + gvalueYield, random =~ 1|FAOST_CODE,
  data = mwy.dt, na.action = na.omit)

mwy.dt[is.na(valueYield) & !is.na(gvalueYield), valueYield2 :=
       predict(lme.fit, mwy.dt[is.na(valueYield)  & !is.na(gvalueYield), ])]

mwy.dt[!is.na(valueYield) & !is.na(gvalueYield), valueYield2 :=
       fitted(lme.fit)]


pdf(file = "yieldImputeCheck.pdf", width = 12)
for(i in unique(final.dt$itemCode)){
  tmp.dt = final.dt[itemCode == i, ]
  ## tmp.dt[, nValue := sum(!is.na(valueYield))/length(valueYield),
  ##        by = c("UNSD_MACRO_REG")]
  ## tmp.dt[, subregionalYield := mean(valueYield, na.rm = TRUE),
  ##        by = c("Year", "UNSD_MACRO_REG")]
  ## tmp.dt[nValue <= 0.5, subregionalYield := as.numeric(NA)]
  ## tmp.dt[is.na(subregionalYield),
  ##        subregionalYield := mean(ovalueProd/ovalueArea, na.rm = TRUE),
  ##        by = "Year"]

  tmp.dt[, valueYieldCh := diffv(valueYield),
         by = "FAOST_CODE"]
  tmp.dt[, subregionalValueYieldCh := mean(valueYieldCh, na.rm = TRUE),
         by = c("UNSD_SUB_REG", "Year")]
  ## TODO (Michael): Need to check whether this is appropriate.
  tmp.dt[is.na(subregionalValueYieldCh), subregionalValueYieldCh := 0]
  
  try({
    ## tmp.fit = lme(valueYield ~ Year + subregionalYield, random =~1|FAOST_CODE,
    ##   na.action = na.omit, data = tmp.dt)
    tmp.fit = lme(valueYield ~ Year + subregionalValueYieldCh,
      random =~1|FAOST_CODE, na.action = na.omit, data = tmp.dt)
    ## tmp.fit = lme(valueYield ~ Year + subregionalValueYieldCh,
    ##   random =~1|FAOST_CODE, correlation = corCAR1(form = ~Year|FAOST_CODE),
    ##   na.action = na.omit, data = tmp.dt)
    
    tmp.dt[is.na(valueYield),
           imputedYield := predict(tmp.fit, tmp.dt[is.na(valueYield), ])]
    
    mtmp.df = melt(tmp.dt[, list(FAOST_CODE, UNSD_SUB_REG, itemCode, Year,
      valueYield, subregionalValueYieldCh, imputedYield)],
      id.var = c("FAOST_CODE", "UNSD_SUB_REG", "itemCode", "Year"))
    
    itemName = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
      "itemName"])
    print(ggplot(data = mtmp.df, aes(x = Year, y = value)) +
          geom_line(data = mtmp.df[mtmp.df$variable == "valueYield", ],
                    aes(col = factor(FAOST_CODE))) +
          geom_point(data = mtmp.df[mtmp.df$variable == "imputedYield", ],
                     aes(col = factor(FAOST_CODE))) +                
          ## geom_line(data = mtmp.df[mtmp.df$variable == "subregionalYield", ],
          ##           lwd = 1.5) +
          theme(legend.position = "none") +
          facet_wrap(~UNSD_SUB_REG, ncol = 3) +
          labs(x = NULL,
               y = paste0(itemName, " (", i, ")"))
          )})
}
graphics.off()
system("evince yieldImputeCheck.pdf&")



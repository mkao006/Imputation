########################################################################
## Title: Imputation and validation methodologies for the FAOSTAT
##        production domain.
## Date:  2013-03-12
########################################################################

library(XML)
library(FAOSTAT)
library(reshape2)
library(zoo)


## Commodity group data take from the SAS script
##---------------------------------------------------------------------

## TODO (Michael): Check which commodity group does 461 belong to
##                 (currently in fruits and other_crops), otherwise it
##                 creates duplicates. We will put it in fruits for now.
tmp = c(cereals = c(15, 27, 44, 75, 56, 71, 79, 83, 89, 101, 92, 94,
                    97, 103, 108),
        pulses = c(176, 187, 191, 195, 201, 181, 197, 210, 205,
                   203, 211),
        roots_tub = c(125, 116, 122, 137, 135, 136, 149),
        treenuts = c(221, 223, 217, 225, 222, 220, 224, 216, 226, 234),
        oilcrops = c(236, 242, 260, 249, 277, 305, 310, 256, 263, 265,
                     267, 270, 275, 280, 289, 292, 296, 299, 328, 329,
                     333, 336, 339),
        veg = c(388, 402, 403, 406, 426, 407, 393, 358, 372, 397, 417,
                414, 423, 420, 366, 367, 399, 449, 401, 373, 394, 446,
                430, 378, 567, 568, 463),
        fruit = c(486, 489, 577, 569, 574, 572, 571, 603, 490, 495, 507,
                  497, 512, 560, 600, 515, 521, 523, 526, 530, 531, 534,
                  536, 544, 547, 552, 554, 558, 550, 549, 592, 587, 542,
                  541, 591, 619),
        stim_beve = c(656, 661, 667, 671, 674),
        spices = c(687, 689, 692, 693, 698, 702, 711, 720, 723),
        tobacco = c(826),
        othr_crops = c(459, 461, 677),
        rubber = c(837, 836),
        leaves = c(748, 754),
        fibres = c(767, 780, 788, 800, 809, 821, 782, 987, 773, 1185, 789),
        othr_meat = c(867, 947, 1035, 977, 1017, 1127, 1097, 1108, 1111,
                      1163, 1166),
        poultry_meat = c(1058, 1069, 1073, 1080, 1141),
        milk = c(882, 951, 982, 1020, 1130), eggs = c(1062, 1091),
        lvstk_prds = c(886, 901),
        sugar = c(162, 163, 1182),
        veg_oils = c(36, 60, 237, 244, 257, 258, 252, 261, 264, 266, 268,
          271, 276, 281, 290, 293, 297, 331, 334, 337, 340, 1242, 278, 307, 313),
        alch_bev = c(564, 51, 66, 82, 86)
  )

comGrp.df = melt(tmp)
comGrp.df$Group = gsub("[0-9]", "", rownames(comGrp.df))
comGrp.df$includeGroup = ifelse(comGrp.df$Group %in% c("lvstk_prds", "sugar",
  "veg_oils", "alch_bev"), FALSE, TRUE)
row.names(comGrp.df) = NULL
comGrp.dt = data.table(comGrp.df)
rm(comGrp.df)
setnames(comGrp.dt, old = colnames(comGrp.dt),
         new = c("itemCode", "comGroup", "includeGroup"))

## Extract the production commodity group from the FAOSTAT 2 website.
doc = htmlParse("http://faostat.fao.org/site/384/default.aspx")
test = getNodeSet(doc, path = "//table")
prodTable.df = readHTMLTable(test[[10]],
  header = c("Group Name", "Item FAO Code", "Item HS+ Code", "Item Name",
    "Definition"), stringsAsFactors = FALSE, skip.rows = 1)
tmp = prodTable.df[, c("Group Name", "Item FAO Code", "Item Name")]



## Read and process the test data
## ---------------------------------------------------------------------
test.dt = data.table(read.csv("nicola_ws_request_12yrsR.csv", header = TRUE,
    stringsAsFactors = FALSE))
setnames(test.dt, old = c("ItemGroup", "AREA", "ITEM", "ELE"),
         new = c("itemGroup", "FAOST_CODE", "itemCode", "elementCode"))

## Remove test area
test.dt = subset(test.dt, FAOST_CODE != 298)

## These countries were excluded (from SAS code)
FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  c(351, 296, 357, 76, 245, 246, 246, 43), ]

## Subset only area harvested (31) and production (51)
## process.dt = subset(test.dt, subset = elementCode == 51)
process.dt = test.dt
process.dt$itemGroup = NULL

## Process the data into the long format
mprocess.df = melt(process.dt, id.var = c("FAOST_CODE", "itemCode", "elementCode"))
mprocess.df$Year = gsub("[^0-9]", "", mprocess.df$variable)
mprocess.df$variable = gsub("[0-9|_]", "", mprocess.df$variable)
cprocess.df = dcast(mprocess.df, FAOST_CODE + itemCode + elementCode + Year ~
    variable)
cprocess.dt = data.table(cprocess.df)
rm(cprocess.df)

## Merge with the regional profile
## TODO (Michael): Need to check for those that doesn't have a region
## TODO (Michael): Need to check the region used for imputation.
cprocess.dt = merge(cprocess.dt,
    data.table(FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG_CODE",
                                    "UNSD_SUB_REG_CODE")]),
    by = "FAOST_CODE", all.x = TRUE)


## Countries/Territory that doesn't have a region under the official
## UNSD definition
FAOcountryProfile[FAOcountryProfile$FAOST_CODE %in%
                  unique(cprocess.dt[is.na(UNSD_MACRO_REG_CODE), FAOST_CODE]),
                  c("FAOST_CODE", "ABBR_FAO_NAME")]

## Manucaly fill the countries which does not have a region based on
## the FAO file from
## (http://faostat.fao.org/site/371/DesktopDefault.aspx?PageID=371)
cprocess.dt[FAOST_CODE == 43, UNSD_MACRO_REG_CODE := as.integer(9)]
cprocess.dt[FAOST_CODE == 51, UNSD_MACRO_REG_CODE := as.integer(2)]
cprocess.dt[FAOST_CODE == 164, UNSD_MACRO_REG_CODE := as.integer(9)]
## No code for Yemen and Democratic Yemen
## cprocess.dt[FAOST_CODE == 246, UNSD_MACRO_REG_CODE]
## cprocess.dt[FAOST_CODE == 247, UNSD_MACRO_REG_CODE]

cprocess.dt[FAOST_CODE == 43, UNSD_SUB_REG_CODE := as.integer(61)]
cprocess.dt[FAOST_CODE == 51, UNSD_SUB_REG_CODE := as.integer(151)]
cprocess.dt[FAOST_CODE == 164, UNSD_SUB_REG_CODE := as.integer(57)]
## No code for Yemen and Democratic Yemen
## cprocess.dt[FAOST_CODE == 246, UNSD_SUB_REG_CODE]
## cprocess.dt[FAOST_CODE == 247, UNSD_SUB_REG_CODE]


## Merge with the commodity group
cprocess.dt = merge(cprocess.dt, comGrp.dt, by = "itemCode", all.x = TRUE)


## items which does not have a commodity group
sort(unique(cprocess.dt[is.na(comGroup), itemCode]))

## Remove entries with no commodity group or not included.
cprocess.dt = subset(cprocess.dt, !is.na(comGroup) & includeGroup)


## Hack the character
## TODO (Michael): Fix this, check why it is converted to character
cprocess.dt$num = as.numeric(cprocess.dt$num)

## Create a column for original value
cprocess.dt$origVal = cprocess.dt$num


## Remove observation marked as "T"
cprocess.dt[cprocess.dt$symb == "T", num := NA]

## Replace duplicated "F" entry with NA
cprocess.dt[duplicated(cprocess.dt[, list(itemCode, FAOST_CODE, elementCode, num,
                                          symb)]) & symb == "F", num := NA]
setkeyv(cprocess.dt, c("FAOST_CODE", "itemCode", "elementCode", "Year"))

## ## Visualise the missing data
## dfViz = function(data){
##   type = sapply(cprocess.dt, typeof)
##   ## num = names(which(sapply(cprocess.dt, typeof) == "double"))
##   ## char = names(which(sapply(cprocess.dt, typeof) == "character"))
##   ## int = names(which(sapply(cprocess.dt, typeof) == "integer"))
##   ## tf = names(which(sapply(cprocess.dt, typeof) == "logical"))
##   nc = NCOL(data)
##   nr = NROW(data)
##   plot.new()
##   plot.window(xlim = c(0, nr), ylim = c(0, nc))
##   for(i in 1:nc){
##     if(type[i] == "double"){
##       points(1:nr, i, pch = 15, cex = 0.5)
##     } else if(type[i] == "integer"){
##       k = as.numeric(as.factor(unlist(data[, i, with = FALSE])))
##       print(unique(k))
##       points(1:nr, rep(i, nr), pch = 15, col = rgb(0, 0, k/max(k),
##                                            maxColorValue = max(k)))
##     }
##   }
## }



## Visualize the missing value
image(data.matrix(is.na(cprocess.dt)))
text(rep(0.5, NCOL(cprocess.dt)), seq(0, 1, length = NCOL(cprocess.dt)),
     labels = paste(colnames(cprocess.dt), " (", round(sapply(X = cprocess.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))

## Check the frequency of the symbols
table(cprocess.dt$symb, useNA = "always")








## Old Imputation
## ---------------------------------------------------------------------

diffv = function(x){
    T = length(x)
    ## Use linear interpolation for weight (this might not be suitable)
    x[x == 0] = NA
    if(sum(!is.na(x)) >= 2)
        ## TODO (Michael): test whether approx or na.approx is faster.
        x = na.approx(x)
    tmp = c(x[2:T]/x[1:(T - 1)])
    tmp
}

## Compute country and commodity specific growth rate
cprocess.dt[, rawGr := c(NA, diffv(num)),
     by = c("FAOST_CODE", "itemCode", "elementCode")]
## cprocess.dt[, rawMin := min(num, na.rm = !all(is.na(num))),
##             by = c("FAOST_CODE", "itemCode", "elementCode")]
## cprocess.dt[, rawMax := max(num, na.rm = !all(is.na(num))),
##             by = c("FAOST_CODE", "itemCode", "elementCode")]

## Country commodity group
cprocess.dt[, countryComSum := sum(num, na.rm = !all(is.na(num))),
     by = c("FAOST_CODE", "Year", "comGroup", "elementCode")]
cprocess.dt[, countryComGr := c(NA, diffv(countryComSum)),
            by = c("FAOST_CODE", "comGroup", "elementCode")]
## cprocess.dt[, countryComImp := c(NA, imp(num[-length(num)], countryComGr[-1])),
##             by = c("FAOST_CODE", "itemCode", "elementCode")]
## cprocess.dt[which(rawMin > countryComImp), countryComImp := NA]
## cprocess.dt[which(rawMax < countryComImp), countryComImp := NA]


## Sub-regional item
cprocess.dt[, subregItemSum := sum(num, na.rm = !all(is.na(num))),
     by = c("UNSD_SUB_REG_CODE", "Year", "itemCode", "elementCode")]
cprocess.dt[, subregItemGr := c(NA, diffv(subregItemSum)),
            by = c("UNSD_SUB_REG_CODE", "itemCode", "elementCode")]
## cprocess.dt[, subregItemImp := c(NA, imp(num[-length(num)],  subregItemGr[-1])),
##             by = c("FAOST_CODE", "itemCode", "elementCode")]
## cprocess.dt[which(rawMin > subregItemImp), subregItemImp := NA]
## cprocess.dt[which(rawMax < subregItemImp), subregItemImp := NA]


## Sub-regional commodity group
cprocess.dt[, subregComSum := sum(num, na.rm = !all(is.na(num))),
     by = c("UNSD_SUB_REG_CODE", "Year", "comGroup", "elementCode")]
cprocess.dt[, subregComGr := c(NA, diffv(subregComSum)),
            by = c("UNSD_SUB_REG_CODE", "comGroup", "elementCode")]
## cprocess.dt[, subregComImp := c(NA, imp(num[-length(num)], subregComGr[-1])),
##             by = c("FAOST_CODE", "itemCode", "elementCode")]
## cprocess.dt[which(rawMin > subregComImp), subregComImp := NA]
## cprocess.dt[which(rawMax < subregComImp), subregComImp := NA]


## Regional item
cprocess.dt[, regItemSum := sum(num, na.rm = !all(is.na(num))),
     by = c("UNSD_MACRO_REG_CODE", "Year", "itemCode", "elementCode")]
cprocess.dt[, regItemGr := c(NA, diffv(regItemSum)),
            by = c("UNSD_MACRO_REG_CODE", "itemCode", "elementCode")]
## cprocess.dt[, regItemImp := c(NA, imp(num[-length(num)], regItemGr[-1])),
##             by = c("FAOST_CODE", "itemCode", "elementCode")]
## cprocess.dt[which(rawMin > regItemImp), regItemImp := NA]
## cprocess.dt[which(rawMax < regItemImp), regItemImp := NA]


## Regional commodity group
cprocess.dt[, regComSum := sum(num, na.rm = !all(is.na(num))),
     by = c("UNSD_MACRO_REG_CODE", "Year", "comGroup", "elementCode")]
cprocess.dt[, regComGr := c(NA, diffv(regComSum)),
            by = c("UNSD_MACRO_REG_CODE", "comGroup", "elementCode")]
## cprocess.dt[, regComImp := c(NA, imp(num[-length(num)], regComGr[-1])),
##             by = c("FAOST_CODE", "itemCode", "elementCode")]
## cprocess.dt[which(rawMin > regComImp), regComImp := NA]
## cprocess.dt[which(rawMax < regComImp), regComImp := NA]

bound = function(x, p = c(0.025, 0.975)){
  if(!all(is.na(x))){
    dist = try(fitdistr(na.omit(x), densfun = "cauchy"))
    bound = qcauchy(p = p, location = dist$estimate[1], scale = dist$estimate[2])
  } else {
    bound = rep(NA, length(bound))
  }
  bound
}

lower.dt = cprocess.dt[!is.na(num), bound(rawGr, p = 0.025),
  by = c("itemCode", "elementCode")]
setnames(lower.dt, old = "V1", new = "lowerBound")
cprocess.dt = merge(cprocess.dt, lower.dt, by = c("itemCode", "elementCode"),
  all.x = TRUE)

upper.dt = cprocess.dt[!is.na(num), bound(rawGr, p = 0.975),
  by = c("itemCode", "elementCode")]
setnames(upper.dt, old = "V1", new = "upperBound")
cprocess.dt = merge(cprocess.dt, upper.dt, by = c("itemCode", "elementCode"),
  all.x = TRUE)

cprocess.dt[countryComGr < lowerBound | countryComGr > upperBound,
            countryComGr := NA]
cprocess.dt[subregItemGr < lowerBound | subregItemGr > upperBound,
            subregItemGr := NA]
cprocess.dt[subregComGr < lowerBound | subregComGr > upperBound,
            subregComGr := NA]
cprocess.dt[regItemGr < lowerBound | regItemGr > upperBound,
            regItemGr := NA]
cprocess.dt[regComGr < lowerBound | regComGr > upperBound,
            regComGr := NA]


## Function to select between different method of imputation
selectImp = function(data, impCol){
  for(i in 1:NROW(data)){
    ind = which(!is.na(data[i, impCol, with = FALSE]))[1]
    if(!is.na(ind)){
      data[i, finalImp := as.numeric(data[i, impCol[ind], with = FALSE])]
    } else {
      data[i, finalImp := as.numeric(NA)]
    }
  }
}

grCol = grep("Gr$", colnames(cprocess.dt), value = TRUE)
impGrCol = grCol[grCol != "rawGr"]
selectImp(cprocess.dt, impCol = impGrCol)


imp = function(value, gr){
  if(all(is.na(value) | value == 0)){
    tmp = value
  } else {
    n = length(value)
    firstObs = which(!is.na(value) & value != 0)[1]
    tmp = double(n)
    tmp[0:(firstObs - 1)] = NA
    for(i in firstObs:n){
      ## This accounts for missing value in the original data, but how
      ## do we account for missing value in the growth rate? Keep it
      ## missing?
      if(is.na(value[i])){
        tmp[i] = tmp[i - 1] * gr[i]
      } else {
        tmp[i] = value[i] * gr[i]
      }
    }
  }
  tmp
}


cprocess.dt[, final := c(NA, imp(num[-length(num)], finalImp[-1])),
            by = c("FAOST_CODE", "itemCode")]
cprocess.dt[, check := final/num]
## TODO (Michael): Use the new data to do this.
  



## Examine the imputation
image(data.matrix(is.na(cprocess.dt)))
text(rep(0.5, NCOL(cprocess.dt)), seq(0, 1, length = NCOL(cprocess.dt)),
     labels = paste(colnames(cprocess.dt), " (", round(sapply(X = cprocess.dt,
       FUN = function(x) 100 * sum(is.na(x))/length(x)), 2), "%)", sep = ""))



## ## Check whether the imputation differ vastly with the original value
## impCheck.dt = subset(cprocess.dt,
##   select = c("FAOST_CODE", "itemCode", "elementCode", "Year", "num",
##     grep("Imp", colnames(cprocess.dt), value = TRUE)))
## impCheck.dt = subset(cprocess.dt,
##   select = c("origVal", grep("Imp", colnames(cprocess.dt), value = TRUE)))
## impCheck.dt[impCheck.dt == 0] = NA
## impCheck.mat = na.omit(data.matrix(impCheck.dt))
## impCheck.mat = (impCheck.mat * 100)/impCheck.mat[, 1]
## image(impCheck.mat, col = heat.colors(3),
##       breaks = quantile(impCheck.mat, probs = c(0, 0.01, 0.99, 1)))
## text(rep(0.5, NCOL(impCheck.dt)), seq(0, 1, length = NCOL(impCheck.dt)),
##      labels = colnames(impCheck.dt))




imped.dt = subset(cprocess.dt, select = c("FAOST_CODE", "itemCode", "elementCode",
                                 "Year", "num", "symb", "origVal",
                                 grep("Imp", colnames(cprocess.dt), value = TRUE)))
area.dt = subset(imped.dt, elementCode == 31)
oldName = c("origVal", "num", "symb", grep("Imp", colnames(cprocess.dt),
  value = TRUE))
setnames(area.dt, old = oldName, new = c(paste(oldName, "Area", sep = "")))
area.dt$elementCode = NULL
prod.dt = subset(imped.dt, elementCode == 51)
setnames(prod.dt, old = oldName, new = c(paste(oldName, "Prod", sep = "")))
prod.dt$elementCode = NULL
final.dt = merge(area.dt, prod.dt,
  by = intersect(colnames(area.dt), colnames(prod.dt)), all = TRUE)

## Entries where there are mismatch between the production and the
## area
subset(final.dt, is.na(symbProd))
subset(final.dt, is.na(symbArea))
subset(final.dt, !is.na(symbProd) & !is.na(symbArea))



selectImp(data = final.dt, impCol = grep("ImpArea", colnames(final.dt),
                                value = TRUE))
setnames(final.dt, old = "finalImp", new = "finalImpArea")
selectImp(data = final.dt, impCol = grep("ImpProd", colnames(final.dt),
                                value = TRUE))
setnames(final.dt, old = "finalImp", new = "finalImpProd")



## Percentage of the missing value imputed
cprocess.dt[is.na(num), 1 - sum(is.na(finalImp))/length(finalImp)]


## Take only the matching item
matchAll.dt = subset(final.dt, !is.na(symbArea) & !is.na(symbProd))

## Compute the yield
matchAll.dt$finalArea = matchAll.dt$numArea
matchAll.dt$finalProd = matchAll.dt$numProd
matchAll.dt[is.na(finalArea), finalArea := finalImpArea]
matchAll.dt[is.na(finalProd), finalProd := finalImpProd]
matchAll.dt[, impYield := finalProd/finalArea]

pdf(file = "yieldCheck.pdf")
for(i in unique(matchAll.dt$itemCode)){
  tmp = subset(matchAll.dt, itemCode == i)
  print(ggplot(data = tmp, aes(x = impYield)) +
        geom_histogram(aes(fill = is.na(numProd) | is.na(numArea))))
}
graphics.off()
system("evince yieldCheck.pdf&")

## Visualise missingness
image(data.matrix(is.na(final.dt)))
text(rep(0.5, length(colnames(final.dt))),
         seq(0, 1, length = length(colnames(final.dt))),
         labels = colnames(final.dt))

with(final.dt, table(is.na(numArea)))
with(final.dt, table(is.na(finalArea)))

with(final.dt, table(is.na(numProd)))
with(final.dt, table(is.na(finalProd)))



## final.dt[, groupYield := sum(numProd, na.rm = TRUE)/sum(numArea, na.rm = TRUE),
##          by = "itemCode"]



## Plot the yield distribution by commodity
final.dt$col = with(final.dt, ifelse(is.na(numProd) | is.na(numArea),
  "red", "black"))

pdf(file = "yieldDist.pdf")
for(i in unique(final.dt$itemCode)){
  ## try(with(final.dt[itemCode == i, ],
  ##      stripchart(calcYield, col = col, method = "stack", at = 0,
  ##        main = unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$itemCode == i,
  ##                "itemName"]))))
  try(print(ggplot(data = final.dt[!is.na(calcYield) & itemCode == i, ],
                   aes(x = calcYield)) +
            geom_bar(aes(fill = col))))
}
graphics.off()


## Data frame where both area and production are missing
miss.dt = subset(final.dt, is.na(numArea) & is.na(numProd))



## Check the imputation
## check.dt = cprocess.dt[, list(FAOST_CODE, itemCode, elementCode, Year, origVal,
##     num, finalImp)]

## ind = unique(check.dt[, list(FAOST_CODE, itemCode, elementCode)])
## pdf(file = "imputationCheck.pdf")
## for(i in 1:NROW(ind)){
##   tmp = subset(check.dt, FAOST_CODE == ind[i, FAOST_CODE] &
##     itemCode == ind[i, itemCode] &
##     elementCode == ind[i, elementCode])
##   try({with(tmp, plot(Year, origVal,
##                  main = paste("Country = ",
##                    FAOcountryProfile[which(FAOcountryProfile$FAOST_CODE ==
##                                            ind[i, FAOST_CODE]), "LAB_NAME"],
##                    ", Item = ",
##            unique(FAOmetaTable$itemTable[which(FAOmetaTable$itemTable$itemCode ==
##                                                 as.character(ind[i, itemCode])),
##                                           "itemName"]), sep = "")))
##   with(tmp, lines(Year, num, col = "red"))
##   with(tmp, points(Year, finalImp, col = "steelblue", pch = 19, cex = 2))
##   with(tmp, lines(Year, finalImp, col = "steelblue", pch = 19, cex = 2))})
## }
## graphics.off()
  



## New Imputation
## ---------------------------------------------------------------------

## Function to compute the ratio between successive years

## NOTE (Michael): The problem with the current formulae is that it
##                 does not account for changes in item in the basket
##                 from year to year. Even if we take into account of
##                 this by computing pair-wise growth, the basket will
##                 still change from year to year because the
##                 pair-wise basket may not be the same.
diffv = function(x){
    T = length(x)
    ## NOTE (Michael): This is a problem!
    x[x == 0] = 1e-5
    ## Use linear interpolation for weight (this might not be suitable)
    if(sum(!is.na(x)) >= 2)
        ## TODO (Michael): test whether approx or na.approx is faster.
        x = na.approx(x)
    tmp = c(x[2:T]/x[1:(T - 1)])
    tmp
}

## Compute country and commodity specific growth rate
cprocess.dt[, data_gr := c(1, diffv(num)),
     by = c("FAOST_CODE", "itemCode", "elementCode")]

## Function to compute the basket aggregate and the imputation
## NOTE (Michael): Might be better to implement the growth rate
##                 directly
nnimp = function(data, index, type_name, dataCol = "num", grCol = "data_gr",
                 includeInter = TRUE){
    setnames(data, old = c(dataCol, grCol), new = c("dataCol", "grCol"))
    ## Calculate the grouped sum
    data[, tmp1 := sum(dataCol, na.rm = !all(is.na(dataCol))),
         by = c(unique(c("Year", "elementCode", index)))]
    ## Calculate the group growth factor
    data[, tmp2 := c(1, diffv(tmp1)),
         by = c(unique(c("FAOST_CODE", "itemCode", "elementCode", index)))]
    ## Calculate the mixed growth based on commodity specific and grouped factor
    data[which(is.na(data[, grCol])),
         tmp3 := data[which(is.na(data[, grCol])), tmp2]]
    data[which(!is.na(data[, grCol])),
         tmp3 := data[which(!is.na(data[, grCol])), grCol]]
    ## Compute the imputation, no imputation is done if all
    ## observation are either zero or NA (i.e. no useful information
    ## available).
    foo = function(val, gr){
        if(all(is.na(val) | val == 0)){
            tmp = val
        } else {
            firstObs = which(!is.na(val) & val != 0)[1]
            tmp = c(rep(NA, firstObs - 1), val[firstObs] *
                cumprod(gr[firstObs:length(gr)]))
        }
        tmp
    }

    data[, tmp4 := foo(val = dataCol, gr = tmp3),
                by = c(unique(c("FAOST_CODE", "itemCode", "elementCode", index)))]
    ## Whether to return the immediate steps for check
    if(includeInter){
        setnames(data, old = c("tmp1", "tmp2", "tmp3", "tmp4", "dataCol", "grCol"),
                 new = c(paste0(type_name, c("_sum", "_gr", "_mixgr", "_imp")),
                         dataCol, grCol))
    } else {
        data[, tmp1 := NULL]
        data[, tmp2 := NULL]
        data[, tmp3 := NULL]
        setnames(data, old = c("tmp4", "dataCol", "grCol"),
                 new = c(paste0(type_name, "_imp"), dataCol, grCol))
    }
}

## Compute the imputation for the 5 different basket
nnimp(data = cprocess.dt, index = c("FAOST_CODE", "comGroup"),
      type_name = "countryComGroup")
nnimp(cprocess.dt, index = c("FAO_SUB_REG", "itemCode"),
      type_name = "subRegionCom")
nnimp(cprocess.dt, index = c("FAO_SUB_REG", "comGroup"),
      type_name = "subRegionComGroup")
nnimp(cprocess.dt, index = c("FAO_MACRO_REG", "itemCode"),
      type_name = "regionCom")
nnimp(cprocess.dt, index = c("FAO_MACRO_REG", "comGroup"),
      type_name = "regionComGroup")

## rows that are imputed
check = subset(cprocess.dt, subset = is.na(num),
    select = c("FAOST_CODE", "Year", "itemCode", "elementCode", "symb", "num",
               "origVal", grep("_imp", colnames(cprocess.dt), value = TRUE)))


## This the case where the basket change
check2 = subset(cprocess.dt, FAOST_CODE == 215 & itemCode == 1091)





## NOTE (Michael): We will not swtich between different imputation
##                 methods, so the cost of NA would be Inf. This
##                 approach is chosen because we prefer to have low
##                 relatitve error rather than low absolute error. It
##                 is true that switching between different
##                 methodology may result in lower absolute error
##                 (with respect to the unobserved data), but from a
##                 modeling approach it may possibly result in in
##                 consistent model. In addition, it is mis-use of
##                 statistic where the data-generating mechanism are
##                 not the same.


## NOTE (Michael): Use the time series which best approximate the
##                 original time series to do the imputation.

## Function to find the best imputation method.
## NOTE (Michael): Need to optimize and merge with nnimp function
optImp = function(data, dataCol, impCol, costFunc){
    FUN = match.fun(costFunc)
    ## noInf = paste(paste0(impCol, " != Inf"), collapse = " & ")
    n_imp = length(impCol)
    cost = double(n_imp)
    for(i in 1:n_imp){
        cost[i] = FUN(x = unlist(data[, dataCol, with = FALSE]),
                      y = unlist(data[, impCol[i], with = FALSE]))
    }
    data$imp_name= impCol[which.min(cost)]
    data$final_imp = data[, impCol[which.min(cost)], with = FALSE]
    data
}


## grType = grep("_gr", colnames(cprocess.dt), value = TRUE)
## baseGr = "data_gr"
## impGr = grType[grType != baseGr]
impType = grep("_imp", colnames(cprocess.dt), value = TRUE)
## noInf = paste(paste0(errorType, " != Inf"), collapse = " & ")

cprocess.dt$key_split =
    with(cprocess.dt, paste(FAOST_CODE, itemCode, elementCode,
                                            sep = "_"))

system.time({
    reduced.lst = split(x = cprocess.dt, cprocess.dt$key_split)
})

## NOTE (Michael): If the cost are all the same for all imputation
##                 method, then the first imputation type
##                 (countryComGrp) will be used.
myCostFun = function(x, y){
    ## Make use of all information by replacing NA's in data
    x = na.locf(x, na.rm = FALSE)
    x[is.na(x)] = mean(na.omit(x))
    ## Fully penalize NA, so a consistent approach is used.
    y[is.na(y)] = Inf
    sqrt(sum((x - y)^2, na.rm = TRUE))
}

## switching strategy for iputation.
fooImp = function(data, impCol){
  for(i in 1:NROW(data)){
    ind = which(!is.na(data[i, impCol, with = FALSE]))[1]
    if(impCol[ind] %in% impCol){
      data[i, "imp2_name"] = impCol[ind]
      data[i, "final_imp2"] = data[i, impCol[ind], with = FALSE]
    } else {
      data[i, "imp2_name"] = NA
      data[i, "final_imp2"] = NA
    }
  }
  data
}


test = lapply(reduced.lst,
    FUN = function(x) optImp(data = x, dataCol = "num", impCol = impType,
    costFunc = myCostFun))

test2 = lapply(test,
  FUN = function(x) fooImp(data = x, impCol = impType))

## Examin the growth rate
pdf(file = "growthExam.pdf")
par(mfrow = c(2, 1))
for(i in 1:100){
  plot(test2[[i]]$data_gr - 1, pch = 19, type = "b")
  hist(test2[[i]]$data_gr - 1, breaks = 5)
}
graphics.off()

## Examine growth of all commodity
uElem = unique(cprocess.dt$itemCode)

pdf(file = "commodityGrCheck.pdf")
for(j in uElem){
  check.dt = subset(cprocess.dt, itemCode == j & num >= 50)
  try({plot(1:5, type = "n", ylim = c(range(check.dt$data_gr - 1, na.rm = TRUE)),
            xlim = c(45, 50))
       for(i in unique(check.dt$FAOST_CODE)){
         lines(check.dt[check.dt$FAOST_CODE == i, Year],
               check.dt[check.dt$FAOST_CODE == i, data_gr] - 1)
       }
     })
}
graphics.off()
system("evince commodityGrCheck.pdf&")




system.time({
    test3 = Reduce(f = function(x, y) rbind(x, y), x = test2[-1],
                   init = test2[[1]])
})

## This is shows that by replacing NA's in data gives a good
## approximation.
subset(test3, FAOST_CODE == 99 & itemCode == 645)

subset(test3, num == 0 & Year == 45)

## Percentage not imputed
sum(is.na(test3$final_imp))/sum(is.na(test3$num))
sum(is.na(test3$final_imp2))/sum(is.na(test3$num))

## check the difference between the switching strategy and the
## constant strategy

check = subset(test3, imp_name != imp2_name,
  select = c("FAOST_CODE", "itemCode", "elementCode", "Year", "num",
    "key_split", "final_imp", "final_imp2"))

mcheck = melt(check, id.var = c("FAOST_CODE", "itemCode", "elementCode",
                       "Year", "key_split"))

ukey = unique(mcheck$key_split)
pdf(file = "strategyCheck.pdf", width = 10)
set.seed(100)
for(i in sample(1:length(ukey), 500)){
print(ggplot(data = mcheck[mcheck$key_split == ukey[i], ],
             aes(x = as.numeric(Year) + 1960, y = value)) +
      geom_line(aes(col = variable)) +
      labs(x = "Year", y = NULL, title = ukey[i]))
}
graphics.off()
system("evince strategyCheck.pdf &")

checkSet = c("105_358_31", "126_292_31", "238_687_31", "26_567_31",
  "203_748_31", "238_655_31", "54_638_31", "121_641_31",
  "26_402_31", "137_1062_31", "9_536_31", "93_459_31")
checkSet.df = subset(test3, key_split %in% checkSet,
  select = c("FAOST_CODE", "itemCode", "elementCode", "Year",
    "key_split", "final_imp", "final_imp2"))
mcheckSet.df = melt(checkSet.df,
  id.var = c("FAOST_CODE", "itemCode", "elementCode",
                       "Year", "key_split"))
mcheckSet.df$variable = 
  ifelse(mcheckSet.df$variable == "final_imp", "Single", "Switching")

ukey = unique(mcheckSet.df$key_split)

pdf(file = "strategyCheckSet.pdf", width = 10)
for(i in 1:length(ukey)){
print(ggplot(data = mcheckSet.df[mcheckSet.df$key_split == ukey[i], ],
             aes(x = as.numeric(Year) + 1960, y = value)) +
      geom_line(aes(col = variable)) +
      geom_point(aes(col = variable)) + 
      labs(x = "Year", y = NULL, title = ukey[i]))
}
graphics.off()
system("evince strategyCheckSet.pdf &")







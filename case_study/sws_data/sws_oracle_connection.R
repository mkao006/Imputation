########################################################################
## Title: Script to connect to the current sws working system
## Date: 2013-11-07
########################################################################

## Load the library and establish the connection.
## NOTE (Michael): the ojdbc14.jar and internal connection is required.
library(RJDBC)
drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
    classPath = "ojdbc14.jar")
conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
    user = "demo", password = "demo")

## Example query
years = paste("num_", 1961:2011, collapse = ", ", sep = "")
elements = "31, 51"
items = "15"

test =
    dbGetQuery(conn,
               paste0("SELECT area, item, ele, ",
                     years,
                     " FROM tsv_ics_work_yr WHERE ele in (",
                     elements,
                     ") AND item in (",
                     items,
                     ") ORDER BY area, item, ele"))




getSWSProduction = function(itemCode, elementCode, year, connection){
    years = paste("num_", year, collapse = ", ", sep = "")
    symbs = paste("symb_", year, collapse = ", ", sep = "")
    elements = paste(elementCode, collapse = ", ")
    items = paste(itemCode, collapse = ", ")
    dbGetQuery(connection,
               paste0("SELECT area, item, ele, ",
                      years,", ", symbs,
                      " From tsv_ics_work_yr WHERE ele in (",
                      elements,
                      ") AND item in (",
                      items,
                      ") ORDER BY area, item, ele"))
}

test = getSWSProduction(itemCode = 15, elementCode = c(31, 51),
    year = 1961:2011, connection = conn)


## Save all production in the crop domain
library(FAOSTAT)
source("../../support_functions/toLowerCamel.R")
allItemTable =
    unique(FAOmetaTable$itemTable[FAOmetaTable$itemTable$domainCode == "QC",
                                  c("itemCode", "itemName")])
for(i in 1:NROW(allItemTable)){
    print(allItemTable[i, ])
    tmp = try(
        getSWSProduction(itemCode = as.numeric(allItemTable[i, "itemCode"]),
                         elementCode = c(31, 51),
                         year = 1961:2011,
                         connection = conn)
        )
    if(!inherits(tmp, "try-error") & NROW(tmp) > 0){
        meltTmp = melt(tmp, id.vars = c("AREA", "ITEM", "ELE"))
        splits = strsplit(x = as.character(meltTmp$variable), split = "\\_")
        meltTmp$TYPE = sapply(splits, FUN = function(x) x[1])
        meltTmp$YEAR = sapply(splits, FUN = function(x) as.numeric(x[2]))
        meltTmp$variable = NULL
        castTmp = dcast(meltTmp, AREA + YEAR + ITEM ~ ELE + TYPE,
            value.var = "value")
        colnames(castTmp)[1:3] = c("areaCode", "year", "itemCode")
        colnames(castTmp) = gsub("51", "production", colnames(castTmp))
        colnames(castTmp) = gsub("31", "areaHarvested", colnames(castTmp))
        colnames(castTmp) = gsub("_NUM", "Value", colnames(castTmp))
        colnames(castTmp) = gsub("_SYMB", "Symb", colnames(castTmp))
        try({
            castTmp$areaHarvestedValue =
                as.numeric(castTmp$areaHarvestedValue)
            })
        try({
            castTmp$productionValue =
                as.numeric(castTmp$productionValue)
        })
        write.csv(castTmp,
                  file = paste0("../../sua_data/",
                      toLowerCamel(allItemTable[i, "itemName"]),
                      "SUA.csv"),
                  row.names = FALSE, na = "")
    } else {
        print("this query failed")
    }
}








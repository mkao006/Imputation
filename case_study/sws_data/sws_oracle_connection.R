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
    elements = paste(elementCode, collapse = ", ")
    items = paste(itemCode, collapse = ", ")
    dbGetQuery(connection,
               paste0("SELECT area, item, ele, ",
                      years,
                      " From tsv_ics_work_yr WHERE ele in (",
                      elements,
                      ") AND item in (",
                      items,
                      ") ORDER BY area, item, ele"))
}

test = getSWSProduction(itemCode = 15, elementCode = c(31, 51),
    year = 1961:2011, connection = conn)


## Tests
## library(FAOSTAT)
## source("../../toLowerCamel.R")
## allItemTable = FAOmetaTable$itemTable
## for(i in 1:NROW(allItemTable)){
##     print(allItemTable[i, ])
##     tmp = try(getSWSProduction(itemCode =
##         as.numeric(allItemTable[i, "itemCode"]), elementCode = c(31, 51),
##         year = 1961:2011, connection = conn))
##     if(!inherits(tmp, "try-error")){
##         meltTmp = melt(tmp, id.vars = c("AREA", "ITEM", "ELE"))
##         meltTmp$variable = as.numeric(gsub("NUM_", "", meltTmp$variable))
##         colnames(meltTmp) = c("areaCode", "itemCode", "elementCode",
##                     "Year", toLowerCamel(allItemTable[i, "itemName"]))
##         write.csv(meltTmp,
##                   file = paste0("./sua_download/",
##                       toLowerCamel(allItemTable[i, "itemName"]),
##                       "SUA.csv"),
##                   row.names = FALSE, na = "")
##     } else {
##         print("this query failed")
##     }
## }





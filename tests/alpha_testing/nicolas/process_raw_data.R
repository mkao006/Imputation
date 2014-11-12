## Load the libraries
library(faoswsUtil)
library(data.table)
library(reshape2)
library(RJDBC)
library(FAOSTAT)

## Set the commodity group to extract and impute
splitDataDirectory = paste0("./split_data/", commodityFolder, "/")

## Connect to the database
drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
    classPath = "~/ojdbc14.jar")
conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
    user = "demo", password = "demo")

## Function to obtain the items within the commodity group
getCommodityList = function(conn, commodityGroupCode){  
    query = paste0("SELECT item, long_name_e
                    FROM item
                    WHERE item in (
                    SELECT item_memb
                    FROM item_agg_grp
                    WHERE item_grp=", commodityGroupCode, 
                    ")
                    ORDER by item")
    commodityList = dbGetQuery(conn, query)
    colnames(commodityList) = c("itemCode", "itemName")
    commodityList
}

## Function to get the data
getProductionData = function(conn, itemCode, yearRange){
    query = paste0("SELECT area, item, ele, ",
                    paste0(c("num_", "symb_"), rep(yearRange, each = 2),
                           collapse = ", "),
                    " FROM tsv_ics_work_yr
                    WHERE item in (", paste0(itemCode, collapse = ","),")
                    AND ele in (31, 41, 51)
                    ORDER BY area, item, ele")
    production = dbGetQuery(conn, query)
    meltedProduction =
        melt(production, id.var = c("AREA", "ITEM", "ELE"))
    colnames(meltedProduction)[1:3] =
        c("areaCode", "itemCode", "elementCode")    
    meltedProduction$year =
        as.numeric(gsub("[^0-9]", "" , meltedProduction$variable))
    meltedProduction$Type =
        tolower(gsub("[0-9|_]", "" , meltedProduction$variable))
    meltedProduction$variable = NULL
    finalProduction =
        dcast(meltedProduction, areaCode + itemCode + elementCode +
                  year ~ Type, value.var = "value")
    finalProduction$num = as.numeric(finalProduction$num)
    data.table(finalProduction)
}

              

## Function to process the data into the desired format
dataProcess = function(data){    
    countryNameTable =
        data.table(
            FAOcountryProfile[, c("FAOST_CODE", "FAO_TABLE_NAME")]
        )
    setnames(countryNameTable, old = c("FAOST_CODE", "FAO_TABLE_NAME"),
             new = c("areaCode", "areaName"))
    itemNameTable =
        data.table(
            unique(FAOmetaTable$itemTable[, c("itemCode", "itemName")])
        )
    itemNameTable[, itemCode := as.numeric(itemCode)]
    named_data=
        merge(merge(data,
                    itemNameTable, by = "itemCode", all.x = TRUE),
              countryNameTable, by = "areaCode", all.x = TRUE)
    tmp = data.table(recast(data = named_data,
        id.var = c("areaCode", "areaName", "itemCode", "itemName",
            "elementCode",  "year"),
        formula = areaCode + areaName + itemCode + itemName + year ~
        elementCode + variable, measure.var = c("num", "symb")))
    setnames(tmp,
             old = colnames(tmp),
             new = c("areaCode", "areaName", "itemCode", "itemName",
                 "year", "areaHarvestedValue", "areaHarvestedFlag",
                 "yieldValue", "yieldFlag",
                 "productionValue", "productionFlag"))
    tmp[, areaHarvestedValue := as.numeric(areaHarvestedValue)]
    tmp[, productionValue := as.numeric(productionValue)]
    tmp[, yieldValue := as.numeric(yieldValue)/10000]  
    tmp
}


## Save the raw data into the repository
if(!file.exists(splitDataDirectory))
    dir.create(splitDataDirectory)

commodityList = getCommodityList(conn, commodityCode)
## commodityList = read.csv(file = "vegetable_primary_1735_test_set.csv")

lapply(commodityList$itemCode,
       FUN = function(x){
           Data =
               try(getProductionData(conn, itemCode = x,
                                 yearRange = 1995:2013))
           if(!inherits(Data, "try-error")){
               processsedData = dataProcess(Data)
               write.csv(processsedData,
                         file = paste0(splitDataDirectory,
                             unique(processsedData$itemName), ".csv"),
                         row.names = FALSE, na = "")
           }
       }
       )

library(reshape2)

dataPath = "../sua_data"
dataFile = dir(dataPath)

cat("\\documentclass{article}\n",
    "\\begin{document}\n",
    file = "symbolTableGraph.tex")


for(i in dataFile){
    cat(gsub("SUA\\.csv",  "", i), file = "symbolTableGraph.tex",
        append = TRUE)
    try({
        myFile = paste0(dataPath, "/", i)
        wheat.dt = data.table(read.csv(myFile, stringsAsFactors = FALSE))
        areaHarvestedTable =
          dcast(data.frame(table(wheat.dt[, list(areaHarvestedSymb, year)])),
                year ~areaHarvestedSymb, value.var = "Freq")

        productionTable =
            dcast(data.frame(table(wheat.dt[, list(productionSymb, year)])),
                  year ~productionSymb, value.var = "Freq")
        colnames(productionTable)[-1] =
            paste0(" ", colnames(productionTable)[-1], " ")

        finalTable = merge(areaHarvestedTable, productionTable, by = "year")
        print(xtable(finalTable), file = "symbolTableGraph.tex",
              append = TRUE, include.rownames = FALSE)
    })
}



cat("\\end{document}", file = "symbolTableGraph.tex", append = TRUE)

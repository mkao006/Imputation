## Building the package
## ---------------------------------------------------------------------

library(roxygen2)
library(data.table)

## Remove the folder if it exists

if(file.exists("./faoswsProductionImputation"))
    unlink("faoswsProductionImputation", recursive = TRUE)

## Build the package
package.skeleton("faoswsProductionImputation",
                 code_files = paste("./codes/",
                     dir("./codes/", pattern = "\\.R$"), sep = ""),
                 force = FALSE)

## build the flag table data
dir.create("faoswsProductionImputation/data")
okrapd =
    data.table(read.csv(file = "./api_integration/Okra.csv",
                        header = TRUE))
save(okrapd,
     file = "faoswsProductionImputation/data/okrapd.RData")


## Include the DESCRIPTION file
file.copy(from = "./DESCRIPTION", to = "faoswsProductionImputation/",
          overwrite = TRUE)
unlink("./faoswsProductionImputation/Read\\-and\\-delete\\-me")

## Use roxygen to build the documentation
roxygenize("faoswsProductionImputation")

## Include vignette
dir.create("./faoswsProductionImputation/vignettes/")
dir.create("./faoswsProductionImputation/inst/")
dir.create("./faoswsProductionImputation/inst/doc/")
file.copy(from = "./api_integration/api_vignette.pdf",
          to = "./faoswsProductionImputation/inst/doc/",
          overwrite = TRUE)
file.copy(from = "./api_integration/api_vignette.Rnw",
          to = "./faoswsProductionImputation/vignettes/",
          overwrite = TRUE)
file.copy(from = "./methodology_paper/V4.1/methodology.pdf",
          to = "./faoswsProductionImputation/inst/doc/",
          overwrite = TRUE)
cat("%\\VignetteIndexEntry{Statistical Working Paper on Imputation Methodology for the FAOSTAT Production Domain}\n%\\VignetteEngine{knitr::knitr}\n\\documentclass{article}\n\\begin{document}\n\\end{document}", file = "./faoswsProductionImputation/vignettes/methodology.Rnw")

## Build and check the package
system("R CMD INSTALL --build faoswsProductionImputation")
system("R CMD build --no-build-vignettes faoswsProductionImputation")
## system("R CMD check --as-cran faoswsProductionImputation")




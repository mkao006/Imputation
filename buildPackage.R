## Building the package
## ---------------------------------------------------------------------

library(roxygen2)

## Remove the folder if it exists
if(file.exists("./faoswsProductionImputation"))
    unlink("faoswsProductionImputation", recursive = TRUE)

## Build the package
package.skeleton("faoswsProductionImputation",
                 code_files = paste("./codes/",
                     dir("./codes/", pattern = "\\.R$"), sep = ""),
                 force = FALSE)

## Include the DESCRIPTION file
file.copy(from = "./DESCRIPTION", to = "faoswsProductionImputation/",
          overwrite = TRUE)
unlink("./faoswsProductionImputation/Read\\-and\\-delete\\-me")

## Use roxygen to build the documentation
roxygenize("faoswsProductionImputation")
unlink("./faoswsProductionImputation/inst/", recursive = TRUE)

## Build and check the package
system("R CMD INSTALL --build faoswsProductionImputation")
system("R CMD build faoswsProductionImputation")
system("R CMD check --as-cran faoswsProductionImputation")


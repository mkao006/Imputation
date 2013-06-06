Imputation for the FAOSTAT production domain
=======

This is the **github** repository for the development of the FAOSTAT
production imputation methodology.

==============================================================================

**Documentation**

The documentation of the current methodology can be found in the
*methodology_paper* folder. The pdf can be regenerated with the
following command.

```r
library(knitr)
knit("methodology.Rnw")
system("pdflatex \\nonstopmode\\input methodology.tex")
```

**Previous Methodology**

All relevant material of the previous methodology can be allocated in
the *previous_methodology* folder.

**Example**

The *fullWheat.R* contains the full process from data manipulation,
imputation to simulation.
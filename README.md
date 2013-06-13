Imputation for the FAOSTAT production domain
=======

This is the **github** repository for the development of the FAOSTAT
production imputation methodology.

==============================================================================

**Documentation**

The documentation of the current methodology can be found in the
*methodology_paper* folder. The pdf can be regenerated with the
following command. (**Latex** is required)

```r
library(knitr)
knit("methodology.Rnw")
system("pdflatex \\nonstopmode\\input methodology.tex")
```

**Previous Methodology**

All relevant material of the previous methodology can be allocated in
the *previous_methodology* folder.

**Proposed Methodology and Case Example**

There are three code scripts to perform the full imputation case study
for *wheat*.

1. **wheatDataManipulation.R** - This script reads and performs data
manipulation of the SUA data into the desired format.

2. ** wheatImputation.R** - This script follows the previous script
and performs the imputation and examination.

3. **wheatSimulation.R** - This script also follows the data
manipulation code and runs the out-of-sample simulation.

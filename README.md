Imputation for the FAOSTAT production domain
=======

This is the **github** repository for the development of the FAOSTAT
production imputation methodology.

==============================================================================

**Documentation**

The documentation of the current methodology can be found in the
*methodology_paper* folder. The pdf can be regenerated with the
following command. (*LaTeX* is required)

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
for the wheat data (*fullWheat.csv*)

1. *wheatDataManipulation.R* - This script reads and performs data
manipulation of the SUA data into the desired format.

2. *wheatImputation.R* - This script follows the previous script
and performs the imputation and examination.

3. *wheatSimulation.R* - This script also follows the data
manipulation code and runs the out-of-sample simulation.


**Supplementary script**

These are the functions that performs or assists the imutation process.

1. *computeYield.R* - This function is used to compute the yield and
to avoid infinity or NaN.

2. *naiveImpute.R* - This function performs the naive imputation, that
is, linear interpolation followed by last/first observation carrid
forward/backward.

3. *lmeImpute.R* - This is the core function which performs the
imputation based on linear mixed model with EM estimation of the
average value.

4. *FAOProductionImputation.R* - This is a wrapper function of the
*lmeImpute* function which imputes the area, production and yield for
a single commodity.
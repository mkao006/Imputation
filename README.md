# Imputation for the FAOSTAT production domain

This is the **github** repository for the development of the FAOSTAT
production imputation methodology.


### Installation
To install the imputation package, simply run the following command

```r
library(devtools)
install_github(repo = "sws_imputation", 
	       username = "mkao006", subdir = "swsImputation")
```


### Documentation

The documentation of the current methodology can be found in the
*methodology_paper* folder. The pdf can be regenerated with the
following command. (*LaTeX* is required)

```r
library(knitr)
knit("methodology_revised.Rnw")
system("pdflatex \\nonstopmode\\input methodology_revised.tex")
```

### Previous Methodology

All relevant material of the previous methodology can be allocated in
the `previous_methodology` folder.

### Proposed Methodology and Case study


### Core functions

The core functions to carry out the whole imputation process, from
imputation, diagnosis and simulation can be found in the `code`
folder.

`computeYield.R` - This function is used to compute the yield and
to avoid infinity or NaN.

`naiveImputation.R` - This function performs the naive imputation, that
is, linear interpolation followed by last/first observation carrid
forward/backward.

`meanlme4.R` - This is the core function which performs the
imputation based on linear mixed model with EM estimation of the
average value.

`predict.meanlme4.R` - The prediction function for the imputation
model from meanlme4 function.

`swsImputation.R` - This is a wrapper function of the
*meanlme4* function which imputes the area, production and yield for
a single commodity.

`impDiag.R` - A function to plot several diagnostic plots.

`impFit.R` - A function to plot the imputed production, area
harvested, and yield against the official and semi-official figures.

`impBootPred.R` - The function performs bootstrap to estimate the
prediction error.
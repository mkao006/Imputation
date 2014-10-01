# FAOSTAT Production Domain Imputation

This is the **github** repository for the development of the FAOSTAT
production imputation methodology.




### Installation
To install the imputation package, simply run the following command. 

```r
library(devtools)

## The package depends on the faoswsFlag package
install_github(repo = "sws_r_api", username = "mkao006", 
               subdir = "faoswsFlag/faoswsFlag")

## The imputation package
install_github(repo = "sws_imputation", 
	       username = "mkao006", 
	       subdir = "faoswsProductionImputation")
```

#### Vignettes

There are two vignettes associated with the package, one for the
methodology while the other for the step-by-step tutorial of the
functionality.

We recommend to skim through the methodlogy paper before proceeding to
the tutorial.

The methodology paper can be accessed via the vignette.

```r
vignette(topic = "methodology", package = "faoswsProductionImputation")
```

The step by step explaination of functionality of the package is
described in a separate documentation also shipped with the pacakge.

```r
vignette(topic = "faoswsProductionImputation", package = "faoswsProductionImputation")
```



### Documentation

All relevant documentation of the current methodology can be found in
the `documentation` folder. There are three folder in this directory.

* `faoswsProductionImputation` - This is the repository containing the
  vignette *faoswsProductionImputation*
* `methodology` - This is the repository containing the vignette
  *methodology*, the version number corresponds to different version
  of the packages.
* `previous_methodology` - This folder contains all the relevant
  materials on the previous methodology.

### Presentation

The `presentation` folder contains previous and current presentation
on the methodology for a speedy introduction.


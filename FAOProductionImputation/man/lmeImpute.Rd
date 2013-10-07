\name{lmeImpute}
\alias{lmeImpute}
\title{This function performs imputation by linear mixed model}
\usage{
  lmeImpute(fixed, random, groupVar = c(year, region),
    Data, n.iter = 1000, tol = 1e-06)
}
\arguments{
  \item{fixed}{The fixed effect of the model excluding the
  grouping variable.}

  \item{random}{The random effect of the model}

  \item{group}{The grouped effect of the model, the mean
  are computed based on this formula}

  \item{Data}{the data.frame or data.table containing the
  data}

  \item{n.iter}{The number of iteration for the
  EM-algorithm for estimating the grouped average effect.}

  \item{tol}{The tolerance, stopping criteria for the
  likelihood.}
}
\description{
  There are three imputation method associated with the
  function, the function will select whether between the
  null model excluding the grouped average fixed effect and
  the full model.
}
\seealso{
  \code{\link{FAOProductionImpute}}
}


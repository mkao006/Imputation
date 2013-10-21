\name{meanlme4}
\alias{meanlme4}
\title{This function performs imputation by linear mixed model}
\usage{
  meanlme4(formula, groupVariable, countryVar, data,
    n.iter, tol, EMverbose = TRUE)
}
\arguments{
  \item{formula}{See the formula of lme4}

  \item{groupVariable}{The grouped effect of the model, the
  mean are computed based on this formula}

  \item{countryVar}{The variable which defines the country,
  which act as the conditional variable in the random
  effect.}

  \item{data}{the data.frame or data.table containing the
  data}

  \item{n.iter}{The number of iteration for the
  EM-algorithm for estimating the grouped average effect.}

  \item{tol}{The tolerance, stopping criteria for the
  likelihood.}

  \item{EMverbose}{logical, whether the likelihood in the
  EM step should be returned.}
}
\description{
  This function performs imputation by linear mixed model
}
\seealso{
  \code{\link{FAOProductionImpute}}
}


\name{meanlme4}
\alias{meanlme4}
\title{This function performs imputation by linear mixed model}
\usage{
  meanlme4(formula, groupVar, countryVar, data, n.iter,
    tol, EMverbose = TRUE, includeMean = TRUE,
    allow.new.levels = FALSE)
}
\arguments{
  \item{formula}{See the formula of lme4}

  \item{groupVar}{The grouped effect of the model, the mean
  are computed based on this formula}

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

  \item{includeMean}{logical, whether the grouped mean
  should be used.}

  \item{allow.new.levels}{logical, whether observation from
  new levels can be predicted.}
}
\description{
  This function performs imputation by linear mixed model
}
\seealso{
  \code{\link{FAOProductionImpute}}
}


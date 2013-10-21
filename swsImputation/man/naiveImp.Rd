\name{naiveImp}
\alias{naiveImp}
\title{This function computes the naive imputation}
\usage{
  naiveImp(x)
}
\arguments{
  \item{x}{The series to put imputed}
}
\description{
  The function performs imputation by linear interpolation
  followed by last observation carry forward and backwards.
}
\examples{
rand = rnorm(20)
(rand[sample(1:length(rand), size = 10)] = NA)
naiveImp(rand)
}


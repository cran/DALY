\name{aggregate.DALY}
\alias{aggregate.DALY}

\title{Aggregate method for class 'DALY'}

\description{
  Aggregate \emph{DALY Calculator} output by outcome, age/sex class, or both.
}

\usage{
\method{aggregate}{DALY}(x, by = NULL, \dots)
}

\arguments{
  \item{x }{Object of class \code{'DALY'}}
  \item{by }{Any of \code{c("outcome", "class", "total")}}
  \item{\dots }{Further arguments passed to or from other methods}
}

\value{
  For \code{by == "outcome"}:
  \item{\emph{i} }{List containing vectors of DALYs, YLDs, YLLs, Cases and Deaths
                   for each outcome \emph{\code{i}}}
  \item{pop }{Population matrix}
  \item{name }{Name of the disease}
  
  For \code{by == "class"}:
  \item{DALY }{Vector of DALYs aggregated by age/sex class}
  \item{YLD }{Vector of YLDs aggregated by age/sex class}
  \item{YLL }{Vector of YLLs aggregated by age/sex class}
  \item{Cases }{Vector of Cases aggregated by age/sex class}
  \item{Deaths }{Vector of Deaths aggregated by age/sex class}
  \item{pop }{Population matrix}
  \item{name }{Name of the disease}
  
  For \code{by == "total"}:
  \item{DALY }{Vector of DALYs aggregated by outcome and age/sex class}
  \item{YLD }{Vector of YLDs aggregated by outcome and age/sex class}
  \item{YLL }{Vector of YLLs aggregated by outcome and age/sex class}
  \item{Cases }{Vector of Cases aggregated by outcome and age/sex class}
  \item{Deaths }{Vector of Deaths aggregated by outcome and age/sex class}
  \item{pop }{Population matrix}
  \item{name }{Name of the disease}
}


\author{
  Brecht Devleesschauwer,
  Arie Havelaar,
  Juanita Haagsma,
  Nicolas Praet,
  Niko Speybroeck

  \strong{Maintainer:} Brecht Devleesschauwer \email{brecht.devleesschauwer@ugent.be}
}

\seealso{
  \code{\link{print.DALY}},
  \code{\link{summary.DALY}}\cr
  
  \code{\link{DALYcalculator}}
  (for a brief description of the \emph{DALY Calculator})\cr
  \code{\link{DALYmanual}}
  (for a more comprehensive overview)
}

\examples{
\dontrun{
## load the Neurocysticercosis example
setDALYexample(1)

## initiate the Monte Carlo simulation process
## and store results in object 'x'
x <- getDALY()

## store simulations aggregated by outcome and age/sex class
y <- aggregate(x, by = "total")

summary(y$DALY)  # default summary of total DALYs
hist(y$Deaths)   # default histogram of total Deaths
}
}

\keyword{output}

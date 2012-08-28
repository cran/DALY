\name{print.DALY}
\alias{print.DALY}

\title{Print method for class 'DALY'}

\description{
  Print summary of \emph{DALY Calculator} output aggregated by age/sex class.
}

\usage{
\method{print}{DALY}(x, relative = FALSE, outcomes = FALSE, prob = 0.95, digits = 0, \dots)
}

\arguments{
  \item{x }{Object of class \code{'DALY'}}
  \item{relative }{Show results per 1000 population? See details below}
  \item{outcomes }{Show results per outcome? See details below}
  \item{prob }{Probability span by printed credibility intervals;
               defaults to \code{.95}}
  \item{digits }{Number of decimals to be printed; defaults to \code{0}}
  \item{\dots }{Further arguments passed to or from other methods}
}

\details{
  The standard \code{print} method gives the absolute number of
  DALYs, YLDs, YLLs, Cases and Deaths. By specifying \code{relative = TRUE},
  the number of DALYs, YLDs, YLLs, Cases and Deaths per 1,000 population will be shown.\cr

  The standard \code{print} method gives the number of
  DALYs, YLDs, YLLs, Cases and Deaths aggregated over all health outcomes.
  By specifying \code{outcomes = TRUE}, the number of DALYs, YLDs, YLLs,
  Cases and Deaths for each outcome will be shown as well.\cr
}

\value{
  \code{print.DALY} returns a list containg the following elements:
  \item{total }{Matrix containing mean, median and \code{prob}
                credible interval of total DALYs, YLDs, YLLs, Cases and Deaths}
  \item{outcomes }{Named list, containing matrices of DALYs, YLDs, YLLs,
                   Cases and Deaths for each health outcome}
  \item{pct }{Vector of contribution of YLDs and YLLs to overall DALYs}
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
  \code{\link{summary.DALY}},
  \code{\link{aggregate.DALY}}\cr
  
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

## view the DALY calculation results
print(x)                   # absolute, total
print(x, prob = .90)       # absolute, total (90\% credible intervals)
print(x, relative = TRUE)  # relative (ie, per 1000 population)
print(x, outcomes = TRUE)  # outcome-wise
}
}

\keyword{output}

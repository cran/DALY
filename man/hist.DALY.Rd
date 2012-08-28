\name{hist.DALY}
\alias{hist.DALY}

\title{Histogram method for class 'DALY'}

\description{
  Plot a standardized histogram of \emph{DALY Calculator} output.
}

\usage{
\method{hist}{DALY}(x, xval = "DALY", prob = 0.95, central = "mean", \dots)
}

\arguments{
  \item{x }{Object of class \code{'DALY'}}
  \item{xval }{Value to plot on x-axis;
               must be any of \code{c("DALY", "YLD", "YLL", "Cases", "Deaths")}}
  \item{prob }{Probability span by plotted credibility interval;
              defaults to \code{.95}}
  \item{central }{Plotted central value of distribution;
                 must be any of \code{c("Mean", "Median")}}
  \item{\dots }{Further arguments passed to or from other methods}
}

\details{
  This function plots a standardized histogram of \emph{DALY Calculator} output.
  The histogram contains by default 25 cells. The limits of the credible interval
  are denoted by vertical lines; the credible interval limits and central tendency
  are printed above the histogram. An empirical density curve is plotted on top of
  the histogram.
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

## plot histogram of total deaths
## 90\% credible interval + median
hist(x, xval = "Deaths", prob = 0.90, central = "median")
}
}

\keyword{output}

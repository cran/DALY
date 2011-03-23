\name{getDALY}

\alias{getDALY}

\title{initiate the Monte Carlo simulation process}

\description{
	This function initiates the Monte Carlo simulation process, based on the entered data, and returns the simulated values.
}

\usage{getDALY(button.call = FALSE)}

\arguments{
	\item{button.call}{Gives information on the origin of the call (i.e., through the GUI button (\code{TRUE}) or the \R Console (\code{FALSE}))}
}

\value{
	\item{DALY}{Vector of simulated total DALYs}
	\item{YLD}{Vector of simulated total YLDs}
	\item{YLL}{Vector of simulated total YLLs}
	\item{Deaths}{Vector of simulated total deaths}
	\item{Name}{Name of the disease, as entered by the user}
	\item{Outcome\strong{x}}{List containing the vectors of simulated DALYs, YLDs, YLLs and deaths for disease category or outcome \strong{x}, as well as the outcome name as entered by the user}
}

\author{Brecht Devleesschauwer, Arie Havelaar, Juanita Haagsma, Nicolas Praet, Niko Speybroeck}

\examples{
## load the Neurocysticercosis example
setDALYexample(1)

## initiate the Monte Carlo simulation process, and store results in object x
x <- getDALY()

## view the available sub-objects
names(x)

## view the results
print(x)

## generate a 90% DALY credible interval
quantile(x$DALY,c(.05,.95))
}
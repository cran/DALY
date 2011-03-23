\name{DALYcalculator}

\alias{DALYcalculator}

\title{DALY Calculator}

\description{
	This function opens the main window of the \emph{DALY Calculator}.\cr\cr
	After initiating the \emph{DALY Calculator} main window, the following steps have to be followed to calculate DALYs:
	\enumerate{
		\item{Set the \sQuote{Population} table}
		\item{Set the \sQuote{Life Expectancy} table - the default life expectancy table is the Coale and Demeny model life table West, level 25 and 26}
		\item{Set the input parameters and corresponding distributions and stratification levels}
		\item{Set the age weighting and time discounting rates}
		\item{Calculate DALYs !}
	}
	In order to demonstrate the calculation process of the \emph{DALY Calculator}, two examples from the foodborne diseaese burden literature are built-in:
	\itemize{
		\item{The burden of \emph{Taenia solium} cysticercosis in Cameroon (Praet et al., 2009)}
		\item{Congenital toxoplasmosis and DALYs in the Netherlands (Kortbeek et al., 2009)}
	}
	In the following sections, a brief summary will be given of the functionalities of the \emph{DALY Calculator}.
	A more comprehensive overview is provided in the \strong{DALY Calculator manual}, which is available through the \code{\link{DALYmanual}} function.
}

\usage{DALYcalculator()}

\details{
	The \emph{DALY Calculator} provides the following three menu options:
	\cr\cr
	\strong{File}
	\itemize{
		\item{\strong{Load DALY data from file\dots} reads population and epidemiological data from a tab-delimited text file (through \code{\link{readDALYdata}})}
		\item{\strong{Save DALY data to file\dots} saves the population and epidemiological data to a tab-delimited text file (through \code{\link{saveDALYdata}})}
		\item{\strong{Reset DALY calculator} clears all entered data, and resets the \emph{DALY Calculator} to its default settings, except for the life expectancy table (through \code{\link{reset}})}
		\item{\strong{Exit} exits the \emph{DALY Calculator}, without saving the entered data}
	}


	\strong{Settings}
	\itemize{
		\item{\strong{Life Expectancy Table\dots} opens the life expectancy table window (through \code{\link{setLifeExp}})}
		\item{\strong{Options\dots} opens the options window (through \code{\link{DALYoptions}})}
	}


	\strong{Help}
	\itemize{
		\item{\strong{Load examples} loads one of the two built-in examples (through \code{\link{setDALYexample}})}
			\enumerate{
				\item{The burden of Neurocysticercosis in West-Cameroon}
				\item{The burden of Congenital Toxoplasmosis in the Netherlands}
			}
		\item{\strong{Html help} opens the HTML help file for the \emph{DALY Calculator} (\emph{cfr} \code{?DALYcalculator})}
		\item{\strong{DALY calculator manual} opens the \strong{DALY Calculator Manual} (through \code{\link{DALYmanual}})}
		\item{\strong{Package description} opens the HTML help file for the \strong{DALY} package (\emph{cfr} \code{?"DALY-package"})}
		\item{\strong{DALY calculator Info} version and developers info}
	}
}

\author{Brecht Devleesschauwer, Arie Havelaar, Juanita Haagsma, Nicolas Praet, Niko Speybroeck}

\references{
	\itemize{
		\item{Praet N, Speybroeck N, Manzanedo R, Berkvens D, Nforninwe DN, Zoli A, Quet F, Preux P-M, Carabin H, Geerts S (2009). The disease burden of \emph{Taenia solium} cysticercosis in Cameroon. \emph{PLoS Neglected Tropical Diseases 3(3),} e406.}
		\item{Kortbeek LM, Hofhuis A, Nijhuis CDM, Havelaar AH (2009). Congenital toxoplasmosis and DALYs in the Netherlands. \emph{Memorias de Instituto Oswaldo Cruz 104(2),} 370-373.}
		\item{\url{http://studwww.ugent.be/~bdvleess/DALYcalculator}}
	}
}
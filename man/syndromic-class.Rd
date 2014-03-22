\docType{class}
\name{syndromic-class}
\alias{syndromic}
\alias{syndromic-class}
\title{Class \code{"syndromic"}}
\description{
  Main class of the package, stores syndromic surveillance
  objects.
}
\details{
  The \code{syndromic} class stores observed data in a
  format appropriate for monitoring, and contains several
  slots to store input and outputs of analysis (temporal
  monitoring). Functions are available to create an object
  of the class syndromic from data already cleaned and
  prepared for monitoring, or alternatively from raw
  observed data.
}
\section{Slots}{
  \describe{ \item{observed}{ A \code{matrix} with the
  number of rows equal to the number of time points
  available (p.e. the number of days of observed data); and
  number of columns equal to the number of syndromes
  monitored.  } \item{dates}{ A \code{DataFrame} which
  first column contains the dates corresponding to the
  observations recorded. Additional columns contain
  additional information extracted from the date, such as
  day-of-the-week, month, holidays, etc.  }
  \item{baseline}{ A \code{matrix} of dimensions exactly
  equal to the slot
}

\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{syndromic(observed, dates, ...)}
}
\examples{
## Load data
data(observed)
my.syndromic <- syndromic(observed,min.date="01/01/2010",max.date="28/05/2013")
my.syndromic <- syndromic(observed[1:5,],min.date="01/01/2010",max.date="05/01/2010")
my.syndromic <- syndromic(observed[1:6,],min.date="01/01/2010",max.date="08/01/2010",
                          weekends=FALSE)
dates = seq(from=as.Date("01/01/2010",format ="\%d/\%m/\%Y" ),
              to=as.Date("05/01/2010",format ="\%d/\%m/\%Y" ),
              by="days")
my.syndromic <- syndromic(observed[1:5,],dates=dates)
}
\keyword{classes}


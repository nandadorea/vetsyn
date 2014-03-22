\docType{methods}
\name{dim-methods}
\alias{dim}
\alias{dim-methods}
\alias{dim,syndromic-method}
\title{\code{dim}}
\value{
  a vector with three items: \describe{ \item{1}{ The
  number of time points monitored (the number of rows for
  all slots of the object).  } \item{2}{ The number of
  syndromic groups monitored, as determined by the number
  of columns in the slot \code{observed}.  } \item{3}{ The
  number of detection algorithms used, as determined by the
  third dimension of the slot \code{alarms}.  }
}
\description{
  Get dimension information for a syndromic object.
}
\examples{
data(lab.daily)
my.syndromic <- raw_to_syndromic (id=lab.daily$SubmissionID,
                                  syndromes.var=lab.daily$Syndrome,
                                  dates.var=lab.daily$DateofSubmission,
                                  date.format="\%d/\%m/\%Y")
## create a subset that only includes the first 10 time points
## (all the slots are trimmed from rows 1 to 10)
## note the use of "," instead of ":"
subset <- my.syndromic[1,10]
}
\keyword{methods}


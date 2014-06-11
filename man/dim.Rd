\docType{methods}
\name{dim}
\alias{dim}
\title{Get dimension information for a syndromic object (\code{syndromicD} or
\code{syndromicW}).}
\value{
  a vector with three items: (1) The number of time points
  monitored (the number of rows for all slots of the
  object); (2)The number of syndromic groups monitored, as
  determined by the number of columns in the slot observed;
  (3) The number of detection algorithms used, as
  determined by the third dimension of the slot alarms.

  a vector with three items: (1) The number of time points
  monitored (the number of rows for all slots of the
  object); (2)The number of syndromic groups monitored, as
  determined by the number of columns in the slot observed;
  (3) The number of detection algorithms used, as
  determined by the third dimension of the slot alarms.
}
\description{
  Get dimension information for a syndromic object
  (\code{syndromicD} or \code{syndromicW}).
}
\examples{
data(lab.daily)
my.syndromicD <- raw_to_syndromicD (id=lab.daily$SubmissionID,
                                  syndromes.var=lab.daily$Syndrome,
                                  dates.var=lab.daily$DateofSubmission,
                                  date.format="\%d/\%m/\%Y")
dim(my.syndromicD)
columns <- dim(my.syndromicD)[2]
print(columns)
data(lab.daily)
my.syndromicW <- rawD_to_syndromicW (id=lab.daily$SubmissionID,
                                  syndromes.var=lab.daily$Syndrome,
                                  dates.var=lab.daily$DateofSubmission,
                                  date.format="\%d/\%m/\%Y")
dim(my.syndromicW)
columns <- dim(my.syndromicW)[2]
print(columns)
}
\keyword{methods}


\docType{methods}
\name{plot_syndromic}
\alias{plot_syndromic}
\alias{plot_syndromic-methods}
\alias{plot_syndromic,syndromic-method}
\title{\code{plot_syndromic}}
\arguments{
  \item{x}{a \code{syndromic} object.}

  \item{syndromes}{an optional parameter, if not specified,
  all columns in the slot \code{observed} (or
  \code{baseline} of the \code{syndromic} object will be
  used. The user can choose to restrict the plotting to a
  few syndromic groups listing their name or column
  position in the \code{observed} matrix. See examples.}

  \item{window}{the number of time points to plot, always
  finishing at the last time point recorded.}

  \item{baseline}{whether to plot the baseline, by default
  equal to TRUE.}

  \item{UCL}{the dimension of the slot UCL, from the
  syndromic object, from which the user wants to plot the
  UCL. Set to NULL or to 0 if it is not desired to plot the
  UCL.}

  \item{algorithms}{an optional parameter specifying which
  dimensions of the}

  \item{limit}{an optional parameter establishing a score
  above which alarms are considered worth of notice. Notice
  that this is not a statistical value, but a score
  equivalent to the number of algorithms employed and the
  number of limits set to them. See the tutorial for
  details.}
}
\description{
  A more specific plotting option, with more control over
  the options of which elements of the syndromic object to
  plot. The user can use the general "plot" function of R
  for a syndromic object as well.
}
\examples{
data(lab.daily)
my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                 syndromes.var=Syndrome,
                                 dates.var=DateofSubmission,
                                 date.format="\%d/\%m/\%Y",
                                 remove.dow=c(6,0),
                                 add.to=c(2,1),
                                 data=lab.daily)
my.syndromic <- holt_winters_synd(x=my.syndromic,
                                evaluate.window=30,
                                frequency=5,
                                baseline.window=260)
plot_syndromic(x=my.syndromic,
              syndromes=c(1,3))
plot_syndromic(x=my.syndromic,
              syndromes=c(1,3),
              limit=NULL)
}
\seealso{
  \code{\link{syndromic}}
}
\keyword{methods}


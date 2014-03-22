\docType{methods}
\name{clean_baseline_perc-methods}
\alias{clean_baseline_perc}
\alias{clean_baseline_perc-methods}
\alias{clean_baseline_perc,syndromic-method}
\title{\code{clean_baseline_perc}}
\arguments{
  \item{x}{a \code{syndromic} object, which must have at
  least the slot of observed data and a data.frame in the
  slot dates.}

  \item{syndromes}{an optional parameter, if not specified,
  all columns in the slot \code{observed} of the
  \code{syndromic} object will be used. The user can choose
  to restriict the analyses to a few syndromic groups
  listing their name or column position in the
  \code{observed} matrix. See examples.}

  \item{limit}{the percentile to be used in identifying
  outliers.}

  \item{run.window}{the number of time points to construct
  the moving percentile window. By default 120 days.}

  \item{plot}{whether plots comparing observed data and the
  result of the cleaning process should be displayed.}
}
\value{
  An object of the class syndromic which contains all
  elements form the object provided in x, but in which the
  slot \code{baseline} has been filled with an
  outbreak-free baseline for each syndromic group. When the
  user chooses to restrict analyses to some syndromes, the
  remaining columns are kept as is (if the slot was not
  empty) or filled with zeros when previously empty.
}
\description{
  Function to retrospectively remove possible outbreak
  signals and excessive noise, producing an \code{outbreak
  free baseline} that will serve to train outbreak-signal
  detection algorithms during prospective analysis.
}
\details{
  The cleaning is non-parametric, based on moving
  percentiles. The user sets a window of time points,
  around each time point, which will be used to calculate
  the percentile set in the user in the argument
  \code{limit}. Any observations falling outside that
  percentile are removed and substituted by the percentile
  itself. See examples and references.
}
\examples{
data(lab.daily)
my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                 syndromes.var=Syndrome,
                                 dates.var=DateofSubmission,
                                 date.format="\%d/\%m/\%Y",
                                 merge.weekends=TRUE,
                                 data=lab.daily)
my.syndromic <- clean_baseline_perc(my.syndromic)
my.syndromic <- clean_baseline_perc(my.syndromic,run.window=90)
my.syndromic <- clean_baseline_perc(my.syndromic,
                              syndromes="Musculoskeletal")
my.syndromic <- clean_baseline_perc(my.syndromic,
                              syndromes=c("GIT","Musculoskeletal"))
my.syndromic <- clean_baseline_perc(my.syndromic,
                              syndromes=3)
my.syndromic <- clean_baseline_perc(my.syndromic,
                              syndromes=c(1,3))
}
\references{
  Fernanda C. Dorea, Crawford W. Revie, Beverly J. McEwen,
  W. Bruce McNab, David Kelton, Javier Sanchez (2012).
  Retrospective time series analysis of veterinary
  laboratory data: Preparing a historical baseline for
  cluster detection in syndromic surveillance. Preventive
  Veterinary Medicine. DOI:
  10.1016/j.prevetmed.2012.10.010.
}
\seealso{
  \code{\link{syndromic}}
}
\keyword{methods}


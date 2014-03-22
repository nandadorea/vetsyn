\docType{methods}
\name{clean_baseline-methods}
\alias{clean_baseline}
\alias{clean_baseline-methods}
\alias{clean_baseline,syndromic-method}
\title{\code{clean_baseline}}
\arguments{
  \item{x}{a \code{syndromic} object, which must have at
  least the slot of observed data and a data.frame in the
  slot dates.}

  \item{syndromes}{an optional parameter, if not specified,
  all columns in the slot \code{observed} of the
  \code{syndromic} object will be used. The user can choose
  to restrict the analyses to a few syndromic groups
  listing their name or column position in the
  \code{observed} matrix. See examples.}

  \item{family}{the GLM distribution family used, by
  default \code{"poisson"}. if \code{"nbinom"} is used, the
  function glm.nb is used instead.}

  \item{limit}{the confidence interval to be used in
  identifying outliers.}

  \item{formula}{the regression formula to be used. The
  following arguments are accepted: trend (for a monotonic
  trend), month, dow (day of week), sin, cos, Ar1
  (auto-regressive for 1 days) to AR7. These elements can
  be combined into any formula. The default is
  formula="dow+sin+cos+Ar1+Ar2+AR3+AR4+AR5". See examples.}

  \item{plot}{whether plots comparing observed data and the
  result of the cleaning process should be displayed.}

  \item{print.model}{whether the result of model fitting
  should be printed on the console. This is recommended
  when the user is exploring which dependent variables to
  keep or drop.}
}
\value{
  An object of the class \code{syndromic} which contains
  all elements from the object provided in x, but in which
  the slot \code{baseline} has been filled with an
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
  The cleaning is based on fitting the complete time series
  using regression methods (by default Poisson regression,
  but any other glm family is accepted, extended to
  negative binomial using the package fitdistrplus), and
  then removing any observations that fall outside a given
  confidence interval (set by the user). These observations
  are substituted by the model prediction for that time
  point.
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
my.syndromic <- clean_baseline(my.syndromic)
my.syndromic <- clean_baseline(my.syndromic, formula="dow+month+year")
my.syndromic <- clean_baseline(my.syndromic, formula="dow+sin+cos+trend")
my.syndromic <- clean_baseline(my.syndromic,
                              syndromes="Musculoskeletal")
my.syndromic <- clean_baseline(my.syndromic,
                              syndromes=c("GIT","Musculoskeletal"))
my.syndromic <- clean_baseline(my.syndromic,
                              syndromes=3)
my.syndromic <- clean_baseline(my.syndromic,
                              syndromes=c(1,3))

my.syndromic <- clean_baseline(my.syndromic,
                              family="nbinom")
my.syndromic <- clean_baseline(my.syndromic,
                              syndromes="Musculoskeletal",family="nbinom")
my.syndromic <- clean_baseline(my.syndromic,
                              syndromes=c("GIT","Musculoskeletal"),family="nbinom")
my.syndromic <- clean_baseline(my.syndromic,
                              syndromes=3,family="nbinom")
my.syndromic <- clean_baseline(my.syndromic,
                              syndromes=c(1,3),family="nbinom")
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


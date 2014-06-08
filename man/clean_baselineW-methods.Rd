\docType{methods}
\name{clean_baselineW-methods}
\alias{clean_baselineW}
\alias{clean_baselineW-methods}
\alias{clean_baselineW,syndromic-method}
\title{\code{clean_baselineW}}
\arguments{
  \item{x}{a \code{syndromicW} object, which must have at
  least the slot of observed data and a data frame in the
  slot dates.}

  \item{syndromes}{an optional parameter, if not specified,
  all columns in the slot observed of the syndromic object
  will be used. The user can choose to restrict the
  analyses to a few syndromic groups listing their name or
  column position in the observed matrix. See examples.}

  \item{family}{the GLM distribution family used, by
  default "poisson". if "nbinom" is used, the function
  glm.nb is used instead.}

  \item{limit}{the confidence interval to be used in
  identifying outliers.}

  \item{formula}{the regression formula to be used. The
  following arguments are accepted: trend (for a monotonic
  trend), year, sin, cos, AR1 (auto-regressive for 1 week)
  to AR4. These elements can be combined into any formula.
  The default is formula="trend+sin+cos". See examples.}

  \item{frequency}{the frequency of repetition in the data,
  by default 52 (one year).}

  \item{plot}{whether plots comparing observed data and the
  result of the cleaning process should be displayed.}

  \item{print.model}{whether the result of model fitting
  should be printed on the console. This is recommended
  when the user is exploring which dependent variables to
  keep or drop.}
}
\value{
  An object of the class syndromicW which contains all
  elements from the object provided in x, but in which the
  slot baseline has been filled with an outbreak-free
  baseline for each syndromic group. When the user chooses
  to restrict analyses to some syndromes, the remaining
  columns are kept as is (if the slot was not empty) or
  filled with NAs when previously empty.
}
\description{
  Function to retrospectively remove possible outbreak
  signals and excessive noise, producing an outbreak free
  baseline that will serve to train outbreak-signal
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
my.syndromicW <- rawD_to_syndromicW (id=SubmissionID,
                                 syndromes.var=Syndrome,
                                 dates.var=DateofSubmission,
                                 date.format="\%d/\%m/\%Y",
                                 data=lab.daily)
my.syndromicW <- clean_baselineW(my.syndromicW)
my.syndromicW <- clean_baselineW(my.syndromicW, formula="sin+cos")
my.syndromicW <- clean_baselineW(my.syndromicW,
                              syndromes="Musculoskeletal")
my.syndromicW <- clean_baselineW(my.syndromicW,
                              syndromes=c("GIT","Musculoskeletal"))
my.syndromicW <- clean_baselineW(my.syndromicW,
                              syndromes=3)
my.syndromicW <- clean_baselineW(my.syndromicW,
                              syndromes=c(1,3))

my.syndromicW <- clean_baselineW(my.syndromicW,
                              family="nbinom")
my.syndromicW <- clean_baselineW(my.syndromicW,
                              syndromes="Musculoskeletal",family="nbinom")
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
  \code{\link{syndromicW}}
}
\keyword{methods}


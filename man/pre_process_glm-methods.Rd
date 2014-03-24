\docType{methods}
\name{pre_process_glm-methods}
\alias{pre_process_glm}
\alias{pre_process_glm-methods}
\alias{pre_process_glm,syndromic-method}
\title{\code{pre_process_glm}}
\arguments{
  \item{x}{a \code{syndromic} object, which must have at
  least the slot of observed data and a data.frame in the
  slot dates.}

  \item{slot}{the slot in the \code{syndromic} object to be
  processed, by default, "observed", but this argument can
  be used to change it to "baseline"}

  \item{syndromes}{an optional parameter, if not specified,
  all columns in the slot \code{observed} (or
  \code{baseline} if that was chosen in the previous
  parameter) of the \code{syndromic} object will be used.
  The user can choose to restrict the analyses to a few
  syndromic groups listing their name or column position in
  the \code{observed} matrix. See examples.}

  \item{family}{the GLM distribution family used, by
  default \code{"poisson"}. if \code{"nbinom"} is used, the
  function glm.nb is used instead.}

  \item{formula}{the regression formula to be used. The
  following arguments are accepted: trend (for a monotonic
  trend), month, dow (day of week), sin, cos, Ar1
  (auto-regressive for 1 days) to AR7. These elements can
  be combined into any formula. The default is
  formula="dow+sin+cos+Ar1+Ar2+AR3+AR4+AR5". See examples.}

  \item{period}{in case pre-processing is applied using
  "glm" AND the sin?cos functions are used, the cycle of
  repetitions need to be set. The default is 365, for
  yearly cycles.}

  \item{plot}{whether plots comparing observed data and the
  result of the pre-processing should be displayed.}

  \item{print.model}{whether the result of model fitting
  should be printed on the console. This is recommended
  when the user is exploring which dependent variables to
  keep or drop.}
}
\value{
  A matrix with all the pre-processed vectors.
}
\description{
  Function to remove known temporal effects from time
  series. It fits a glm model to the time series, and
  delivers the residuals.
}
\details{
  This function is provided for users interested in
  capturing (saving or plotting) the results of this
  pre-processing step. However, in the context of syndromic
  surveillance through objects of the class
  \code{syndromic}, pre-processing is performed in
  conjunction with the application of control-charts,
  saving results into an object of the class
  \code{syndromic} (which does not contain slots to save
  pre-processed data, as pre-processing is done within
  detection algorithms.)
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
pre_processed_data <- pre_process_glm(my.syndromic)
pre_processed_data <- pre_process_glm(my.syndromic,
                              syndromes="Musculoskeletal")
pre_processed_data <- pre_process_glm(my.syndromic,
                              syndromes=c("GIT","Musculoskeletal"))
pre_processed_data <- pre_process_glm(my.syndromic,
                              syndromes=3)
pre_processed_data <- pre_process_glm(my.syndromic
                              syndromes=c(1,3))

pre_processed_data <- pre_process_glm(my.syndromic,
                              family="nbinom")
pre_processed_data <- pre_process_glm(my.syndromic,slot="baseline")
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


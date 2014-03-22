\docType{methods}
\name{cusum_synd}
\alias{cusum_synd}
\alias{cusum_synd-methods}
\alias{cusum_synd,syndromic-method}
\title{\code{cusum_synd}}
\arguments{
  \item{x}{a \code{syndromic} object. If pre-processing
  using regression is going to be used, the slot dates must
  contain a data.frame containing at least the columns for
  the regression variables chosen to be used (year, dow,
  month).}

  \item{syndromes}{an optional parameter, if not specified,
  all columns in the slot \code{observed} of the
  \code{syndromic} object will be used. The user can choose
  to restrict the analyses to a few syndromic groups
  listing their name or column position in the
  \code{observed} matrix. See examples.}

  \item{evaluate.window}{the number of time points to be
  evaluated. By default only the last time point is
  evaluated, but the user can set any window (as long as
  the number of time points in the time series allows so).}

  \item{baseline.window}{The baseline used to train the
  algorithm in order to provide a forecast, which will
  serve to decide whether the current observed data is
  expected. Normally 1-2 years.}

  \item{limit.sd}{The limit of detection to be used, that
  is, the cut-off of the confidence interval that decides
  when an observed is abnormal, provided in number of
  standard deviations. This can be provided as a single
  value or as a vector. When a vector is provided, multiple
  detection results are given, and the alarm result stored
  is a sum of how many detection limits were met.}

  \item{alarm.dim}{The \code{syndromic} object is set to
  accept the result of multiple detection algorithms. These
  results are stored as a third dimension in the slot
  alarms. Here the user can choose which order in that
  dimension should store the results of this algorithm.}

  \item{guard.band}{The number of time units used to
  separate the current time unit evaluated and the baseline
  window. The default is 7 (assuming weekly data).}

  \item{correct.baseline}{besides detecting abnormal
  observations, the algorithm can also be used to correct
  the data, removing these observations and substituting
  them by the limit of the confidence interval of the
  prediction provided by the cusum() algorithm. If that is
  to be carried out, the user needs to specify which of the
  provided limits is to be used for the correction. This
  variable should be filled with the INDEX of the limit to
  be used. For example, if limit.sd was provided as
  "c(2.5,3.0,3.5)", the use of correct.baseline=1 will
  cause the algorithm to substitute any observations above
  2.5 standard deviations from the predicted value with
  this exact cut-off limit. If using correct.baseline=2,
  only observations above a standard deviation of 3
  (limit.sd[2]) will be corrected. To avoid that a baseline
  is generated or modified, set this argument to zero or
  NULL.}

  \item{UCL}{the minimum number that would have geerated an
  alarm, for every time point, can be recorded in the slot
  UCL of the \code{syndromic} object.The user must provide
  the INDEX in the limit.sd vector for which the UCL values
  should be corrected (as explained for the argument
  correct.baseline above). Set to FALSE to prevent the
  recording.}

  \item{LCL}{default is FALSE. If set to an index of
  limit.sd (see UCL above) then alarms are also generated
  when the observed number of events is LOWER than
  expected, and the maximum number of observations that
  would have generated a low alarm is recorded in the slot
  LCL. In this case alarms are recorded as -1 for each
  detection limit met.}

  \item{pre.process}{whether to pre-process the time series
  in order to remove temporal effects before applying the
  control-chart. Set to FALSE to apply the control-chart to
  the original, observed time series, using the data in the
  slot baseline as training (if the slot is empty, observed
  data will be copied into it). Set to "diff" to apply
  simple differencing. Set to "glm" to apply a regression
  model and deliver only the residuals to the
  control-chart. The next arguments set details of either
  method.}

  \item{diff.window}{only relevant if "pre.process" is set
  to "diff". Corresponds to the number of time units of
  differencing, default is 7 (weekly differencing). Change
  to 5 if weekends do not contain weekend days.}

  \item{family}{when using pre-processing using glm, the
  GLM distribution family used, by default
  \code{"poisson"}. if \code{"nbinom"} is used, the
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
}
\value{
  An object of the class \code{syndromic} which contains
  all elements from the object provided in x, but in which
  the slot \code{alarm} has been filled in the following
  way: for the rows assigned in \code{evaluate.window},
  columns indicated in \code{syndromes} (or all columns
  from observed if syndromes is left undefined), and for
  the third dimension specified in \code{alarm.dim} (1 by
  default), zeros have been assigned if no alarm was
  generated; otherwise a numerical value gives the number
  of alarms detected. That is, how many of the limits given
  in \code{limits.sd} detected an abnormal observation. See
  examples.  If the user sets a correct.baseline value, the
  baseline will also have been modified.
}
\description{
  This function applies the \code{cusum()} algorithm
  (available in the {qcc} R package. Here it is employed as
  part of an iterative process to allow detection of
  outbreak signals. The additional features compared to the
  regular \code{cusum()} algorithm are:
}
\details{
  \describe{ \item{pre-processing}{Instead of applying
  cusum directly to the time-series, it is possible to
  choose one of two pre-processing methods: (1) modeling
  and removing temporal effects with a GLM regression model
  (families "poisson","nbinom" or "gaussian"); (2)
  differencing to remove for instance day-of-week effects.
  The user can of course also set pre-processing to FALSE,
  and apply no temporal effects removal to the data.}
  \item{iterative application} {the algorithm is applied to
  a range of time points in an iterative manner, so if
  syndromic data needs to be evaluated for the past 30
  days, for instance, the function is called once and the
  internal loops evaluate one day at a time.}
  \item{Detection of deviations one day at a time} {in this
  implementation rather than running the algorithm to
  multiple time units in a "batch", it applies the
  algorithm one time unit (e.g., day) at a time, so that
  aberrations detected in any given time unit can be
  corrected, before proceeding to the next. The correction
  of aberrations can be performed using this algorithm, or
  if the time series has already been corrected using
  another algorithm (with results saved in the slot
  \code{baseline} of the \code{syndromic} object being
  analysed), the corrected baseline will always considered
  as trainig data, rather than the observed data (which may
  contain aberrations)} \item{guard-band}{The user can set
  a guard-band between the time unit being evaluated and
  the start of the window used as training data, in order
  to avoid contamination of the baseline with undetected
  outbreak-signals}.  \item{recording of the detection
  limits}{that is already a feature of the \code{cusum()}
  function, and in the syndromic application the LCL and
  UCL limits are stored in the appropriate slot of the
  object \code{syndromic}. The main innovation here is that
  if pre-processing methods are being used, the LCL and UCL
  are recorded after transformation of the values back to
  the scale of the original data, rathee than being
  recorded in the scale of the residuals of pre-processing,
  which are the actual values used by the control-chart
  method.} \item{data correction}{in case an observation is
  found to be greater than the confidence interval of the
  forecast, the user can choose to update the outbreak-free
  baseline by substituting the observed value with the UCL
  value. As mentioned before, this feature should not be
  used if the baseline was already constructed using
  another algorithm} \item{multiple limits}{the user can
  apply the algorithm with multiple detection limits - that
  is to say, different confidence intervals} }
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
my.syndromic <- cusum_synd(x=my.syndromic,
                         syndrome="Musculoskeletal",
                         evaluate.window=30,
                         baseline.window=260,
                         limit.sd=c(2.5,3,3.5),
                         guard.band=5,
                         correct.baseline=FALSE,
                         alarm.dim=4,
                         pre.process="glm",
                         family="nbinom",
                         formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
                         period=260)

my.syndromic2 <- cusum_synd(x=my.syndromic,
                          syndrome= c(1,2,4,5),
                          evaluate.window=30,
                          baseline.window=260,
                          limit.sd=c(2.5,3,3.5),
                          guard.band=5,
                          correct.baseline=FALSE,
                          alarm.dim=4,
                          pre.process="diff",
                          diff.window=5)
}
\seealso{
  \link{holt_winters_synd}

  pre_process_glm

  cusum

  holt_winters_synd
}
\keyword{methods}


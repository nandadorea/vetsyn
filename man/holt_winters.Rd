\docType{methods}
\name{holt_winters}
\alias{holt_winters}
\alias{holt_winters,syndromic-method}
\alias{holt_winters-methods}
\title{\code{holt_winters}}
\arguments{
  \item{x}{a \code{syndromic} object, which must have the
  slot of observed data, and a data.frame containing the
  variable \code{year} in the slot dates. If the slot
  baseline is not available, then the data in in the slot
  observed will be copied into the baseline slot.}

  \item{syndromes}{an optional parameter, if not specified,
  all columns in the slot \code{observed} of the
  \code{syndromic} object will be used. The user can choose
  to restrict the analyses to a few syndromic groups
  listing their name or column position in the
  \code{observed} matrix. See examples.}

  \item{evaluate.window}{the number of time points to be
  evaluated. By default only the last time point is
  evaluated, but the user can set any window (as long as
  the number of time points in the time series allows so).
  Please note that the HoltWinters algorithm requires at
  least two cycles to initialize. That is, if the cycle of
  the data is weekly, then the two initial weeks of the
  data cannot be included int he analyses.}

  \item{frequency}{The frequency of cycles in the time
  series. Even though this is normally a year
  (frequency=365), we have found that the algorithm works
  better with a frequency equal to the week (5 or 7 days,
  dependening on whether weekends are included) when strong
  day-of-week effects are present in the data.}

  \item{baseline.window}{The baseline used to train the
  algorithm in order to provide a forecast, which will
  serve to decide whether the current observed data is
  expected. Normally 1-2 years.}

  \item{limit.sd}{The limit of detection to be used, that
  is, the cut-off of the confidence interval that decides
  when an observed is abnormal. Rather than a percentage
  (for instance 95% or 99%), this should be informed as
  number of standard deviations above the mean (for
  instance 2.5, or 3). This is to ensure comparability with
  the other detection algorithms used this package (control
  charts). This can be provided as a single value or as a
  vector. When a vector is provided, multiple detection
  results are given, which are store is successive columns
  in the slot alarms.}

  \item{alarm.dim}{The \code{syndromic} object is set to
  accept the result of multiple detection algorithms. These
  results are stored as a third dimension in the slot
  alarms. Here the user can choose which order in that
  dimension should store the results of this algorithm.}

  \item{nahead}{how many days ahead predictions should be
  made. One-day ahead predictions may have narrower
  confidence intervals, but they are unsafe as they do not
  protect the prediction from incoporating undetected
  outbreak signals. A one week ahead prediction (5 or 7
  days) is recommended.}

  \item{alpha}{the alpha parameter to be passed to the
  HoltWinters() algorithm.}

  \item{beta}{the beta parameter to be passed to the
  HoltWinters() algorithm.}

  \item{gamma}{the gamma parameter to be passed to the
  HoltWinters() algorithm.}

  \item{seasonal}{the seasonal parameter to be passed to
  the HoltWinters() algorithm. Deault is "additive". The
  user can change to "multiplicative", but that is not
  recommended if the data contains zeros.}

  \item{correct.baseline}{besides detecting abnormal
  observations, the algorithm can also be used to correct
  the data, removing these observations and substituting
  them by the limit of the confidence interval of the
  prediction provided by the HoltWinters() algorithm. If
  that is to be carried out, the user needs to specify
  which of the provided limits is to be used for the
  correction. This variable should be filled with the INDEX
  of the limit to be used. For example, if limit.sd was
  provided as "c(2.5,3.0,3.5)", the use of
  correct.baseline=1 will cause the algorithm to substitute
  any observations above 2.5 standard deviations from the
  predicted value with this exact cut-off limit. If using
  correct.baseline=2, only observations above a standard
  deviation of 3 (limit.sd[2]) will be corrected. To avoid
  that a baseline is generated or modified, set this
  argument to zero or NULL.}

  \item{UCL}{if TRUE (default) besides signaling whether an
  unusual observation was found, the minimum number that
  would have geerated an alarm, for every time point, is
  also recorded in the slot UCL of the \code{syndromic}
  object.}
}
\value{
  An object of the class \code{syndromic} which contains
  all elements form the object provided in x, but in which
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
  This function applies the \code{Holt-Winters()} algorithm
  (available in the {stats} R package available in the
  basic R installation)). This algorithm is normally used
  for decomposition and forecasting, but here it is
  employed as part of an iterative process to allow
  detection of outbreak signals. The additional features
  compared to the basic \code{HoltWinters()} algorithm are:
}
\details{
  \describe{ \item{iterative application} {the algorithm is
  applied to a range of time points in an iterative manner,
  so if syndromic data needs to be evaluated for the past
  30 days, for instance, the function is called once and
  the internal loops evaluate one day at a time.}
  \item{Detection of deviations} {in this implementation
  the \code{n} days ahead forecast of the algorithm is used
  as an outbreak signal detector - observations above a
  confidence interval are flagged as "aberrations". }
  \item{baseline}{it is possible to point to an
  outbrak-free time series (baseline) which will serve to
  train the algorithm and calculate the forecast, which is
  then compared to the actual observed data that is being
  analysed} \item{recording of the detection limits}{the
  minimum value that would have generated an alarm in each
  time point can be recorded in the UCL slot of the
  \code{syndromic} object provided} \item{data
  correction}{in case an observation is found to be greater
  than the confidence interval of the forecast, the user
  can choose to update the outbreak-free baseline by
  substituting the observed value withteh UCL value}
  \item{multiple limits}{the user can apply the algorithm
  with up to five different detection limits - that is to
  say, different confidence intervals} }
}
\examples{
data(lab.daily)
my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                 syndromes.var=Syndrome,
                                 dates.var=DateofSubmission,
                                 date.format="\%d/\%m/\%Y",
                                 merge.weekends=TRUE,
                                 data=lab.daily)
my.syndromic <- holt_winters(x=my.syndromic,
                            evaluate.window=30,
                            frequency=5,
                            baseline.window=260,
                            limit.sd=c(2.5,3,3.5), #default
                            nahead=5,
                            correct.baseline=2,
                            alarm.dim=1)

my.syndromic <- holt_winters(x=my.syndromic,
                            syndromes="Musculoskeletal",
                            evaluate.window=30,
                            frequency=5,
                            baseline.window=260,
                            limit.sd=c(2.5,3,3.5), #default
                            nahead=5,
                            correct.baseline=2,
                            alarm.dim=1)
}
\seealso{
  \code{\link{syndromic}}

  HoltWinters
}
\keyword{methods}


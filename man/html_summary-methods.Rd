\docType{methods}
\name{html_summary-methods}
\alias{html_summary}
\alias{html_summary,syndromic-method}
\alias{html_summary-methods}
\title{\code{html_summary}}
\arguments{
  \item{x}{a \code{syndromic} object, from where dates and
  observed data will be extracted.}

  \item{file.name}{a name for the html file to be created
  with the summary. The default is "syndromic.summary".
  When changing the file name remember to use quotes. Also
  remember to set the right directory before using the
  function (see example) - as several figure files will be
  created, you may prefer to create a dedicated
  subdirectory within the current working directory.}

  \item{weekends}{Whether the data contained in the object
  \code{syndromic} includes weekends (and therefore has an
  annual cycle of 365 days) or not (cycles of 260 days). By
  default it is TRUE.}

  \item{short}{By default set to FALSE. When set to TRUE,
  omits the fitting of poisson and negative binomial
  distributions, displaying only summary statistics and
  plots for each series.}
}
\value{
  An ".html" page with sections corresponding to each
  syndromic groups found in the slot in the slot observed
  of the \code{syndromic} object. These include: \describe{
  \item{daily and weekly plots}{ Line plots of the data
  found in the slot observed of the \code{syndromic} object
  provided. Weekly plots are produced merging the daily
  data by week.  }

  \item{basic summary statistics}{ Such as mean, quartiles,
  auto-correlation and partial auto-correlation for week
  and year.  }

  \item{box-plots}{ of the data by day-of-week, monh and
  year, intended to allow a preliminary assessment of which
  temporal effects are present (day-of-week, seasonal or
  trends) }

  \item{Poisson model fitting}{ Fitting of a Poisson model
  to the data using day-of-week only, and day-of-week
  combined with month OR sin/cos for seasonal effects.
  Auto-regressive terms from 1 to 7 days are also included,
  and AIC for all fitted models are displayed.  }

  \item{PoNegative Binomial model fitting}{ Fitting of a
  negative binomial model to the data using day-of-week
  only, and day-of-week combined with month OR sin/cos for
  seasonal effects.  Auto-regressive terms from 1 to 7 days
  are also included, and AIC for all fitted models are
  displayed.  }

  }
}
\description{
  Performs an exploratory, descriptive analysis of the time
  series of observed data, for as many syndromic groups as
  under study, and outputs an html summary.
}
\details{
  The summary should constitue a first step in the
  retrospective exploratory analysis of available syndromic
  data. It is also intended to serve as means to check the
  result of the creation of an object of the class
  \code{syndromic}. That is, it is a convenient, fast way
  to plot all syndromic time-series in the object.

  Because the function allows little manipulation of the
  Poisson/Negative Binomial fitting, errors can be thrown
  by that part of the code. In that case it is recommended
  to use short=TRUE and perform regression analysis
  individually using the in the slot observed data
  directly.
}
\examples{
data(lab.daily)
my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                  syndromes.var=Syndrome,
                                 dates.var=DateofSubmission,
                                 date.format="\%d/\%m/\%Y",
                                 merge.weekends=TRUE,
                                 data=lab.daily)
wd = getwd()
setwd(paste0(wd,"/retro"))
html_summary(my.syndromic, weekends=FALSE)
setwd(wd)


my.syndromic <- raw_to_syndromic (id=lab.daily$SubmissionID,
                                 syndromes.var=lab.daily$Syndrome,
                                 dates.var=lab.daily$DateofSubmission,
                                 date.format="\%d/\%m/\%Y",
                                 merge.weekends=FALSE)
wd = getwd()
setwd(paste0(wd,"/retro"))
html_summary(my.syndromic)
setwd(wd)
}
\seealso{
  \code{\link{syndromic}}
}
\keyword{methods}


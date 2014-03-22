\docType{methods}
\name{retro_summary-methods}
\alias{retro_summary}
\alias{retro_summary-methods}
\alias{retro_summary,syndromic-method}
\title{\code{retro_summary}}
\arguments{
  \item{x}{a \code{syndromic} object, from where dates and
  observed data will be extracted.}

  \item{object.name}{a name for the title in the html file,
  byt default "my.syndromic".}

  \item{file.name}{a name for the rmd/html file to be
  created with the summary. The default is
  "syndromic.retro.summary". When changing the file name
  remember to use quotes. Please note that the function
  will create a subdirectory within the current working
  directory, whre all files will be saved. Make sure to
  check the current working directory (\code{getwd()}) and
  set a convenient one if needed (\code{setwd()}). See
  examples.}

  \item{frequency}{The cycle of data repetition. By default
  equal to 365 (year). For data without weekends, for
  instance, it should be set to 260.}

  \item{short}{By default set to FALSE. When set to TRUE,
  omits the fitting of poisson and negative binomial
  distributions, displaying only summary statistics and
  plots for each series.}
}
\value{
  A ".Rmd" file and a ".html" page with sections
  corresponding to each syndromic groups found in the slot
  in the slot observed of the \code{syndromic} object.
  These include: \describe{ \item{daily and weekly plots}{
  Line plots of the data found in the slot observed of the
  \code{syndromic} object provided. Weekly plots are
  produced merging the daily data by week.  }

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

  \item{Negative Binomial model fitting}{ Fitting of a
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
  under study, and outputs both a markdown file, where the
  user can have access to all retrospective analysis R
  codes, and an html summary (produced by knitting the .Rmd
  file.
}
\details{
  The summary should constitue a first step in the
  retrospective exploratory analysis of available syndromic
  data. It is also intended to serve as means to check the
  result of the creation of an object of the class
  \code{syndromic}. That is, it is a convenient, fast way
  to plot all syndromic time-series in the object.

  If the user want to make changes to the summary produced,
  it is easy to open the .Rmd file in RStudio and produce
  any changes to the R code generated.
}
\examples{
data(lab.daily)
my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                  syndromes.var=Syndrome,
                                  dates.var=DateofSubmission,
                                  date.format="\%d/\%m/\%Y",
                                  data=lab.daily)
retro_summary(my.syndromic)


my.syndromic <- raw_to_syndromic (id=lab.daily$SubmissionID,
                                 syndromes.var=lab.daily$Syndrome,
                                 dates.var=lab.daily$DateofSubmission,
                                 date.format="\%d/\%m/\%Y")
wd = getwd()
setwd(paste0(wd,"/retro"))
retro_summary(my.syndromic)
setwd(wd)
}
\seealso{
  \code{\link{syndromic}}
}
\keyword{methods}


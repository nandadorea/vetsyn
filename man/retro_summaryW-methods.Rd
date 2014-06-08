\docType{methods}
\name{retro_summaryW-methods}
\alias{retro_summaryW}
\alias{retro_summaryW-methods}
\alias{retro_summaryW,syndromic-method}
\title{\code{retro_summary}}
\arguments{
  \item{x}{a \code{syndromicW} object, from where weeks and
  observed data will be extracted.}

  \item{object.name}{a name for the title in the html file,
  by default "my.syndromic".}

  \item{file.name}{a name for the rmd/html file to be
  created with the summary. The default is
  "syndromic.retro.summary". When changing the file name
  remember to use quotes. Please note that the function
  will create a subdirectory within the current working
  directory, where all files will be saved. Make sure to
  check the current working directory (\code{getwd()}) and
  set a convenient one if needed (\code{setwd()}). See
  examples.}

  \item{frequency}{The cycle of data repetition. By default
  equal to 52 (year).}

  \item{short}{By default set to FALSE. When set to TRUE,
  omits the fitting of poisson and negative binomial
  distributions, displaying only summary statistics and
  plots for each series.}
}
\value{
  A ".Rmd" file and a ".html" page with sections
  corresponding to each syndromic group found in the slot
  observed of the \code{syndromic} object. These include:
  \itemize{ \item{time series plots}{ Line plots of the
  data found in the slot observed of the \code{syndromic}
  object provided.  }

  \item{basic summary statistics}{ Such as mean, quartiles,
  auto-correlation and partial auto-correlation.  }

  \item{box-plots}{ meant to provide a year view of the
  data, super-imposing the data from multiple year by week,
  in order to provide an assessment of the seasonal
  patterns that may be present in the time series.  }

  \item{Poisson model fitting}{ Fitting of a Poisson model
  to the data using a formula specified by the user.  }

  \item{Negative Binomial model fitting}{ Fitting of a
  negative binomial model to the data using a formula
  specified by the user.  }

  }
}
\description{
  Performs an exploratory, descriptive analysis of the time
  series of observed data, for as many syndromic groups as
  under study, and outputs both a markdown file, where the
  user can have access to all retrospective analysis R
  codes, and an html summary (produced by knitting the .Rmd
  file).
}
\details{
  The summary should constitue a first step in the
  retrospective exploratory analysis of available syndromic
  data. It is also intended to serve as means to check the
  result of the creation of an object of the class
  \code{syndromic}. That is, it is a convenient, fast way
  to plot all syndromic time-series in the object.

  If the user wants to make changes to the summary
  produced, it is easy to open the .Rmd file in RStudio and
  produce any changes to the R code generated.
}
\examples{
data(lab.daily)
my.syndromicW <- rawD_to_syndromicW (id=SubmissionID,
                                  syndromes.var=Syndrome,
                                  dates.var=DateofSubmission,
                                  date.format="\%d/\%m/\%Y",
                                  data=lab.daily)
retro_summary(my.syndromicW)


my.syndromicW <- rawD_to_syndromicW (id=lab.daily$SubmissionID,
                                 syndromes.var=lab.daily$Syndrome,
                                 dates.var=lab.daily$DateofSubmission,
                                 date.format="\%d/\%m/\%Y")
wd = getwd()
setwd(paste0(wd,"/retro"))
retro_summaryW(my.syndromicW)
setwd(wd)
}
\seealso{
  \code{\link{syndromicW}}
}
\keyword{methods}


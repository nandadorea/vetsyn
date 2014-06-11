\name{convert_days_to_week}
\alias{convert_days_to_week}
\title{convert_days_to_week}
\usage{
  convert_days_to_week(counts.df, dates.df,
    date.format = "\%Y-\%m-\%d")
}
\arguments{
  \item{counts.df}{a matrix with rows corresponding to
  daily counts, and columns corresponding to syndromic
  groups}

  \item{dates.df}{a data frame where the first column
  contain dates}

  \item{date.format}{the date format in the dates variable,
  by default "\%Y-\%m-\%d".}
}
\description{
  Converts a matrix of daily counts into weekly counts,
  using the ISOweek standard, provided that a deparate data
  frame is appointed from which to extract the actual dates
  corresponding to each row of the matrix.
}
\details{
  This is an internal function of the package vetsyn, made
  available as an exported funciton in order to allow users
  to take advantage of the function retro_summary() from
  the package. That function generates a markdown file with
  R code that can be useful for users during restrospective
  analysis of syndromic data. When those R codes are run
  manually by the user, this function may be needed.
}
\examples{
data(lab.daily)
my.syndromic <- raw_to_syndromicD (id=lab.daily$SubmissionID,
                                  syndromes.var=lab.daily$Syndrome,
                                  dates.var=lab.daily$DateofSubmission,
                                  date.format="\%d/\%m/\%Y")
weekly <- convert_days_to_week(my.syndromic'at'observed; my.syndromic'at'dates)
}


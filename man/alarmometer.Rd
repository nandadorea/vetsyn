\docType{methods}
\name{alarmometer}
\alias{alarmometer}
\alias{alarmometer-methods}
\title{\code{alarmometer}}
\usage{
  alarmometer(score, scale = 9, radius = 4, center = 5.5,
    fill.colors = c("yellow2", "orange", "tomato"),
    arrow.colors = c("green", "orange", "tomato", "red"))
}
\arguments{
  \item{score}{the observed alarm score for which an alarm
  meter is desired.}

  \item{scale}{the maximum possible score (scale of
  alarms). Based on the number of detection algorithms
  used, and the number of detection limits set to each (3
  algorithms with 5 detection limits for each, for
  instance, will give a scale of 15)}

  \item{radius}{the radius of the circle to be drawn. It's
  generally not necessary to set this parameter, since the
  resulting figure doesn't show the scale.}

  \item{center}{the center of plotting, but the scale is
  not shown, so generally should not need to be changed.}

  \item{fill.colors}{the colors to be used for the 3
  degrees of alarm. By default they are yellow, orange and
  light red (tomato).}

  \item{arrow.colors}{the colors of the arrow for varying
  levels of alarm. By default green is used when the score
  is zero, and a progressive scale is used for increasing
  scores: orange, tomato and red.}
}
\description{
  Code to plot a syndromic final score when an alarm scale
  is used
}
\examples{
alarmometer(0)
alarmometer(score=2,scale=9)
alarmometer(score=9,scale=12)
}


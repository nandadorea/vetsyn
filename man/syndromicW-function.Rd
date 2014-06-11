\name{syndromicW-function}
\alias{syndromicW-function}
\title{syndromicW-function}
\usage{
  syndromicW(observed, dates = data.frame(),
    min.week = NULL, max.week = NULL, min.year = NULL,
    max.year = NULL, baseline = matrix(nrow = 0, ncol = 0),
    alarms = array(dim = 0), UCL = array(dim = 0),
    LCL = array(dim = 0))
}
\arguments{
  \item{observed}{A \code{matrix} with the observed data}

  \item{dates}{A \code{data.frame} with the complete
  ISOweeks of of each time point in the first column (each
  row in the matrix provided in \code{observed}).}

  \item{min.week}{Alternative to providing dates: the user
  can specify the minimum week-year, and the maximum
  week-year, and the function will generate a complete
  sequence of weeks between them. This is the recommended
  use of the function because then additional date
  variables will also be created and assigned to the slot
  dates, such as ISOweek, year and numerical weeks. Note
  that the sequence must match the number of rows in the
  matrix \code{observed}.}

  \item{min.year}{see min.week above}

  \item{max.week}{see min.week above}

  \item{max.year}{see min.week above}

  \item{baseline}{Optional argument. A \code{matrix} of
  same dimensions as \code{observed}, containing observed
  data that has been pre-processed to remove excessive
  noise and outbreak signals retrospectively, generating an
  "outbreak-free baseline". Normally syndromic objects are
  created without this slot, receiving by default an empty
  matrix. Methods within this package are then used to
  create this slot from data on \code{observed}.}

  \item{alarms}{Optional argument. An \code{array} with the
  same number of rows and columns as \code{observed}, with
  an additional dimension to store the alarm output of
  multiple detection algorithms used. Normally syndromic
  objects are created without this slot, receiving by
  default an empty array. Methods within this package are
  then used to create this slot from data on
  \code{observed}.}

  \item{UCL}{Optional argument. An \code{array} with the
  same number of rows and columns as \code{observed}, with
  an additional dimension to store the upper control limit
  output of multiple detection algorithms used. Normally
  syndromic objects are created without this slot,
  receiving by default an empty array. Methods within this
  package are then used to create this slot from data on
  \code{observed}.}

  \item{LCL}{Lower control limit. See UCL above.}
}
\value{
  an object of the class \code{syndromicW} with the slots
  corresponding to the parameters described. See
  \code{class-syndromic} for more details.
}
\description{
  Create an object of the class \code{syndromicW} from
  pre-cleaned data.
}
\details{
  Syndromic is the main class of the package. Two versions
  are available, \code{syndromicD}, which is intended for
  use when data are to be monitored daily, and
  \code{syndromicW}, which is to be used when data are
  monitored weekly.

  The observed data are assumed to be formatted as a
  \code{matrix}, in which each row corresponds to an
  observation time point (here, WEEKS), columns correspond
  to each syndromic group to be monitored, and the numbers
  refer to the number of observatiosn per group per time
  point. For an example of data formatted this way use
  date(observedW). For data not formatted use directly the
  function \code{rawD_to_syndromicW} or
  \code{rawW_to_syndromicW}.

  The dates are expected to be a \code{data.frame} with the
  first column in the \code{ISOweek} format, though the
  function will also accept a minimum and maximum week, and
  take care of constructing the appropriate data frame
  (this is the recommended use).
}
\examples{
data(observedW)
my.syndromicW <- syndromicW(observed,min.week=1, min.year=2011,
                              max.week=22, max.year=2013)
}


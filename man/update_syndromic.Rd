\name{update_syndromic}
\alias{update_syndromic}
\title{update_syndromic}
\arguments{
  \item{x}{the syndromic object to be updated (if one is
  not already existing, please use "raw_to_syndromic" or
  "syndromic")}

  \item{id}{indicates a variable (or multiple variables)
  which should be used to identify unique events in the
  data. It can be provided as an R vector (p.e.
  mydata$myid), as the name of a DataFrame column (p.e.
  id=myid, data=my.data), or as multiple column names (p.e.
  id=list(id1,id2,id3), data=my.data).}

  \item{syndromes.var}{the variable that identifies group
  membership (in general the syndromic grouping). Can be
  \code{numeric}, \code{character} or \code{factor}.}

  \item{add.syndromes}{a logical argument indicating
  whether syndromic groups found in the new data and not
  existing in the old syndromic object should be added (set
  to TRUE) or ignored (set to FALSE).}

  \item{dates.var}{The vector (dates.var=mydata$mydates) or
  column name (dates.var=mydates, data=mydata) where the
  dates of the events are to be found.}

  \item{date.format}{The date.format of the date.variable.
  Default is "%d/%m/%Y". See strptime() for format
  specifications}

  \item{remove.dow}{An optional argument, by default set to
  FALSE. This allows the user to specify weekdays that must
  be removed from the dataset, for instance when weekends
  are not relevant. This must be se to integers between 0
  and 6 specifying the day of the week to be removed. To
  remove saturdays and sundays, for instance, set
  remove.dow=c(6,0)}

  \item{add.to}{when remove.dow is used, the user has the
  option to completely remove any counts assigned to the
  days of week to be remove (set add.to=0) or add then to
  the following or precedent day. For instance when
  removing weekends, the counts registered during weekends
  can be assigned to the following Monday or the preceding
  Friday, using add.to=1 or add.to=-1 respectively. Please
  note that: (i) the vector add.to must have the exact same
  dimensions as remove.dow. To remove weekends adding any
  observed counts to the following Monday the user would
  need to set remove.dow=c(6,0) and add.to=c(2,1)
  (Saturdays added to 2 days ahead, and Sunday to 1 day
  ahead)}

  \item{replace.dates}{a logical argument indicating
  whether dates in the new dataset for which data was
  already available in the syndromic object should replace
  the old value (set to TRUE) or be ignored (set to FALSE)}

  \item{data}{Optional argument. If used the other
  arguments can be specified as column names within the
  dataset provided through this argument}
}
\value{
  an updated object of the class \code{syndromic} with the
  following slots: \describe{ \item{observed}{The previous
  observed matrix is updated adding lines corresponding to
  the new events found in the new dataset.}
  \item{dates}{also updated to include extra dates}
  \item{baseline}{if a baseline matrix was available lines
  are added, with data copied from
}
\description{
  \code{update_syndromic}
}
\details{
  Updates an object of the class \code{syndromic}, adding
  additional rows (additional time points) from raw,
  observed data. To understand a syndromic object, please
  refer to the help page for the syndromic class.
}
\examples{
data(lab.daily)
data(lab.daily.update)
my.syndromic <- raw_to_syndromic (id=SubmissionID,
                                  syndromes.var=Syndrome,
                                  dates.var=DateofSubmission,
                                  date.format="\%d/\%m/\%Y",
                                  data=lab.daily)
}


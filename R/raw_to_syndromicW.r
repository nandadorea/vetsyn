##' \code{raw_to_syndromicW}
##'
##' An object \code{syndromicW} (syndromic main class for data to be monitored weekly)
##' can be created from data that were originally recorded with the WEEK of the
##' event or when
##' the DATE of the event was recorded, but the user wants to group events
##' per week. The function will handle both cases, group events by week,
##' and output counts by week. For data
##' already grouped into the number of observations per week, please see the
##' function \code{syndromicW}.
##' 
##' This function will count the number of cases for one or more defined groups, weekly.
##' Weeks without counts will be assigned a count of zero,
##' generating a complete sequence of weeks. The complete sequence will
##' start at the minimum week found in the dataset and end at the maximum week,
##' by default. However it is also possible to provide a minimum DATE EARLIER 
##' than the minimum in the dataset(since the
##' original data were recorded based on the date, it's assumed that the user may
##' wanst to establish cut-offs based on the dates from the original data, not weeks,
##' which is the format of the output). It is also possible to provide a maximum 
##' date LATER than the
##' latest recorded. The extra weeks created are assigned counts of zero (minimum
##' or maximum dates already within the range of the dataset are ignored).
##'
##' The raw, observed data, are assumed to be stored in a \code{data.frame}
##' in which each observed event (for instance a laboratory submission) is
##' recorded in one or multiple rows. Unique events can be identified by one
##' unique ID. It is possible however to take into consideration an hierarchical
##' organization of the data, by which an unique ID can only be verified taking
##' into account multiple columns (p.e. animal ID is unique within farm, but
##' not between farms, therefore the IDs are unique combinations of the
##' variables "farm" and "animal").
##'
##' Multiple events with the same unique ID are acceptable, but counted
##' only once per time unit (p.e. WEEK). Besides removing duplicated events, the
##' function also completes missing weeks, assigning them a count of zero.
##'
##' The function counts the number of events, per week, for each of the groups
##' found in the variable \code{syndromes.var}. However, the variable
##' \code{syndromes.name} can be used to RESTRICT the groups counted (if not all
##' values appearing in the data are to be subjected to monitoring, p.e. when
##' "nonspecific" or "non-classified" values exist); or to EXTEND the list to
##' include values which did not appear in the dataset (this is the recommended
##' use of this function for regular monitoring, in order to assure that
##' groups with zero events in the specific data batch being analyzed will
##' still be represented in the output of the function, though with zero
##' counts every day.)
##' 
##' IMPORTANT: Please note that this function removed DUPLICATED records based on
##' a repeat id, within the same DATE, since daily records are provided. If two 
##' cases with the same ID are recorded in the same week, but dfferent days,
##' these will be counted as TWO CASES.To eliminate repeated cases within the same
##' week, please convert the date to ISOweek format using the functions in this package,
##' and use, instead, the function \code{rawW_to_syndromicW}.
##'
##' @rdname raw_to_syndromic-methods
##' @docType methods
##' 
##' @param id indicates a variable (or multiple variables) which should
##' be used to identify unique events in the data. It can be provided as an R
##' vector (p.e. mydata$myid), as the name of a DataFrame column
##' (p.e. id=myid, data=my.data), or as multiple column names
##' (p.e. id=list(id1,id2,id3), data=my.data).
##' @param syndromes.var the variable that identifies group membership
##' (in general the syndromic grouping). Can be  \code{numeric},
##' \code{character} or \code{factor}.
##' @param syndromes.name An optional argument providing the syndromic
##' groups to be monitored. If not given, it is taken from the
##' values found in \code{syndromes.var}. When syndromes.name IS provided, it
##' should be provided as character value or vector
##' (p.e. "Mastitis" or c("Mastitis","GIT") ).
##' @param dates.var The vector (dates.var=mydata$mydates) or column name
##' (dates.var=mydates, data=mydata) where the dates of the events are to be found.
##' This parameter will be able to handle a column in which the DATE of the event
##' was recorded or in which the WEEK of the event was recorded. In the latter case, however,
##' the week MUST be in the ISOweek format (p.e., "2014-W02-1"). In any case the function will
##' group events by week and outputs will be in the ISOweek format.
##' @param date.format The date.format of the date.variable.
##' Default is d/m/Y. See strptime() for format specifications. If the dates 
##' variable records the WEEKS of the events, please set date.format=ISOweek"
##' @param min.date An optional argument. If not provided, the minimum date found
##' in the dataset is used. Must be provided in the same date format as set n "date.format",
##' that is, the same format for dates when the DATE of the event was recorded,
##' or ISOweek.
##' @param max.date An optional argument. If not provided, the maximum date found
##' in the dataset is used.As with min.date, can be a date or a ISOweek.
##' @param sort Default is true, which organizes the groups found in syndromes.name
##' alphabetically. If set to FALSE, groups are listed in the order they are found
##' in the dataset or provided in syndromes.name.
##' @param data Optional argument. If used the other arguments can be specified
##' as column names within the dataset provided through this argument
##'
##' @return an object of the class \code{syndromicW} with the following slots:
##' (1) OBSERVED: A matrix with as many columns as syndromic groups
##'  found in the dataset (or listed by the user); (2) DATES: A data frame
##'  where the first column contains the complete
##'  ISOweek of dates from the minimum to the maximum date found in the dataset
##'  (or set by the user), and additional columns contain additional date variables
##'  (such as day of numerical week and year) as generated by default when an object of
##'  the class \code{syndromicW} is created. 
##'  
##' @import ISOweek
##' @export 
##' @examples
##' ##data recorded with the DATE of the event
##' data(lab.daily)
##' my.syndromicW <- raw_to_syndromicW (id=lab.daily$SubmissionID,
##'                                   syndromes.var=lab.daily$Syndrome,
##'                                   dates.var=lab.daily$DateofSubmission,
##'                                   date.format="%d/%m/%Y")
##'
##' my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'
##' my.syndromicW <- raw_to_syndromicW (id=list(HerdID,AnimalID),
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'
##' my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   syndromes.name=c("GIT","Musculoskeletal"),
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'
##' my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'
##' my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   min.date="01/01/2011",
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'                                   
##'##data recorded with the WEEK of the event
##' my.syndromicW <- raw_to_syndromicW (id=lab.weekly$SubmissionID,
##'                                     syndromes.var=lab.weekly$Syndrome,
##'                                     dates.var=lab.weekly$DateofSubmission,
##'                                     date.format="ISOweek")
##'
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                    syndromes.var=Syndrome,
##'                                    dates.var=DateofSubmission,
##'                                    date.format="ISOweek",
##'                                    data=lab.weekly)
##'
##'my.syndromicW <- raw_to_syndromicW (id=list(HerdID,AnimalID),
##'                                    syndromes.var=Syndrome,
##'                                    dates.var=DateofSubmission,
##'                                    date.format="ISOweek",
##'                                    data=lab.weekly)
##'
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                    syndromes.var=Syndrome,
##'                                    syndromes.name=c("GIT","Musculoskeletal"),
##'                                    dates.var=DateofSubmission,
##'                                    date.format="ISOweek",
##'                                    data=lab.weekly)
##'
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                    syndromes.var=Syndrome,
##'                                    syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
##'                                    dates.var=DateofSubmission,
##'                                    date.format="ISOweek",
##'                                    data=lab.weekly)
##'
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                    syndromes.var=Syndrome,
##'                                    dates.var=DateofSubmission,
##'                                    date.format="ISOweek",
##'                                    min.date="2010-W50-1",
##'                                    data=lab.weekly)



raw_to_syndromicW <- function(id,
                             syndromes.var, 
                             syndromes.name,
                             dates.var, 
                             date.format="%d/%m/%Y", 
                             min.date, 
                             max.date,
                             sort=TRUE,
                             data=NULL) {
  
  
  #account for use of "data="
  id           <-eval(substitute(id),data, parent.frame())
  syndromes.var<-eval(substitute(syndromes.var),data, parent.frame())
  dates.var    <-eval(substitute(dates.var),data, parent.frame())
    
  
  #syndrome names if not supplied
  if (missing("syndromes.name"))  (syndromes.name <- unique(syndromes.var))
  if (sort==TRUE) (syndromes.name <- sort(syndromes.name) )
  
  
  
  if (date.format=="ISOweek"){
    
    
    #making sure of ISOweek format
    dates.var <- as.character(dates.var)
    week <- as.numeric(substr(as.character(dates.var),7,8))
    year <- as.numeric(substr(as.character(dates.var),1,4))
    dates.var <- create_isoweek(week,year,reference.day=1)
    
    
    #set date interval from data or based on user parameters
    if (missing("min.date")) {
      min.week <- as.numeric(substr(as.character(date2ISOweek(min(ISOweek2date(dates.var)))),7,8))
      min.year <- as.numeric(substr(as.character(date2ISOweek(min(ISOweek2date(dates.var)))),1,4))
    } else {
      min.week <- as.numeric(substr(as.character(min.date),7,8)) 
      min.year <- as.numeric(substr(as.character(min.date),1,4)) 
    }
    
    if (missing("max.date")) {
      max.week <- as.numeric(substr(as.character(date2ISOweek(max(ISOweek2date(dates.var)))),7,8))
      max.year <- as.numeric(substr(as.character(date2ISOweek(max(ISOweek2date(dates.var)))),1,4))
    } else {
      max.week <- as.numeric(substr(as.character(max.date),7,8)) 
      max.year <- as.numeric(substr(as.character(max.date),1,4)) 
    }
    
    min.date <- ISOweek2date(create_isoweek(min.week,min.year,reference.day=1))
    max.date <- ISOweek2date(create_isoweek(max.week,max.year,reference.day=1))
    dates <- dates_df(min.date=min.date,max.date=max.date, 
                      by="days",
                      date.format = "%Y-%M-%d")
    ISOweek <- date2ISOweek(dates[,1])
    ISOweek<- paste0( (substr(as.character(ISOweek),1,9)),"1" )
    week <- as.numeric(substr(as.character(ISOweek),7,8))
    year <- as.numeric(substr(as.character(ISOweek),1,4))
    dates <- data.frame(ISOweek,week,year)
    dates <- unique(dates)
    
    
    #count cases for each syndromic group
    column <- 0
    
    for (syndrome in syndromes.name){
      column <- column+1
      
      if (length(which(syndromes.var==syndrome))==0) {
        nsubmissions <- rep(0,dim(dates)[1])} else {
          
          #Syndrome specific ID: syndromes may have been given as a list
          if (is.list(id)==TRUE) {
            id <- data.frame(matrix(unlist(id), nrow=length(id[[1]]), byrow=F))}
          
          #Syndrome specific ID: after converting lists to DF, 
          #account for syndrome being vector
          if (is.data.frame(id)==FALSE) {
            id2 <- id[which(syndromes.var==syndrome)]
          } else {
            id2 <- id[which(syndromes.var==syndrome),]}
          
          #Syndrome specific dates:
          dates.var2 <- dates.var[which(syndromes.var==syndrome)]
          
          #count submissions for this syndrome, per unique id and date
          nsubmissions  <- data.frame(id2,dates.var2)
          nsubmissions <- unique(nsubmissions)
          counts <- rep(1,dim(nsubmissions)[1])
          
          #remove id
          if (is.data.frame(id)==FALSE)  {
            nsubmissions <- nsubmissions[,-1]
          } else {
            nsubmissions = nsubmissions[,-(1:(dim(id)[2]))]}
          
          #merge counts by date
          
          nsubmissions <- aggregate(counts,by=list(week=nsubmissions),FUN=sum)
          
          nsubmissions <- merge(dates, nsubmissions, by.x="ISOweek", by.y="week", all.x = TRUE) 
          nsubmissions <- nsubmissions$x
          nsubmissions[is.na(nsubmissions)==TRUE]<-0
          
        }
      
      if (column==1){
        syndrome.counts <- as.matrix(nsubmissions)
      } else {
        syndrome.counts <- cbind(syndrome.counts,nsubmissions)
      }
      
    }
    
    colnames(syndrome.counts) <- syndromes.name
    
    
    
    
    
    
    
  }else{
  
  
  
    
  
  
  
  
  #set date interval from data or based on user parameters
  if (missing("min.date")) {
    min.date <- min(as.Date (dates.var, format = date.format))
  } else {
    min.date <- as.Date(min.date, format = date.format)
  }
  
  if (missing("max.date")) {
    max.date <- max(as.Date (dates.var, format = date.format))
  } else {
    max.date <- as.Date(max.date, format = date.format)
  }
  
  
  #count cases for each syndromic group
  column <- 0
  
  for (syndrome in syndromes.name){
    column <- column+1
    
    if (length(which(syndromes.var==syndrome))==0) {
      nsubmissions <- rep(0,(max.date-min.date+1))} else {
        
        #Syndrome specific ID: syndromes may have been given as a list
        if (is.list(id)==TRUE) {
          id <- data.frame(matrix(unlist(id), nrow=length(id[[1]]), byrow=F))}
        
        #Syndrome specific ID: after converting lists to DF, 
        #account for syndrome being vector
        if (is.data.frame(id)==FALSE) {
          id2 <- id[which(syndromes.var==syndrome)]
        } else {
          id2 <- id[which(syndromes.var==syndrome),]}
        
        #Syndrome specific dates:
        dates.var2 <- dates.var[which(syndromes.var==syndrome)]
        
        #count submissions for this syndrome, per unique id and date
        nsubmissions  <- data.frame(id2,dates.var2)
        nsubmissions <- unique(nsubmissions)
        counts <- rep(1,dim(nsubmissions)[1])
        
        #remove id
        if (is.data.frame(id)==FALSE)  {
          nsubmissions <- nsubmissions[,-1]
        } else {
          nsubmissions = nsubmissions[,-(1:(dim(id)[2]))]}
        
        #merge counts by date
        nsubmissions <- aggregate(counts,by=list(date=nsubmissions),FUN=sum)
        dateR <- as.Date(nsubmissions$date, format =  date.format)
        nsubmissions <- nsubmissions[order(dateR),]
        dateR <- dateR[order(dateR)]
        dateR <- strptime (as.character(dateR), format = "%Y-%m-%d")
        seq <- seq(from=min.date,to=max.date, by='days')        
        date.complete <- strptime (substring(as.character(seq), 1,10), 
                                   format = "%Y-%m-%d")
        date.complete <- as.data.frame(date.complete)
        nsubmissions <- cbind(nsubmissions,dateR)
        nsubmissions <- merge (nsubmissions, date.complete, 
                               by.x="dateR", by.y="date.complete", all="T")
        nsubmissions$x[is.na(nsubmissions$x)==TRUE] <- 0
        nsubmissions <- as.vector(nsubmissions$x)
      }
    
    if (column==1){
      syndrome.counts <- as.matrix(nsubmissions)
    } else {
      syndrome.counts <- cbind(syndrome.counts,nsubmissions)
    }
    
  }
  
  colnames(syndrome.counts) <- syndromes.name
  
  
  #create dates matrix using internal function
  dates <- dates_df(min.date,max.date, 
                    by="days",
                    date.format = "%Y-%m-%d")
  
  
  syndrome.counts <- convert_days_to_week (syndrome.counts,
                                   dates,date.format="%Y-%m-%d")
  syndrome.counts$week <- NULL
  syndrome.counts$year <- NULL
  
  
  ISOweek <- date2ISOweek(dates[,1])
    ISOweek<- paste0( (substr(as.character(ISOweek),1,9)),"1" )
  week <- as.numeric(substr(as.character(ISOweek),7,8))
  year <- as.numeric(substr(as.character(ISOweek),1,4))
  dates <- data.frame(ISOweek,week,year)
  dates <- unique(dates)
  
  }  
  
  syndromicW(observed=as.matrix(syndrome.counts), dates=dates)
  
  
}

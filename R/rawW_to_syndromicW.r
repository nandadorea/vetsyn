##' \code{rawW_to_syndromicW}
##'
##' An object \code{syndromicW} (syndromic main class for data to be monitored weekly)
##' can be created from data that were originally recorded with the DAY of the
##' event, but the user wants to group by WEEK (then use \code{rawD_to_syndromicW});
##' or from data that were already recorded with only indication to the WEEK of the event,
##' in which case the user must use the function \code{rawW_to_syndromicW}. For data
##' already grouped into the number of observations per week, please see the
##' function \code{syndromicW}.
##' 
##' This function will count the number of cases for one or more defined groups, WEEKLY.
##' Weeks without counts will be assigned a count of zero,
##' generating a complete sequence of weeks The complete sequence will
##' start at the minimum week found in the dataset and end at the maximum week,
##' by default. However it is also possible to provide a minimum week-year
##' EARLIER than the minimum in the dataset or a maximum week-year LATER than the
##' latest recorded. The extra weeks are assigned counts of zero (minimum
##' or maximum weeks already within the range of the dataset are ignored).
##'
##' The raw, observed data, is assumed to be stored in a \code{data.frame}
##' in which each observed event (for instance a laboratory submission) is
##' recorded in one or multiple rows. Unique events can be identified by one
##' unique ID. It is possible however to take into consideration an hierarchical
##' organization of the data, by which an unique ID can only be verified taking
##' into account multiple columns (p.e. animal ID is unique within farm, but
##' not between farms, therefore the IDs are unique combinations of the
##' variables "farm" and "animal").
##'
##' Multiple events with the same unique ID are acceptable, but counted
##' only once per time unit (p.e. day). Besides removing duplicated events, the
##' function also completes missing days, assigning them a count of zero.
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
##' @title rawW_to_syndromicW
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
##' @param weeks.var The vector of weeks in the ISOWEEK format. If weeks are
##' not recorded in the ISOweek format, please see the function
##' \code{create_isoweek} which creates ISOweek from data provided as week and year,
##' in numerical formats. Consult the examples in that function for an example
##' of how to add a column to a dataset in the format ISOweek.
##' @param min.week The user can specify the
##' minimum week-year, and/or the maximum week-year, to create a dataset that extends 
##' the weeks found in the data. 
##' @param min.year see min.week above
##' @param max.week see min.week above
##' @param max.year see min.week above
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
##' data(lab.daily)
##' my.syndromicW <- rawW_to_syndromicW (id=lab.weekly$SubmissionID,
##'                                   syndromes.var=lab.weekly$Syndrome,
##'                                   week.var=lab.weekly$DateofSubmission)
##'
##' my.syndromicW <- rawW_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   week.var=DateofSubmission,
##'                                   data=lab.weekly)
##'
##' my.syndromicW <- rawW_to_syndromicW (id=list(HerdID,AnimalID),
##'                                   syndromes.var=Syndrome,
##'                                   week.var=DateofSubmission,
##'                                   data=lab.weekly)
##'
##' my.syndromicW <- rawW_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   syndromes.name=c("GIT","Musculoskeletal"),
##'                                   week.var=DateofSubmission,
##'                                   data=lab.weekly)
##'
##' my.syndromicW <- rawW_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
##'                                   week.var=DateofSubmission,
##'                                   data=lab.weekly)
##'
##' my.syndromicW <- rawW_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   week.var=DateofSubmission,
##'                                   min.week=40,
##'                                   min.year=2010,
##'                                   data=lab.weekly)
##'                                   
##'                                   


rawW_to_syndromicW <- function(id,
                             syndromes.var, 
                             syndromes.name,
                             week.var,
                             min.week,
                             min.year,
                             max.week,
                             max.year,
                             sort=TRUE,
                             data=NULL) {
  
  
  #account for use of "data="
  id           <-eval(substitute(id),data, parent.frame())
  syndromes.var<-eval(substitute(syndromes.var),data, parent.frame())
  week.var    <-eval(substitute(week.var),data, parent.frame())
  
    
  #syndrome names if not supplied
  if (missing("syndromes.name"))  (syndromes.name <- unique(syndromes.var))
  if (sort==TRUE) (syndromes.name <- sort(syndromes.name) )
  
  #making sure of ISOweek format
  week.var <- as.character(week.var)
  week <- as.numeric(substr(as.character(week.var),7,8))
  year <- as.numeric(substr(as.character(week.var),1,4))
  week.var <- create_isoweek(week,year,reference.day=1)
    
  
  #set date interval from data or based on user parameters
  if (missing("min.week")) {
    min.week <- as.numeric(substr(as.character(min(week.var)),7,8))
    min.year <- as.numeric(substr(as.character(min(week.var)),1,4))
  } else {
    min.week <- min(min.week, as.numeric(substr(as.character(min(week.var)),7,8)) )
    min.year <- min(min.year, as.numeric(substr(as.character(min(week.var)),1,4)) )
  }
  
  if (missing("max.week")) {
    max.week <- as.numeric(substr(as.character(max(week.var)),7,8))
    max.year <- as.numeric(substr(as.character(max(week.var)),1,4))
  } else {
    max.week <- max(max.week, as.numeric(substr(as.character(max(week.var)),7,8)) ) 
    max.year <- max(max.year, as.numeric(substr(as.character(max(week.var)),1,4)) )
  }
  
  min.date <- ISOweek2date(create_isoweek(min.week,min.year,reference.day=1))
  max.date <- ISOweek2date(create_isoweek(max.week,max.year,reference.day=1))
  dates <- dates_df(min.date=min.date,max.date=max.date, 
                    by="days",
                    date.format = "%Y-%M-%d")
  dates <- dates[,c("week","year")]
  dates <- unique(dates)
  ISOweek<- create_isoweek(dates$week,dates$year,reference.day=1)
  dates <- cbind(ISOweek,dates)
  
  
  
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
        week.var2 <- week.var[which(syndromes.var==syndrome)]
        
        #count submissions for this syndrome, per unique id and date
        nsubmissions  <- data.frame(id2,week.var2)
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
  
    
  
  syndromicW(observed=as.matrix(syndrome.counts), dates=dates)
  
  
}

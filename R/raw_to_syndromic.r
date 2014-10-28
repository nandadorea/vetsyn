##' \code{raw_to_syndromicD}
##'
##' Create an object of the class \code{syndromicD} from raw, observed data.
##' This assumed data will be monitored DAILY. For weekly monitoring please
##' see \code{rawD_to_syndromicW} and \code{rawW_to_syndromicW}.
##' 
##' This functions will count the number of cases for one or more defined groups, daily.
##' Days without counts will be assigned a count of zero,
##' generating a complete sequence of dates. The complete sequence will
##' start at the minimum date found in the dataset and end at the maximum day,
##' by default. However it is also possible to provide a minimum date
##' EARLIER than the minimum in the dataset or a maximum date LATER than the
##' latest recorded. The extra days are assigned counts of zero (minimum
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
##' only once per time unit (p.e. day). Besides removing duplicated events, the
##' function also completes missing days, assigning them a count of zero.
##'
##' The function counts the number of events, per day, for each of the groups
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
##' @title raw_to_syndromicD
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
##' @param date.format The date.format of the date.variable.
##' Default is "%d/%m/%Y". See strptime() for format specifications
##' @param min.date An optional argument. If not provided, the minimum date found
##' in the dataset is used.
##' @param max.date An optional argument. If not provided, the maximum date found
##' in the dataset is used.
##' @param sort Default is true, which organizes the groups found in syndromes.name
##' alphabetically. If set to FALSE, groups are listed in the order they are found
##' in the dataset or provided in syndromes.name.
##' @param remove.dow An optional argument, by default set to FALSE. This allows
##' the user to specify weekdays that must be removed from the dataset, for instance
##' when weekends are not relevant. This must be se to integers between 0 and 6
##' specifying the day of the week to be removed. To remove saturdays and sundays, 
##' for instance, set remove.dow=c(6,0). (Note that in R days of week are counted
##' from 0-Sunday to 6-Saturday)
##' @param add.to when remove.dow is used, the user has the option to completely remove
##' any counts assigned to the days of week to be removed (set add.to=0) or add
##' them to the following or precedent day. For instance when removing weekends,
##' the counts registered during weekends can be assigned to the following Monday or
##' the preceding Friday, using add.to=1 or add.to=-1 respectively. Please note that:
##' (i) the vector add.to must have the exact same dimensions as remove.dow. To remove 
##' weekends adding any observed counts to the following Monday the user would need to set
##' remove.dow=c(6,0) and add.to=c(2,1) (Saturdays added to 2 days ahead, and Sunday to
##' 1 day ahead)
##' @param data Optional argument. If used the other arguments can be specified
##' as column names within the dataset provided through this argument
##'
##' @return an object of the class \code{syndromic} with the following slots:
##' (1) OBSERVED: A matrix with as many columns as syndromic groups
##'  found in the dataset (or listed by the user); (2) DATES: A data frame
##'  where the first column contains the complete
##'  sequence of dates from the minimum to the maximum date found in the dataset
##'  (or set by the user), and additional columns contain additional date variables
##'  (such as day of week, holidays, month) as generated by default when an object of
##'  the class \code{syndromic} is created. 
##'  
##' @import ISOweek
##' @export 
##' @examples
##' data(lab.daily)
##' my.syndromicD <- raw_to_syndromicD (id=lab.daily$SubmissionID,
##'                                   syndromes.var=lab.daily$Syndrome,
##'                                   dates.var=lab.daily$DateofSubmission,
##'                                   date.format="%d/%m/%Y")
##'
##' my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'
##' my.syndromicD <- raw_to_syndromicD (id=list(HerdID,AnimalID),
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'
##' my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   syndromes.name=c("GIT","Musculoskeletal"),
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'
##' my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   syndromes.name=c("GIT","Musculoskeletal","NonExisting"),
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'
##' my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   min.date="01/01/2011",
##'                                   date.format="%d/%m/%Y",
##'                                   remove.dow=c(6,0),
##'                                   add.to=c(2,1),
##'                                   data=lab.daily)


raw_to_syndromicD <- function(id,
                             syndromes.var, 
                             syndromes.name,
                             dates.var, 
                             date.format="%d/%m/%Y", 
                             min.date, 
                             max.date,
                             remove.dow=FALSE,
                             add.to=0,
                             sort=TRUE,
                             data=NULL) {
  
  
  #account for use of "data="
  id           <-eval(substitute(id),data, parent.frame())
  syndromes.var<-eval(substitute(syndromes.var),data, parent.frame())
  dates.var    <-eval(substitute(dates.var),data, parent.frame())
  
  #checking validity
  if (remove.dow!=FALSE&&(sum(remove.dow<0)>0||sum(remove.dow>6)>0)) {
    stop("remove.dow must be an integer (or vector of integers)
         between 0 (Sunday) and 6 (Monday) or FALSE")
  }
  
  if (remove.dow!=FALSE&&length(add.to)!=length(remove.dow)) {
    stop("the argument add.to must have the exact same vector length
         as remove.dow")
  }
  
  
  #syndrome names if not supplied
  if (missing("syndromes.name"))  (syndromes.name <- unique(syndromes.var))
  if (sort==TRUE) (syndromes.name <- sort(syndromes.name) )
  
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
  
  #remove.dow    
  if (as.character(remove.dow[1])!="FALSE") {
    
    for (r in 1:length(remove.dow)){
      remove <- which(dates$dow==remove.dow[r])
      add    <- remove+(add.to[r])
          ignore <- which(add>dim(syndrome.counts)[1]|add<1)
      if (length(ignore)>0){
      remove <- remove[-ignore]
      add    <- add[-ignore]}
     
      syndrome.counts[add,] <- syndrome.counts[add,]+ syndrome.counts[remove,]
      syndrome.counts <- syndrome.counts[-(remove),]
      dates           <- dates[-(remove),]  
        
    }   
    
  }
  
  
  syndromicD(observed=as.matrix(syndrome.counts), dates=dates)
  
  
}

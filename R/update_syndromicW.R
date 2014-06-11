##' \code{update_syndromicW}
##'
##' Updates an object of the class \code{syndromicW}, adding additional rows
##' (additional time points) from raw, observed data. Observed data may have 
##' been recorded weekly, but the function can also accomodate for data that were
##' originally collected daily, or for which the day of the event was recorded, 
##' and the user wants to group those data weekly . To understand
##' a syndromicW object, please refer to the help page for the syndromic class.
##'
##' @name update_syndromic
##' @docType methods
##' 
##' @param x the syndromicW object to be updated (if one does not already exist,
##' please use "rawD_to_syndromicW" or "syndromicW")
##' @param id indicates a variable (or multiple variables) which should
##' be used to identify unique events in the data. It can be provided as an R
##' vector (p.e. mydata$myid), as the name of a Data Frame column
##' (p.e. id=myid, data=my.data), or as multiple column names
##' (p.e. id=list(id1,id2,id3), data=my.data).
##' @param syndromes.var the variable that identifies group membership
##' (in general the syndromic grouping). Can be  \code{numeric},
##' \code{character} or \code{factor}. 
##' @param add.syndromes a logical argument indicating whether
##' syndromic groups found in the new data and not existing in the old
##' syndromic object should be added (set to TRUE) or ignored (set to FALSE). 
##' @param dates.var The vector (dates.var=mydata$mydates) or column name
##' (dates.var=mydates, data=mydata) where the dates of the events are to be found.
##' @param date.format The date.format of the date.variable.
##' Default is "%d/%m/%Y". See strptime() for format specifications. If the
##' data have already been recorded weekly, then the column "dates.var" MUST
##' be in the ISOweek format, and the date.format must be set to "ISOweek".
##' @param replace.dates a logical argument indicating whether dates in the
##' new dataset for which data were already available in the syndromic object should
##' replace the old values (set to TRUE) or be ignored (set to FALSE)
##' @param data Optional argument. If used the other arguments can be specified
##' as column names within the dataset provided through this argument
##'
##' @return an updated object of the class \code{syndromicW} with the following slots: 
##' (1) OBSERVED: The previous observed matrix is updated adding lines corresponding
##'  to the new events found in the new dataset;
##'  (2) DATES: also updated to include extra dates;
##'  (3) BASELINE: if a baseline matrix was available lines are added, 
##'  with data copied from the slot observed;
##'  (4) ALARMS: the same number of lines added to observed are added, 
##'  but given a value of NA to make clear that aberration detection analyses
##'  were not yet carried out in these data;
##'  (5) UCL and (6) LCL: same as alarms.
##'    
##' @import ISOweek
##' @import abind
##' @export
##' 
##' @examples
##' data(lab.daily)
##' data(lab.daily.update)
##' my.syndromicW <- rawD_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##'my.syndromicW <- update_syndromic(x=my.syndromicW,
##'                                  id=lab.daily.update$SubmissionID,
##'                                  syndromes.var=lab.daily.update$Syndrome, 
##'                                  add.syndromes=TRUE,
##'                                  dates.var=lab.daily.update$DateofSubmission, 
##'                                  date.format="%d/%m/%Y", 
##'                                  replace.dates=TRUE)
##'
##'
##'my.syndromicW <- update_syndromic(x=my.syndromicW,
##'                                  id=SubmissionID,
##'                                  syndromes.var=Syndrome, 
##'                                  add.syndromes=TRUE,
##'                                  dates.var=DateofSubmission, 
##'                                  date.format="ISOweek", 
##'                                  replace.dates=TRUE,
##'                                  data=lab.weekly.update)##'



setMethod('update_syndromic',
          signature(x = 'syndromicW'),
          function (x,
                    id,
                    syndromes.var, 
                    add.syndromes=TRUE,
                    dates.var, 
                    date.format="%d/%m/%Y", 
                    replace.dates=TRUE,
                    data=NULL
          )
          {
            
            
            #account for use of "data="
            id           <-eval(substitute(id),data, parent.frame())
            syndromes.var<-eval(substitute(syndromes.var),data, parent.frame())
            dates.var    <-eval(substitute(dates.var),data, parent.frame())
            
            
            if(date.format=="ISOweek"){
              
              
              
              #set date interval from new data
              min.date <- min(ISOweek2date(dates.var))
              max.date <- max(ISOweek2date(dates.var))
              
              
              if (replace.dates==FALSE){
                
                min.date <- ISOweek2date(x@dates[dim(x@dates)[1],1])+7
                
                if(min.date>max.date){
                  stop("The dataset provided contains no new data and replace.dates has been set to FALSE")
                }
                
                
                dates <- dates_df(min.date=min.date,max.date=max.date, 
                                  by="days",
                                  date.format = "%Y-%M-%d")
                dates <- dates[,c("week","year")]
                dates <- unique(dates)
                ISOweek<- create_isoweek(dates$week,dates$year,reference.day=1)
                dates <- cbind(ISOweek,dates)
                
                
                remove <- which(ISOweek2date(dates.var) < min.date)
                id <- id[-remove]
                syndromes.var <- syndromes.var[-remove]
                dates.var<-dates.var[-remove]
              } 
              
              
              #syndrome names 
              if (add.syndromes==TRUE){
                syndromes.name <- unique(c(colnames(x@observed),as.character(syndromes.var)))
              } else {
                syndromes.name <- colnames(x@observed)
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
              
              min.week <- paste0(substr(as.character(date2ISOweek(min.date)),1,8),"-1")
              min.week <- ISOweek2date(min.week)
              ISOweek <-  paste0(substr(as.character(x@dates[,1]),1,8),"-1") 
              ISOweek <- ISOweek2date(ISOweek)
              
              end.rows <- max(which(ISOweek<min.week))
              
              
              add.columns <- dim(syndrome.counts)[2]-dim(x@observed)[2]
              baseline <- matrix(nrow=0,ncol=0)
              alarms <- array(dim=0)
              UCL <- array(dim=0)
              LCL <- array(dim=0)
              
              if (add.columns==0){
                observed <- rbind(x@observed[1:end.rows,],syndrome.counts)
                datesy <- rbind(x@dates[1:end.rows,],dates)
                
                if (dim(x@baseline)[1]!=0){
                  baseline <- rbind(x@baseline[1:end.rows,],syndrome.counts)
                }
                
                if (dim(x@alarms)[1]!=0){
                  alarms <- abind(x@alarms[1:end.rows,,],
                                  array(NA,dim=c(dim(syndrome.counts)[1],dim(x@alarms)[2],dim(x@alarms)[3])),
                                  along=1)
                }
                
                if (dim(x@UCL)[1]!=0){
                  UCL <- abind(x@UCL[1:end.rows,,],
                               array(NA,dim=c(dim(syndrome.counts)[1],dim(x@UCL)[2],dim(x@UCL)[3])),
                               along=1)
                }
                
                if (dim(x@LCL)[1]!=0){
                  LCL <- abind(x@LCL[1:end.rows,,],
                               array(NA,dim=c(dim(syndrome.counts)[1],dim(x@LCL)[2],dim(x@LCL)[3])),
                               along=1)    
                }
                
              } else{
                observed <- rbind(
                  cbind(x@observed[1:end.rows,],matrix(0,nrow=end.rows,ncol=add.columns)),
                  syndrome.counts)
                datesy <- rbind(x@dates[1:end.rows,],dates)
                
                if (dim(x@baseline)[1]!=0){
                  baseline <- rbind(
                    cbind(x@baseline[1:end.rows,],matrix(0,nrow=end.rows,ncol=add.columns)),
                    syndrome.counts)
                }
                
                if (dim(x@alarms)[1]!=0){
                  alarms <- abind(abind(x@alarms[1:end.rows,,],
                                        array(NA,dim=c(end.rows,add.columns,dim(x@alarms)[3])),along=2),
                                  array(NA,dim=c(dim(syndrome.counts)[1],(dim(x@alarms)[2]+add.columns),dim(x@alarms)[3])),
                                  along=1)
                }
                
                if (dim(x@UCL)[1]!=0){
                  UCL <- abind(abind(x@UCL[1:end.rows,,],
                                     array(NA,dim=c(end.rows,add.columns,dim(x@alarms)[3])),along=2),
                               array(NA,dim=c(dim(syndrome.counts)[1],(dim(x@UCL)[2]+add.columns),dim(x@UCL)[3])),
                               along=1)
                }
                
                if (dim(x@LCL)[1]!=0){
                  LCL <- abind(abind(x@LCL[1:end.rows,,],
                                     array(NA,dim=c(end.rows,add.columns,dim(x@alarms)[3])),along=2),
                               array(NA,dim=c(dim(syndrome.counts)[1],(dim(x@LCL)[2]+add.columns),dim(x@LCL)[3])),
                               along=1)
                }
              }
              
              
              
              y <- syndromicW(observed=observed, dates=datesy)
              setBaselineW(y) <- baseline
              setAlarmsW(y)<-alarms
              setUCLW(y)<-UCL
              setLCLW(y)<-LCL              
              
              
              
              
              
              
              
              
              
            }else{
            
              
              
            #set date interval from new data
            min.date <- min(as.Date (dates.var, format = date.format))
            max.date <- max(as.Date (dates.var, format = date.format))
            
            if (replace.dates==FALSE){
              min.date <- ISOweek2date(x@dates[dim(x@dates)[1],1])+7
            
              
              if(min.date>max.date){
                stop("The dataset provided contains no new data and replace.dates has been set to FALSE")
              }
              remove <- which(as.Date (dates.var, format = date.format) < min.date)
              id <- id[-remove]
              syndromes.var <- syndromes.var[-remove]
              dates.var<-dates.var[-remove]
            }
            
            
            
            
            #syndrome names 
            if (add.syndromes==TRUE){
              syndromes.name <- unique(c(colnames(x@observed),as.character(syndromes.var)))
            } else {
              syndromes.name <- colnames(x@observed)
            }
            
            
            #count cases for each syndromic group
            column <- 0
            
            for (syndrome in syndromes.name){
              column <- column+1
              
              if (length(which(syndromes.var==syndrome))==0) {
                nsubmissions <- rep(0,(max.date-min.date+1))
                } else {
                  
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

            
            min.week <- paste0(substr(as.character(date2ISOweek(min.date)),1,8),"-1")
              min.week <- ISOweek2date(min.week)
            ISOweek <-  paste0(substr(as.character(x@dates[,1]),1,8),"-1") 
              ISOweek <- ISOweek2date(ISOweek)
            
            end.rows <- max(which(ISOweek<min.week))
            
            add.columns <- dim(syndrome.counts)[2]-dim(x@observed)[2]
            baseline <- matrix(nrow=0,ncol=0)
            alarms <- array(dim=0)
            UCL <- array(dim=0)
            LCL <- array(dim=0)
            
            if (add.columns==0){
              observed <- rbind(x@observed[1:end.rows,],syndrome.counts)
              datesy <- rbind(x@dates[1:end.rows,],dates)
              
              if (dim(x@baseline)[1]!=0){
                baseline <- rbind(x@baseline[1:end.rows,],syndrome.counts)
              }
              
              if (dim(x@alarms)[1]!=0){
                alarms <- abind(x@alarms[1:end.rows,,],
                                array(NA,dim=c(dim(syndrome.counts)[1],dim(x@alarms)[2],dim(x@alarms)[3])),
                                along=1)
              }
              
              if (dim(x@UCL)[1]!=0){
                UCL <- abind(x@UCL[1:end.rows,,],
                             array(NA,dim=c(dim(syndrome.counts)[1],dim(x@UCL)[2],dim(x@UCL)[3])),
                             along=1)
              }
              
              if (dim(x@LCL)[1]!=0){
                LCL <- abind(x@LCL[1:end.rows,,],
                             array(NA,dim=c(dim(syndrome.counts)[1],dim(x@LCL)[2],dim(x@LCL)[3])),
                             along=1)    
              }
              
            } else{
              observed <- rbind(
                cbind(x@observed[1:end.rows,],matrix(0,nrow=end.rows,ncol=add.columns)),
                syndrome.counts)
              datesy <- rbind(x@dates[1:end.rows,],dates)
              
              if (dim(x@baseline)[1]!=0){
                baseline <- rbind(
                  cbind(x@baseline[1:end.rows,],matrix(0,nrow=end.rows,ncol=add.columns)),
                  syndrome.counts)
              }
              
              if (dim(x@alarms)[1]!=0){
                alarms <- abind(abind(x@alarms[1:end.rows,,],
                                      array(NA,dim=c(end.rows,add.columns,dim(x@alarms)[3])),along=2),
                                array(NA,dim=c(dim(syndrome.counts)[1],(dim(x@alarms)[2]+add.columns),dim(x@alarms)[3])),
                                along=1)
              }
              
              if (dim(x@UCL)[1]!=0){
                UCL <- abind(abind(x@UCL[1:end.rows,,],
                                   array(NA,dim=c(end.rows,add.columns,dim(x@alarms)[3])),along=2),
                             array(NA,dim=c(dim(syndrome.counts)[1],(dim(x@UCL)[2]+add.columns),dim(x@UCL)[3])),
                             along=1)
              }
              
              if (dim(x@LCL)[1]!=0){
                LCL <- abind(abind(x@LCL[1:end.rows,,],
                                   array(NA,dim=c(end.rows,add.columns,dim(x@alarms)[3])),along=2),
                             array(NA,dim=c(dim(syndrome.counts)[1],(dim(x@LCL)[2]+add.columns),dim(x@LCL)[3])),
                             along=1)
              }
            }
            
            
            
            y <- syndromicW(observed=as.matrix(observed), dates=datesy)
            setBaselineW(y) <- as.matrix(baseline)
            setAlarmsW(y)<-alarms
            setUCLW(y)<-UCL
            setLCLW(y)<-LCL
            
            }
            
            
            
            return(y)
            }
)

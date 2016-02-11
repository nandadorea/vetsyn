##' \code{syndromic_alarm}
##'
##' A simple command to generate an alarm around a syndromic (\code{syndromicD} 
##' or \code{syndromicW}) object - the
##' user can generate automated e-mails and also generate a pdf report of 
##' all syndromes in a syndromic object, or only those for which an 
##' alarm was generated.
##'
##'
##' @name syndromic_alarm
##' @docType methods
##' 
##' 
##' @param x a syndromic (\code{syndromicD} or \code{syndromicW}) object.
##' @param ... Additional arguments to the method.
##' @param pdf.report default is TRUE, that is, a pdf report will be generated. 
##' @param email.alarm.to email recipient(s) for when an alarm is detected. If
##' a pdf report has been generated, it will be attached to the email. See examples
##' for e-mail formats. Multiple e-mails should be separated
##' by commas. If set to NULL, no e-mail will be generated.
##' @param email.noalarm.to email recipient(s) for when NO alarm is detected. 
##' This is used so that a recipient can be warned that the calculations were performed
##' and finished successfully, but no alarm was generated (for control that the system
##' actuall ran with no problems). See examples
##' for e-mail formats. Multiple e-mails should be separated
##' by commas. If set to NULL, no e-mail will be generated.
##' @param date by default (NULL) it looks up alarms in the last date saved in the
##' syndromic object, but the user can set past dates in order to plot
##' historical alarms. Dates must be provided in the same format as they are
##' stored in dates, that is, for \code{syndromicD} objects, in the format
##' "yyyy-mm-dd" (for instance "2013-12-31"); and for \code{syndromicW} in the ISOweek
##' format (for instance "2014-W01-2")
##' @param plot.all by default, only syndromes associated with an
##' alarm are plotted (plot.all=FALSE), but the user can set plot.all=TRUE
##' to plot all syndromes found in the syndromic object. 
##' @param window the number of time points to plot, always finishing at the 
##' last time point recorded, or the date specified in the parameter "date" above.
##' @param baseline whether to plot the baseline, by default equal to TRUE.
##' @param UCL the dimension of the slot UCL, from the syndromic object, from which
##' the user wants to plot the UCL. Set to NULL or to 0 if it is not desired to plot the UCL.
##' @param algorithms an optional parameter specifying which dimensions
##' of the alarms slot to plot and sum for a final alarm score. 
##' If not specified (NULL), all are plotted. If set 
##' to zero, none are plotted.
##' @param limit the parameter specifying the limit above which alarms are 
##' considered meaningful. Only important if the user has specified that only
##' syndromes with an alarm are to be plotted. Remember that this is not a statistical 
##' value, but the sum of the scores of each individual detection algorithm. If for 
##' instance the syndromic object has been subjected to detection using a
##' \code{holt_winters_synd} algorithm with 3 alarm detection limits, and 
##' an \code{ewma_synd} algorithm with 3
##' alarm detection limits, than the maximum alarm score is 6. The limit parameter
##' establishes the minimum value (in this 0-6 scale) that in considered an alarm. By default
##' 1 is used. It can be provided as a single value (if all syndromes are to have
##' the same limit) or as a vector with length equal to the number of syndromes
##' in use (number of columns in the slot observed)
##' @param file.name an optional text to add to the date being evaluated, as
##' the name of the file to be saved.
##' @param email.from e-mail client to use when sending alarms, if left as NULL, the
##' default email in the package mail will be used, but it doesn't support
##' attachments (email will be sent without a pdf report attachment)
##' @param smtpServer to be used as control prameter in the mail sending function. 
##' The smtpServer for the email client provided as the "email.from" above. If left as NULL, the
##' default email in the package mail will be used, but it doesn't support attachments
##' (email will be sent without a pdf report attachment).
##' @param subject the subject in the email in case of alarms. By default
##' it's the system date, plus the text "ALARM: There are alarms today"
##' @param message any message that the user wants to add to the body of the email generated
##' in case of alarm. This will be added to the default message, which 
##' is the name of the syndromes which generated an alarm.
##' @param height in inches, for the pdf page. The default (7.5) fits well a letter or A4 page.
##' @param height in inches, for the pdf page. The default (10.5) fits well a letter or A4 page.
##' @param pdf.dir whether to create (or use) a directory called "PDF_reports" within the current
##' working directory, to save the files. If set to FALSE, the current working directory
##' is used. The current working directory is not changed by using the function.
##' @param width the width, in inches, if a pdf file is to be generated.
##' @param height the height, in inches, if a pdf file is to be generated.
##' 
##' @keywords methods
##' @import mail
##' @import sendmailR
##' @export
##' @examples
##'data(lab.daily)
##'my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  remove.dow=c(6,0),
##'                                  add.to=c(2,1),
##'                                  data=lab.daily)
##'my.syndromicD <- holt_winters_synd(x=my.syndromicD,
##'                                 evaluate.window=30,
##'                                 frequency=5,
##'                                 baseline.window=260)
##'                                 
##'syndromic_alarm(x=my.syndromicD,
##'                plot.all=TRUE,
##'                email.alarm.to="<dorea.meyer@@gmail.com>",
##'                email.noalarm.to="<dorea.meyer@@gmail.com>")
##'                


setGeneric('syndromic_alarm',
           signature = 'x',
           function(x, ...) standardGeneric('syndromic_alarm'))

setMethod('syndromic_alarm',
          signature(x = 'syndromicD'),
          function (x,
                    pdf.report=TRUE,
                    email.alarm.to=NULL,
                    email.noalarm.to=NULL,
                    date=NULL,
                    plot.all=FALSE,
                    window=365,
                    baseline=TRUE,
                    UCL=1,
                    algorithms=NULL,
                    limit=1,
                    email.from=NULL,
                    smtpServer=NULL,
                    subject= paste((Sys.Date()),"ALARM: There are alarms today",sep=","),
                    message=NULL,
                    height=7.5,
                    width=10.5,
                    pdf.dir=TRUE,
                    file.name=NULL)
        {
    
            
            ##check that limit is valid
            if (length(limit)==1){
              limit<-rep(limit,dim(x@observed)[2])
            }else{
              if (length(limit)!=dim(x@observed)[2]){
  stop("limit must be a single value or have the same length as the number
       of columns in @observed - number of syndromes in the object")
}
            }

     ##set date
        row <- dim(x@dates)[1]
     if (class(date)!="NULL"){
       row<-which(x@dates[,1]==as.Date(date))
     }


      algo.names<-dimnames(x@alarms)[[3]]
      #algorithms to be used
      if (class(algorithms)=="NULL") {
        alarms.array <- x@alarms
        algorithms <- 1:dim(x@alarms)[3]
      }else{
        alarms.array <- x@alarms[,,algorithms]
      }


      
      if(length(algorithms)==1&&algorithms!=0){
        n.algos <- 1
      }else{
        n.algos<-dim(alarms.array)[3]
      }
      alarms.sum<-apply(alarms.array,MARGIN=c(1,2),FUN="sum",na.rm=TRUE)
            

syndromes.alarm <- colnames(x@observed)[which(alarms.sum[row,]>=limit)]
syndromes.alarm.num <- match(syndromes.alarm,colnames(x@observed))


      ##define syndromes
       if (plot.all==TRUE){
         syndromes <- colnames(x@observed)
         }else{       
           syndromes <- syndromes.alarm
         }


        #window of plotting
        end<-row
        start<-max(1, end-window+1)      

        #syndrome=numeric
       syndromes.num <- match(syndromes,colnames(x@observed))



 body=list()
if (class(message)!="NULL"){
  body[[1]] <- print(message)
}
body[[length(body)+1]] <- list(paste("alarm for syndromes",print(syndromes.alarm),sep=":"))




if (pdf.report==TRUE){
  workdir <- getwd()    
      #set file.name
    if (class(file.name)=="NULL"){
      file.name <- paste((x@dates[row,1]),"pdf", sep=".")
    }else{
      file.name <- paste((x@dates[row,1]), file.name, "pdf", sep=".")  
    }


      
      #create or use directory
      if (pdf.dir==TRUE){
        dir.create(file.path(workdir, "PDF_reports"), showWarnings = FALSE)
        setwd(file.path(workdir, "PDF_reports"))
      }


 if(length(syndromes.num)==0){
   warning("There were no alarms for the date being evaluated,
        no PDF report was created. Set plot.all=TRUE if
        you would like to generate a PDF report even without alarms,
        or reduce the limit for meaningful alarms")
 }else{
       
pdf(file=file.name, height=7.5, width=10.5)


      for (s in syndromes.num){      

        #set limits
        ymax<-max(x@observed[start:end,s])
        ymax.bar <- max(1,max(alarms.sum[,s]))
        x.date <- x@dates[start:end,1]
        
        #set empty bar chart
        par(yaxt="s")
        plot(y=rep(0,length(x.date)),x=1:length(x.date), 
             ylim=c(0,ymax.bar), type="l", yaxt="n",xaxt="n",
             col="white",col.lab="white")
        if (n.algos>0){
        Axis(side=4,at=1:ymax.bar)        
        mtext("Final alarm score", side = 4, line=2)
        }
        
        #set grey bar of non-significant alarms
        
          polygon(x=c(min(0), min(0), length(x.date), length(x.date)), 
                  y=c(0,limit[s],limit[s],0),col="lightgray",border=NA)
        
        
        if(n.algos>0){
        legend (x=1, y=ymax.bar, title="Alarm Algorithm", 
                algo.names[algorithms],pch=18,col=2:(2+n.algos-1))
        }
        
        

        #plot observed data
        par(new=T, yaxt="n")
        plot(x@observed[start:end,s],x=x.date, yaxt="s", 
             ylim=c(0,ymax), type="l", 
             main=colnames(x@observed)[s],xlab="Days", ylab="Events")
        
        
        if (n.algos==1){
          par(new=T, yaxt="n")
          barplot(alarms.array[start:end,s,1], 
                  ylim=c(0,ymax.bar), border=2+a-1,col=2+a-1)
        }else{
                
        if (n.algos>0){
        for (a in 1:n.algos){
        par(new=T, yaxt="n")
        barplot(apply(as.matrix(alarms.array[start:end,s,(1+a-1):(n.algos)]),FUN="sum",
                      MARGIN=1,na.rm=TRUE), 
                ylim=c(0,ymax.bar), border=2+a-1,col=2+a-1)
        }
        }
        
        }
        
        par(new=T, yaxt="n")
        plot(x@observed[start:end,s],x=x.date, 
             ylim=c(0,ymax), type="l", lwd=1.5,  col.lab=0, ylab="",xlab="") 
        
        if (baseline==TRUE){
        lines(x=x.date, y=x@baseline[start:end,s],col="blue")
        }
        
        if (class(UCL)!="NULL"&&UCL>0){
          lines(x=x.date, y=x@UCL[start:end,s,UCL], col="red", lty=2)
        }
       
      }
dev.off()
graphics.off()

body[[length(body)+1]] = mime_part(file.name)  

 }
setwd(workdir)


} #end of if pdf.alarm==TRUE



if (length(syndromes.alarm)>0&&class(email.alarm.to)!="NULL"){
  
  if(class(email.from)!="NULL"&&class(smtpServer)!="NULL"){
    sendmailR::sendmail(from=email.from, 
                        to=email.alarm.to, 
                        subject=subject, 
                        msg=body,
                        control=list(smtpServer=smtpServer))
  }
  else{
    mail::sendmail(recipient=email.alarm.to,
             subject=subject,
             message=body[[1]], 
             password="rmail")
  } 
}


if (length(syndromes.alarm)==0&&class(email.noalarm.to)!="NULL"){
  
  if(class(email.from)!="NULL"&&class(smtpServer)!="NULL"){
    sendmailR::sendmail(from=email.from, 
                        to=email.alarm.to, 
                        subject=paste((Sys.Date()),"calculations finished, no alarm",sep=","), 
                        msg=body,
                        control=list(smtpServer=smtpServer))
  }
  else{
    mail::sendmail(recipient=email.alarm.to,
                   subject=paste((Sys.Date()),"calculations finished, no alarm",sep=","),
                   message=body[[1]], 
                   password="rmail")
  }        
          }
on.exit(setwd(workdir), add=TRUE)
}
)

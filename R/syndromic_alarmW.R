##' @name syndromic_alarm
##' @docType methods
##' @keywords methods
##' @import mail
##' @import sendmailR
##' @import ISOweek
##' @export
##' @examples
##'data(lab.daily)
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  data=lab.daily)
##'my.syndromicW <- ewma_synd(x=my.syndromicW,
##'                           evaluate.window=10,
##'                           limit.sd=c(2.5,3,3.5),
##'                           pre.process="diff",
##'                           diff.window=4)
##'                           
##'  ##substitute 'at' for the appropriate syMbol in the example below
##'syndromic_alarm(x=my.syndromicW,
##'                plot.all=TRUE,
##'                email.alarm.to="<dorea.meyer'at'gmail.com>",
##'                email.noalarm.to="<dorea.meyer'at'gmail.com>")
##'                


setMethod('syndromic_alarm',
          signature(x = 'syndromicW'),
          function (x,
                    pdf.report=TRUE,
                    email.alarm.to=NULL,
                    email.noalarm.to=NULL,
                    date=NULL,
                    plot.all=FALSE,
                    window=52,
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
              row<-which(as.character(x@dates[,1])==as.character(date))
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
                  x.date <- ISOweek2date(x@dates[start:end,1])
                  
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

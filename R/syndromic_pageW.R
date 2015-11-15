##' @name syndromic_page
##' @docType methods
##' @importFrom xtable xtable
##' @import ISOweek
##' @keywords methods
##' @export
##' @examples
##' data(lab.weekly)
##' my.syndromicW <- raw_to_syndromicW (id=lab.weekly$SubmissionID,
##'                                   syndromes.var=lab.weekly$Syndrome,
##'                                   week.var=lab.weekly$DateofSubmission)
##'                                   
##'my.syndromicW <- ewma_synd(x=my.syndromicW,
##'                           evaluate.window=10,
##'                           limit.sd=c(2.5,3,3.5),
##'                           pre.process="diff",
##'                           diff.window=4)
##'syndromic_page (x=my.syndromicW,
##'                 tpoints.display=4,
##'                 file.name="SpeciesX",
##'                 title="Lab data daily for Species X",
##'                 data.page=TRUE,
##'                 data=lab.weekly,
##'                 date.format="ISOweek",
##'                 dates.var="DateofSubmission",
##'                 syndromes.var="Syndrome",
##'                 scale=9)     
##'data(lab.daily)
##'my.syndromicW2 <- raw_to_syndromicW (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  data=lab.daily)
##'my.syndromicW2 <- ewma_synd(x=my.syndromicW2,
##'                           evaluate.window=10,
##'                           limit.sd=c(2.5,3,3.5),
##'                           pre.process="diff",
##'                           diff.window=4)
##'syndromic_page(x=my.syndromicW2,
##'                 tpoints.display=4,
##'                 file.name="SpeciesX",
##'                 title="Lab data daily for Species X",
##'                 data.page=TRUE,
##'                 data=lab.daily,
##'                 date.format="%d/%m/%Y",
##'                 dates.var="DateofSubmission",
##'                 syndromes.var="Syndrome",
##'                 scale=9) 


setMethod('syndromic_page',
          signature(x = 'syndromicW'),
          function (x,
                    tpoints.display=4,
                    window=52,
                    baseline=TRUE,
                    UCL=1,
                    algorithms=NULL,
                    limit=1,
                    file.name="my.syndromic",
                    title="My syndromic",
                    data.page=FALSE,
                    data=NULL,
                    date.format="ISOweek",
                    dates.var=NULL,
                    syndromes.var=NULL,
                    color.null="F8F8FF",
                    color.low="F8FF2F",
                    color.alarm="FF0000",
                    scale=9, 
                    fill.colors=c("yellow2","orange","tomato"),
                    arrow.colors=c("green","orange","tomato","red"))
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
            
            
            ##which algorithms to use
            algo.names<-dimnames(x@alarms)[[3]]
            #algorithms to be used
            if (class(algorithms)=="NULL") {
              alarms.array <- x@alarms
              algorithms <- 1:dim(x@alarms)[3]
            }else{
              alarms.array <- x@alarms[,,algorithms,drop=FALSE]
            }
            
            
            
            if(length(algorithms)==1&&algorithms!=0){
              n.algos <- 1
            }else{
              n.algos<-dim(alarms.array)[3]
            }
            alarms.sum<-apply(alarms.array,MARGIN=c(1,2),FUN="sum",na.rm=TRUE)
            
            
            
            ##define syndromes
            syndromes <- colnames(x@observed)
            syndromes.num <- 1:dim(x@observed)[2]
            
            #window of plotting
            end<-dim(x@observed)[1]
            start<-max(1, end-window+1)      
            
            
            workdir <- getwd()    
            
            #create or use directory
            dir.create(file.path(workdir, "html"), showWarnings = FALSE)
            workdir.html <- paste0(getwd(),"/html")
            
            dir.create(file.path(workdir.html, "figures"), showWarnings = FALSE)
            workdir.figures <- paste0(workdir.html,"/figures")
            
            
            #alarmometer figures
            setwd(workdir.figures)
            
            for (a in 0:scale){
              png(filename = paste0("alarmometer",a,".png"), width = 400, height = 350)  
              
              alarmometer(a,scale=scale,  
                          fill.colors=fill.colors,
                          arrow.colors=arrow.colors)
              dev.off()
              graphics.off() 
            }
            
            
            setwd(workdir.html)
            html <- file(paste(file.name,"html",sep="."), "w+")
            
            
            ###data.tables
            ###############################
            if(data.page==TRUE){
              data.tables1 = list()
              data.tables2 = list()
              
              
              if (date.format=="ISOweek"){
                
                for (syndrome in syndromes.num){
                  
                  #making sure ISOweek format is recognized in both sets of data:
                  week.var.data <- as.character(data[,dates.var])
                  week.data <- as.numeric(substr(as.character(week.var.data),7,8))
                  year.data <- as.numeric(substr(as.character(week.var.data),1,4))
                  week.var.data <- create_isoweek(week.data,year.data,reference.day=1)
                   
                  
                  week.var.x <- as.character(x@dates[,1])
                  week.x <- as.numeric(substr(as.character(week.var.x),7,8))
                  year.x <- as.numeric(substr(as.character(week.var.x),1,4))
                  week.var.x <- create_isoweek(week.x,year.x,reference.day=1)
                  
                  
                  
                  data.tables1[[syndrome]]<-data[which(data[,syndromes.var]==syndromes[syndrome]&&
                                                         (week.var.data==week.var.x[dim(x@dates)[1]])),
                                                 ]
                  data.tables2[[syndrome]]<-data[which(data[,syndromes.var]==syndromes[syndrome]&&
                                                         (week.var.data>(week.var.x[dim(x@dates)[1]-tpoints.display]))),
                                                 ]
                }
                
                
                
              }else{
                
                #making sure ISOweek format is recognized:
                              
                week.var.x <- as.character(x@dates[,1])
                week.x <- as.numeric(substr(as.character(week.var.x),7,8))
                year.x <- as.numeric(substr(as.character(week.var.x),1,4))
                week.var.x <- create_isoweek(week.x,year.x,reference.day=1)
                        
              for (syndrome in syndromes.num){
                data.tables1[[syndrome]]<-data[which(data[,syndromes.var]==syndromes[syndrome]&&
                                                       (date2ISOweek(as.Date(data[,dates.var], format=date.format))
                                                        ==week.var.x[dim(x@dates)[1]])),
                                               ]
                data.tables2[[syndrome]]<-data[which(data[,syndromes.var]==syndromes[syndrome]&&
                                                       (date2ISOweek(as.Date(data[,dates.var], format=date.format))
                                                        >(week.var.x[dim(x@dates)[1]-tpoints.display]))),
                                               ]
              }
            }
            
            }
            
            ####html page head
            ###########
            cat("<html>\n", file=html)
            cat("<head>\n", file=html)
            cat(sprintf("<title>%s</title>\n", paste(title, x@dates[dim(x@dates)[1],1],sep="-")), file=html)
            cat("</head>\n", file=html)
            
            cat("<body>\n", file=html)
            
            
            #cat(sprintf('<h1 align="center">%s</h1>\n', paste(title, (Sys.Date()),sep=" ")), file=html)
            cat('<a name="top"></a>\n', file=html)
            
            
            alarms.table<-rep(0,length(syndromes)*(tpoints.display+1))
            dim(alarms.table)<-c(length(syndromes),(tpoints.display+1))
            rownames(alarms.table)<- syndromes
            for (j in syndromes.num){
              alarms.table[j,1:tpoints.display] <- (alarms.sum[(end-(tpoints.display-1)):end,j])
              alarms.table[j,(tpoints.display+1)]   <- limit[j]
            }
            
            
            counts.table<-rep(0,length(syndromes)*(tpoints.display))
            dim(counts.table)<-c(length(syndromes),(tpoints.display))
            rownames(counts.table)<- syndromes
            for (j in syndromes.num){
              counts.table[j,1:tpoints.display] <- round(x@observed[(end-(tpoints.display-1)):end,j])  
            }
            
            colors.table = rep (color.null,length(counts.table))
            dim(colors.table)<-dim(counts.table)
            
            
            for (r in 1:dim(colors.table)[1]){
              for (c in 1:(dim(colors.table)[2])){
                if (alarms.table[r,c]>0) (colors.table[r,c]<-color.low)
                if (alarms.table[r,c]>=alarms.table[r,(tpoints.display+1)]) (colors.table[r,c]<-color.alarm)
              }}
            
            cat(sprintf('<h1 align="center">%s</h1>\n', paste(title, x@dates[dim(x@dates)[1],1],sep=" ")),file=html)
            
            cat("<TABLE border=\"1\" align=\"center\">\n", file=html)
            
            cat("<tr>\n", file=html)
            cat(sprintf("<td></td>\n<td colspan=\"2\"><center><b>Today</b></center></td>\n<td>____</td>\n
                        <td colspan=\"%s\"><center><b>Previous Days History</b></center></td>\n",
                        (tpoints.display-1)), file=html)
            cat("</tr>\n", file=html)
            
            col.head <- "<td>_D-1_</td>\n"
            
            cat("<tr>\n", file=html)
            if (tpoints.display>2){
              for (d in 2:(tpoints.display-1)){
                col.head <- paste0(col.head,"<td>_D-",d,"_</td>\n")
              }  
            }
            cat(paste0("<td></td>\n<td>Syndromic cases</td>\n<td><b>Alarm Today</b></td>\n<td></td>\n",
                       col.head), file=html)
            cat("</tr>\n", file=html)
            
            
            
            
            for (r in 1:dim(counts.table)[1]){
              
              row.fill <- "<td></td>\n"
              if (tpoints.display>1){
                for (d in 1:(tpoints.display-1)){
                  row.fill <- paste0(row.fill,
                                     "<td BGCOLOR='",colors.table[r,(tpoints.display-d)],"'><center>",
                                     counts.table[r,tpoints.display-d],"</center></td>\n")
                }  
              }  
              
              
              cat("<tr>\n", file=html)
              cat("<td><a href='#",rownames(counts.table)[r],"'>",rownames(counts.table)[r],"</a></td>\n
                  <td BGCOLOR='",colors.table[r,tpoints.display],"'><center>",counts.table[r,tpoints.display],
                  "</center></td>\n
                  <td BGCOLOR=\"",color.null,"\"><center><img src=\"",
                  paste("figures//alarmometer",alarms.table[r,tpoints.display],".png",sep=""),
                  "\" width=\"100\" height=\"80\" /></center></td>\n",
                  row.fill, file=html)
              cat("</tr>\n", file=html)
            }
            
            cat("</table>  \n", file=html)
            
            
            ##plots
            ##########################
            
            
            
            setwd(workdir.figures)
            
            par(mfrow=c(length(syndromes), 1))
            png(filename = paste0(file.name,"%03d.png"), width = 800, height = 400)
            
            for (s in syndromes.num){
              plot_syndromic(x,
                             syndromes=s,
                             window=window,
                             baseline=baseline,
                             UCL=UCL,
                             algorithms=algorithms,
                             limit=limit[s])
            }
            dev.off()
            graphics.off()
            
            
            setwd(workdir.html)
            
            for (p in syndromes.num){
              anchor = paste('<a name=" ',syndromes[p],'"></a>\n',sep="")
              cat(anchor, file=html)
              cat(sprintf('<h3 align="center">%s</h3>\n', paste(file.name, syndromes[p],(x@dates[dim(x@dates)[1],1]), sep=" ")), file=html)
              cat("<p align=\"center\">\n", file=html)
              cat("<img src=\"",paste("figures//",file.name,sprintf("%03d", p),".png",sep=""),"\"/>\n", file=html)
              cat("</p>\n", file=html)
              
              if (data.page==TRUE){
                cat("<TABLE border=\"0\">\n", file=html)
                cat("<tr>\n", file=html)
                cat(paste0("<td>\n<a href=\"#top\">Go back to top</a>\n</td>\n
                           <td>&nbsp;&nbsp;&nbsp;&nbsp;</td>\n<td>\n<a href=\"html2/",
                           file.name, p, ".html\">SEE THE SUBMISSIONS FOR THIS SYNDROME</a>\n</td>\n"),
                    file=html)
                cat("</tr>\n", file=html)
                cat("</table>\n", file=html)
                
                cat("<br></br>", file=html)
                cat("<br></br>", file=html)
              }
              
            }
            
            close(html)
            
            if (data.page==TRUE){
              dir.create(file.path(workdir.html, "html2"), showWarnings = FALSE)
              #workdir.html2 <- paste0(workdir.html,"/html2")
              #setwd(workdir.html2)
              
              
              for (syndrome in syndromes.num){
                html <- file(paste0("html2\\",file.name,syndrome,".html"), "w+")
                
                
                cat("<html>\n", file=html)
                cat("<head>\n", file=html)
                cat(sprintf("<title>%s</title>\n", paste(file.name,syndromes[syndrome],sep=" ")), file=html)
                cat("</head>\n", file=html)
                
                cat("<body>\n", file=html)
                
                cat(sprintf('<h1 align="center">%s</h1>\n', 
                            paste(file.name,syndromes[syndrome],x@dates[dim(x@dates)[1],1],sep=" - ")), file=html)
                
                cat(paste0("<a href=\"../",file.name,".html\">Go back to ",file.name, " main page</a>\n"),
                    file=html)
                
                cat(sprintf('<h3 align="center">%s</h3>\n', "Data from last day"), file=html)
                cat(print(xtable(data.tables1[[syndrome]],digits=0), type="html"), file=html)
                
                cat("<TABLE border=\"0\">\n", file=html)
                cat("<tr>\n", file=html)
                cat(paste0("<td>\n<a href=\"#top\">Go back to top</a>\n</td>\n<td>
                           &nbsp;&nbsp;&nbsp;&nbsp;</td>\n<td>\n<a href=\"../",
                           file.name,".html\">Go back to ", file.name, " main page</a>\n</td>\n"), 
                    file=html)
                cat("</tr>\n", file=html)
                cat("</table>\n", file=html)
                
                
                cat(sprintf('<h3 align="center">%s</h3>\n', "Data from the last week"), file=html)
                cat(print(xtable(data.tables2[[syndrome]],digits=0), type="html"), file=html)
                
                cat("<TABLE border=\"0\">\n", file=html)
                cat("<tr>\n", file=html)
                cat(paste0("<td>\n<a href=\"#top\">Go back to top</a>\n</td>\n<td>
               &nbsp;&nbsp;&nbsp;&nbsp;</td>\n<td>\n<a href=\"../",
                           file.name,".html\">Go back to ", file.name, " main page</a>\n</td>\n"), 
                    file=html)
                cat("</tr>\n", file=html)
                cat("</table>\n", file=html)
                
                
                cat("</body>\n", file=html)
                
                cat("</html>\n", file=html)
                
                close(html)
                
              }
            }
            
            
            on.exit(setwd(workdir),add=TRUE)
            }
)

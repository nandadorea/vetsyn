##' \code{syndromic_page}
##'
##' A simple command to generate an html page that summarizes the current 
##' state of a syndromic (\code{syndromicD} 
##' or \code{syndromicW}) object. A "html" folder is created (or used if already existing)
##' into the current working directory.
##'
##'
##'
##' @rdname syndromic_page-methods
##' @docType methods
##' 
##' 
##' @param x a syndromic (\code{syndromicD} 
##' or \code{syndromicW}) object.
##' @param ... Additional arguments to the method.
##' @param tpoints.display This is used to choose how many days of alarms to display. The
##' normal for daily data (syndromic object provided is form the class \code{syndromicD} )
##' is to show the entire last week (so 7 or 5 days, depending on whether weekends are
##' included). For weekly data the user my choose for instance 4 weeks (one month). 
##' The user can set this number
##' to any desired number of days to tabulate.
##' @param window the number of time points to plot, always finishing at the 
##' last time point recorded, or the date specified in the parameter "date" above.
##' @param baseline whether to plot the baseline, by default equal to TRUE.
##' @param UCL the dimension of the slot UCL, from the syndromic object, from which
##' the user wants to plot the UCL. Set to NULL or to 0 if it is not desired to plot the UCL.
##' @param algorithms an optional parameter specifying which dimensions
##' of the alarm slot to plot and sum for a final alarm score. 
##' If not specified (NULL), all are plotted. If set 
##' to zero, none are plotted.
##' @param limit the parameter specifying the limit above which alarms are 
##' considered meaningful. Only important if the user has specified that only
##' syndromes with an alarm are to be plotted. Remember that this is not a statistical 
##' value, but the sum of the scores of each individual detection algorithm. If for 
##' instance the syndromic object has been subjected to detection using a
##' \code{holt_winters_synd} algorithm with 3 alarm detection limits, 
##' and an \code{ewma_synd} algorithm with 3
##' alarm detection limits, than the maximum alarm score is 6. The limit parameter
##' establishes the minimum value (in his 0-6 scale) that in considered an alarm. By default
##' 1 is used. It can be provided as a single value (if all syndromes are to have
##' the same limit) or as a vector with length equal to the number of syndromes
##' in use (number of columns in the slot observed)
##' @param file.name an optional text to add to the date being evaluated, as
##' the name of the file to be saved.
##' @param title a tile for the html page. If not given the value in "file.name" is used.
##' @param data.page the user should specify whether the plots should also provide
##' a link to the original data, so that page viewers can inspect the original data. If
##' using TRUE, the original data must be provided in the argument "data".
##' @param data the original data to tabulate in case "data.page" has been set to TRUE.
##' @param date.format the date format in the original data, if provided in the previous argument.
##' For WEEKLY data, the original data may have been recorded daily or weekly (see 
##' \code{rawD_to_syndromicW} and \code{rawW_to_syndromicW}). If theoriginal date are
##' recorded daily, please indicate the date.format. If the original data are recorded
##' weekly, remember that the date format MUST be ISOweek, and for date.format
##' please provide the value "ISOweek". See examples.
##' @param syndromes.var the variable (column) in data to match to the syndromes
##' found in the slot observed
##' @param dates.var the variable (column) in data to look for dates, in order to
##' find the data from last week.
##' @param color.null a color for the table of alarms, used in cells giving the number
##' of syndromic events which corresponded to NO alarm.
##' @param color.low a color for the table of alarms, used in cells giving the number
##' of syndromic events which generated an alarm score higher than 0, but lower than
##' the limit for alarms.
##' @param color.alarm a color for the table of alarms, used in cells giving the number
##' of syndromic events which corresponded to an alarm.
##' @param scale the maximum possible score (scale of alarms). Based on the number
##' of detection algorithms used, and the number of detection limits set to each
##' (3 algorithms with 5 detection limits for each, for instance, will give a
##' scale of 15)
##' @param fill.colors the colors to be used for the 3 degrees of alarm. By default
##' they are yellow, orange and light red (tomato).
##' @param arrow.colors the colors of the arrow for varying levels of alarm. By
##' default green is used when the score is zero, and a progressive scale
##' is used for increasing scores: orange, tomato and red. 
##' 
##' @importFrom xtable xtable
##' @keywords methods

##' @examples
##'##DAILY data
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
##'syndromic_page (x=my.syndromicD,
##'                 tpoints.display=5,
##'                 file.name="SpeciesX",
##'                 title="Lab data daily for Species X",
##'                 data.page=TRUE,
##'                 data=lab.daily,
##'                 date.format="%d/%m/%Y",
##'                 dates.var="DateofSubmission",
##'                 syndromes.var="Syndrome",
##'                 scale=9)        
##' ##WEEKLY    
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
                                      



setGeneric('syndromic_page',
           signature = 'x',
           function(x, ...) standardGeneric('syndromic_page'))

##' @rdname syndromic_page-methods
##' @export

setMethod('syndromic_page',
          signature(x = 'syndromicD'),
          function (x,
                    syndromes=NULL,
                    tpoints.display=7,
                    window=365,
                    baseline=TRUE,
                    UCL=1,
                    algorithms=NULL,
                    limit=1,
                    file.name="my.syndromic",
                    title="My syndromic",
                    data.page=FALSE,
                    data=NULL,
                    date.format="%d/%m/%Y",
                    dates.var=NULL,
                    syndromes.var=NULL,
                    color.null="F8F8FF",
                    color.low="F8FF2F",
                    color.alarm="FF0000",
                     scale=9, 
                     fill.colors=c("yellow2","orange","tomato"),
                     arrow.colors=c("green","orange","tomato","red"))
        {
    
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if ((!is.character(syndromes))&&(!is.numeric(syndromes))) {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
            
            #syndromes index to be always numeric
            if (is.null(syndromes)){
              syndromes<-1:dim(x@observed)[2]
            }
            if (is.numeric(syndromes)) {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
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
          #syndromes <- colnames(x@observed)
          #syndromes.num <- 1:dim(x@observed)[2]

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
    
  for (syndrome in syndromes.num){
data.tables1[[syndrome]]<-data[which(data[,syndromes.var]==syndromes[syndrome]&
                (as.Date(data[,dates.var], format=date.format)==x@dates[dim(x@dates)[1],1])),
                ]
data.tables2[[syndrome]]<-data[which(data[,syndromes.var]==syndromes[syndrome]&
                (as.Date(data[,dates.var], format=date.format)>(x@dates[dim(x@dates)[1],1]-tpoints.display))),
                ]
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
rownames(alarms.table)<- colnames(x@observed[syndromes.num])
for (j in 1:length(syndromes.num)){
  alarms.table[j,1:tpoints.display] <- (alarms.sum[(end-(tpoints.display-1)):end,syndromes.num[j]])
  alarms.table[j,(tpoints.display+1)]   <- limit[syndromes.num[j]]
}


counts.table<-rep(0,length(syndromes)*(tpoints.display))
dim(counts.table)<-c(length(syndromes),(tpoints.display))
rownames(counts.table)<- <- colnames(x@observed[syndromes.num])
for (j in 1:length(syndromes.num)){
  counts.table[j,1:tpoints.display] <- 
    round(x@observed[(end-(tpoints.display-1)):end,syndromes.num[j]])  
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


##' @rdname syndromic_page-methods
##' @export

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
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if ((!is.character(syndromes))&&(!is.numeric(syndromes))) {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
            
            #syndromes index to be always numeric
            if (is.null(syndromes)){
              syndromes<-1:dim(x@observed)[2]
            }
            if (is.numeric(syndromes)) {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
            
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
            #syndromes <- colnames(x@observed)
            #syndromes.num <- 1:dim(x@observed)[2]
            
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
            rownames(alarms.table)<- <- colnames(x@observed[syndromes.num])
            for (j in 1:length(syndromes.num)){
              alarms.table[j,1:tpoints.display] <- (alarms.sum[(end-(tpoints.display-1)):end,syndromes.num[j]])
              alarms.table[j,(tpoints.display+1)]   <- limit[syndromes.num[j]]
            }
            
            
            counts.table<-rep(0,length(syndromes)*(tpoints.display))
            dim(counts.table)<-c(length(syndromes),(tpoints.display))
            rownames(counts.table)<- <- colnames(x@observed[syndromes.num])
            for (j in length(syndromes.num)){
              counts.table[j,1:tpoints.display] <- round(x@observed[(end-(tpoints.display-1)):end,syndromes.num[j]])  
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

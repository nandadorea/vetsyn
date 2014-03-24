##' \code{syndromic_page}
##'
##' A simple command to generate an html page that summarizes the current 
##' state of a syndromic object.A "html" folder is created (or used if already existing)
##' into teh current working directory.
##'
##'
##' @name syndromic_page
##' @docType methods
##' @seealso \code{\link{syndromic}}
##' @aliases syndromic_page
##' @aliases syndromic_page-methods
##' @aliases syndromic_page,syndromic-method
##' 
##' 
##' @param x a \code{syndromic} object.
##' @param week number of days in a week (for instance 7 for a full wee and 5 for 
##' weekdays only). This is used to choose how many days of alarms to display. The
##' normal is to show the entire last week, but the user can set this number
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
##' HolteWinter algorithm with 3 alarm detection limits, and an EWMA algorithm with 3
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
##' @param syndromes.var the variable (column) in data to match to the syndromes
##' found in the slot observed
##' @param dates.var the variable (column) in data to look for dates, in order to
##' find the data from last week.
##' @param color.null a color for the table of alarms, used in cells giving the number
##' of syndromic events which corresponded to no alarm.
##' @param color.low a color for the table of alarms, used in cells giving the number
##' of syndromic events which generated an alarm score higher than 0, but lower than
##' the limit for alarms.
##' @param color. alarm a color for the table of alarms, used in cells giving the number
##' of syndromic events which corresponded to an alarm.
##' @param scale the maximum possible score (scale of alarms). Based on the number
##' of detection algorithms used, and the number of detection limits set to each
##' (3 algorithms with 5 detection limits for each, for instance, will give a
##' scale of 15)
##' @param fill.colors the colors to be used for the 3 degrees of alarm. By default
##' they are yellow, organe and light red (tomato).
##' @param arrow.colors the colors of the arrow for varying levels of alarm. By
##' default green is used when the score is zero, and a progressive scale
##' is used for increasing scores: orange, romato and red. 
##' 
##' @importFrom xtable xtable
##' @keywords methods
##' @export
##' @examples
##'data(lab.daily)
##'my.syndromic <- raw_to_syndromic (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  remove.dow=c(6,0),
##'                                  add.to=c(2,1),
##'                                  data=lab.daily)
##'my.syndromic <- holt_winters_synd(x=my.syndromic,
##'                                 evaluate.window=30,
##'                                 frequency=5,
##'                                 baseline.window=260)
##'syndromic_page (x=my.syndromic,
##'                 week=5,
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

setMethod('syndromic_page',
          signature(x = 'syndromic'),
          function (x,
                    week=7,
                    window=365,
                    baseline=TRUE,
                    UCL=1,
                    algorithms=NULL,
                    limit=1,
                    file.name="my.syndromic",
                    title=file.name,
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
        alarms.array <- x@alarms[,,algorithms]
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
    
  for (syndrome in syndromes.num){
data.tables1[[syndrome]]<-data[which(data[,syndromes.var]==syndromes[syndrome]&&
                (as.Date(data[,dates.var], format=date.format)==x@dates[dim(x@dates)[1],1])),
                ]
data.tables2[[syndrome]]<-data[which(data[,syndromes.var]==syndromes[syndrome]&&
                (as.Date(data[,dates.var], format=date.format)>(x@dates[dim(x@dates)[1],1]-week))),
                ]
}
}

####html page head
###########
cat("<html>\n", file=html)
cat("<head>\n", file=html)
cat(sprintf("<title>%s</title>\n", paste(title, Sys.Date(),sep="-")), file=html)
cat("</head>\n", file=html)

cat("<body>\n", file=html)


#cat(sprintf('<h1 align="center">%s</h1>\n', paste(title, (Sys.Date()),sep=" ")), file=html)
cat('<a name="top"></a>\n', file=html)


alarms.table<-rep(0,length(syndromes)*(week+1))
dim(alarms.table)<-c(length(syndromes),(week+1))
rownames(alarms.table)<- syndromes
for (j in syndromes.num){
  alarms.table[j,1:week] <- (alarms.sum[(end-(week-1)):end,j])
  alarms.table[j,(week+1)]   <- limit[j]
}


counts.table<-rep(0,length(syndromes)*(week))
dim(counts.table)<-c(length(syndromes),(week))
rownames(counts.table)<- syndromes
for (j in syndromes.num){
  counts.table[j,1:week] <- (x@observed[(end-(week-1)):end,j])  
}

colors.table = rep (color.null,length(counts.table))
dim(colors.table)<-dim(counts.table)


for (r in 1:dim(colors.table)[1]){
  for (c in 1:(dim(colors.table)[2])){
    if (alarms.table[r,c]>0) (colors.table[r,c]<-color.low)
    if (alarms.table[r,c]>=alarms.table[r,(week+1)]) (colors.table[r,c]<-color.alarm)
  }}

cat(sprintf('<h1 align="center">%s</h1>\n', paste(title, (Sys.Date()),sep=" ")),file=html)

cat("<TABLE border=\"1\" align=\"center\">\n", file=html)

cat("<tr>\n", file=html)
cat(sprintf("<td></td>\n<td colspan=\"2\"><center><b>Today</b></center></td>\n<td>____</td>\n
                <td colspan=\"%s\"><center><b>Previous Days History</b></center></td>\n",
            (week-1)), file=html)
cat("</tr>\n", file=html)

col.head <- "<td>_D-1_</td>\n"

cat("<tr>\n", file=html)
if (week>2){
  for (d in 2:(week-1)){
    col.head <- paste0(col.head,"<td>_D-",d,"_</td>\n")
  }  
}
cat(paste0("<td></td>\n<td>Syndromic cases</td>\n<td><b>Alarm Today</b></td>\n<td></td>\n",
           col.head), file=html)
cat("</tr>\n", file=html)




for (r in 1:dim(counts.table)[1]){

  row.fill <- "<td></td>\n"
  if (week>1){
    for (d in 1:(week-1)){
      row.fill <- paste0(row.fill,
                         "<td BGCOLOR='",colors.table[r,(week-d)],"'><center>",
                         counts.table[r,week-d],"</center></td>\n")
    }  
  }  
  
  
  cat("<tr>\n", file=html)
  cat("<td><a href='#",rownames(counts.table)[r],"'>",rownames(counts.table)[r],"</a></td>\n
      <td BGCOLOR='",colors.table[r,week],"'><center>",counts.table[r,week],
          "</center></td>\n
      <td BGCOLOR=\"",color.null,"\"><center><img src=\"",
          paste("figures//alarmometer",alarms.table[r,week],".png",sep=""),
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
  cat(sprintf('<h3 align="center">%s</h3>\n', paste(file.name, syndromes[p],(Sys.Date()-1), sep=" ")), file=html)
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
                paste(file.name,syndromes[syndrome],Sys.Date(),sep=" - ")), file=html)
    
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


setwd(workdir)
}
)

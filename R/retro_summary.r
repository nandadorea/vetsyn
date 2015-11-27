##' \code{retro_summary}
##'
##' Performs an exploratory, descriptive analysis of the time series of observed
##' data, for as many syndromic groups as under study, and outputs both a
##' markdown file, where the user can have access to all retrospective
##' analysis R codes, and an html summary (produced by knitting the .Rmd file).
##'
##' The summary should constitue a first step in the retrospective
##' exploratory analysis of available syndromic data. It is also intended to
##' serve as means to check the result of the creation
##' of an object of the class syndromic (\code{syndromicD} or 
##' \code{syndromicW}). That is, it is a convenient, fast
##' way to plot all syndromic time-series in the object.
##'
##' If the user wants to make changes to the summary produced, it is easy
##' to open the .Rmd file in RStudio and produce any changes to the R
##' code generated.
##'
##' @name retro_summary
##' @docType methods
##'
##' @param x a syndromic (\code{syndromicD} or \code{syndromicW}) object, 
##' from where dates and observed
##' data will be extracted.
##' @param ... Additional arguments to the method.
##' @param object.name a name for the title in the html file, by default
##' "my.syndromic".
##' @param file.name a name for the rmd/html file to be created with
##' the summary. The default is "syndromic.retro.summary". When changing the
##' file name remember to use quotes. Please note that the function will create
##' a subdirectory within the current working directory, where all files will be
##' saved. Make sure to check the current working directory (\code{getwd()}) and
##' set a convenient one if needed (\code{setwd()}). See examples.
##' @param frequency The cycle of data repetition. By default equal to 365 (year)
##' for objects of the class \code{syndromicD} and 52 weeks for objects of the
##' class \code{syndromicW}. For DAILY data without weekends, for instance, 
##' it should be set to 260. 
##' @param short By default set to FALSE. When set to TRUE, omits the
##' fitting of poisson and negative binomial distributions, displaying only
##' summary statistics and plots for each series.
##' 
##' @return A ".Rmd" file and a ".html" page with sections corresponding to each syndromic group
##' found in the slot observed of the syndromic object. These include:
##' \itemize{
##'   \item{daily and weekly plots}{
##'     Line plots of the data found in the slot observed of the syndromic
##'     object provided. In the case of daily data, 
##'     Weekly plots are produced merging the daily data by week.
##'   }
##'
##'   \item{basic summary statistics}{
##'     Such as mean, quartiles, auto-correlation and partial auto-correlation.
##'   }
##'
##'   \item{box-plots}{
##'     of the data by day-of-week, month and year, intended to allow a
##'     preliminary assessment of which temporal effects are present (day-of-week,
##'     seasonal or trends). Plots vary depending on whether the data provided is monitored daily
##'     (\code{syndromicD}) or weekly (\code{syndromicW})
##'   }
##'
##'   \item{Poisson model fitting}{
##'     Fitting of a Poisson model to the data using a formula specified by the user.
##'   }
##'
##'   \item{Negative Binomial model fitting}{
##'     Fitting of a negative binomial model to the data using a formula specified by the user.
##'   }
##'
##' }
##' @keywords methods
##' @export
##' @import MASS 
##' @import knitr 
##' @examples
##' data(lab.daily)
##' my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##' retro_summary(my.syndromicD)
##'
##'
##'my.syndromicD <- raw_to_syndromicD (id=lab.daily$SubmissionID,
##'                                  syndromes.var=lab.daily$Syndrome,
##'                                  dates.var=lab.daily$DateofSubmission,
##'                                  date.format="%d/%m/%Y")
##'wd = getwd()
##'setwd(paste0(wd,"/retro"))
##'retro_summary(my.syndromicD)
##'setwd(wd)
##'


setGeneric('retro_summary',
           signature = 'x',
           function(x, ...) standardGeneric('retro_summary'))

setMethod('retro_summary',
          signature(x = 'syndromicD'),
          function (x,
                    object.name="my.syndromic",
                    file.name="syndromic.retro.summary",
                    frequency=365,
                    short=FALSE)
          {

            workdir <- getwd()
   dir.create(file.path(workdir, "syndromic.retro.summary"), showWarnings = FALSE)
   setwd(file.path(workdir, "syndromic.retro.summary"))
            
            
            #object.name <- as.character(match.call()[[2]])
            
            save(x, file=paste0(object.name,".RData"))
            
            rmd <- file(paste0(file.name,".Rmd"), "w+")

    cat(paste0("# Retrospective analysis summary for  ",object.name,"\n"), file=rmd)
            cat("\n", file=rmd)
    
            cat("```{r}\n", file=rmd)
            cat("#loading and preparing the data to work with\n", file=rmd)
            #cat("workdir <- getwd()\n", file=rmd)
            #cat("setwd(file.path(workdir, \"syndromic.retro.summary\"))\n", file=rmd)
            cat(paste0("load(\"",object.name,".RData\")\n"), file=rmd)
            
            cat("require(vetsyn)\n", file=rmd)
   
            cat("matrix.days <- x@observed\n", file=rmd)
            cat("matrix.week.full <- convert_days_to_week(x@observed,x@dates)\n", file=rmd)
            cat("matrix.week <- matrix.week.full[,-(1:2),drop=FALSE]\n", file=rmd)
            cat(paste0("frequency=",frequency,"\n"), file=rmd)
            
            cat("```\n", file=rmd)
            cat("\n", file=rmd)
    
            
            matrix.days <- x@observed

            #iterate for all syndromic groups
            for (s in 1:dim(matrix.days)[2]) {
              
              syndrome <- colnames(matrix.days)[s]
              
              cat(paste0("## ", syndrome, "\n"), file=rmd)
              
              cat("```{r}\n", file=rmd)
              cat("#create time series\n", file=rmd)
              cat(paste("s=",s,"\n", sep=""),file=rmd)
              cat("days    <-  matrix.days[,s]\n", file=rmd)
              cat("days.ts <-  ts(days, start = c(x@dates$year[1], x@dates$yday[1]),\n", file=rmd)
              cat("               frequency = frequency)\n", file=rmd)
              cat("t = 1:length(days)\n", file=rmd)
              cat("\n", file=rmd)
              cat("week    <-  matrix.week[,s]\n", file=rmd)
              cat("week.ts <-  ts(week, start = c(matrix.week.full[1,2],as.numeric(substr(as.character(matrix.week.full[1,1]),7,8))),\n", file=rmd)
              cat("               frequency = 52)\n", file=rmd)
              cat("t.week <- 1:length(week)\n", file=rmd)
              cat("```\n", file=rmd)
              cat("\n", file=rmd)
              
              cat("```{r fig.width=10, fig.height=5}\n", file=rmd)              
              cat("#Plot series\n", file=rmd)
              cat("plot(days.ts , xlab=\"Days\", main=\"Daily\")\n", file=rmd)
              cat("plot(week.ts , xlab=\"Weeks\",main=\"Weeks\")\n", file=rmd)
              cat("```\n", file=rmd)
              cat("\n", file=rmd)
              

              cat("### Summary statistics\n", file=rmd)
              

              cat("```{r}\n", file=rmd)
              cat("#Percentiles\n", file=rmd)
              cat("quantile(days,probs=c(0,0.25,0.5,0.75,1),names=FALSE)\n", file=rmd)
              cat("round(mean(days),4)\n", file=rmd)
              cat("\n", file=rmd) 
              cat("#Number of days at minimum value\n", file=rmd) 
              cat("(countInMin <- length(which(days == min(days))))\n", file=rmd)
              cat("(percentInMin <- round(((countInMin)/(length(days)))*100,2))\n", file=rmd)
              cat("```\n", file=rmd)
              cat("\n", file=rmd)            

              cat("```{r fig.width=10, fig.height=5}\n", file=rmd)              
              cat("#ACF and PACF\n", file=rmd)
        cat("acf(days,main=\"ACF for daily data\")\n", file=rmd)
        cat("pacf(days,main=\"PACF for daily data\")\n", file=rmd)          
        cat("```\n", file=rmd)
        cat("\n", file=rmd)

             
              cat("### Crude (visual) assessment of temporal effects\n", file=rmd)
          
              cat("```{r fig.width=10, fig.height=5}\n", file=rmd)              
              cat("boxplot(days ~ x@dates$dow, main=\"Day of the Week\")\n", file=rmd)
          cat("boxplot(days ~ x@dates$month, main = \"Month\")\n", file=rmd)
          cat("boxplot(days ~ x@dates$year, main = \"Year\")\n", file=rmd)           
          cat("```\n", file=rmd)
          cat("\n", file=rmd)

             
              if (short==FALSE){
                
                cat("### POISSON  Regression with DOW, month or sin/cos wave\n", file=rmd)

            cat("```{r}\n", file=rmd)
            cat("distribution=\"poisson\"\n", file=rmd)
                  cat("\n", file=rmd)
            cat("month.f = as.factor (x@dates$month)\n", file=rmd)
            cat("dow.f <- as.factor (x@dates$dow)\n", file=rmd)   
            cat("cos = cos (2*pi*t/frequency)\n", file=rmd)
            cat("sin = sin (2*pi*t/frequency)\n", file=rmd)
            cat("tminus1<-c(days[1],days[1:(length(days)-1)])\n", file=rmd)
            cat("tminus2<-c(days[1:2],days[1:(length(days)-2)])\n", file=rmd)
            cat("tminus3<-c(days[1:3],days[1:(length(days)-3)])\n", file=rmd)
            cat("tminus4<-c(days[1:4],days[1:(length(days)-4)])\n", file=rmd)
            cat("tminus5<-c(days[1:5],days[1:(length(days)-5)])\n", file=rmd)
            cat("tminus6<-c(days[1:6],days[1:(length(days)-6)])\n", file=rmd)
            cat("tminus7<-c(days[1:7],days[1:(length(days)-7)])\n", file=rmd)
                  cat("\n", file=rmd)
            cat("fit1 = glm(days~ dow.f, family=distribution)\n", file=rmd)
            cat("fit1AR1 = glm(days~ dow.f + tminus1, family=distribution)\n", file=rmd)
            cat("fit1AR2 = glm(days~ dow.f + tminus1+ tminus2, family=distribution)\n", file=rmd)
            cat("fit1AR3 = glm(days~ dow.f + tminus1+ tminus2 + tminus3, family=distribution)\n", file=rmd)
            cat("fit1AR4 = glm(days~ dow.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)\n", file=rmd)
            cat("fit1AR5 = glm(days~ dow.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)\n", file=rmd)
            cat("fit1AR6 = glm(days~ dow.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)\n", file=rmd)
            cat("fit1AR7 = glm(days~ dow.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)\n", file=rmd)
                  cat("\n", file=rmd)
            cat("fit2 = glm(days~ dow.f+ month.f, family=distribution)\n", file=rmd)
            cat("fit2AR1 = glm(days~ dow.f+ month.f + tminus1, family=distribution)\n", file=rmd)
            cat("fit2AR2 = glm(days~ dow.f+ month.f + tminus1+ tminus2, family=distribution)\n", file=rmd)
            cat("fit2AR3 = glm(days~ dow.f+ month.f + tminus1+ tminus2 + tminus3, family=distribution)\n", file=rmd)
            cat("fit2AR4 = glm(days~ dow.f+ month.f +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)\n", file=rmd)
            cat("fit2AR5 = glm(days~ dow.f+ month.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)\n", file=rmd)
            cat("fit2AR6 = glm(days~ dow.f+ month.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)\n", file=rmd)
            cat("fit2AR7 = glm(days~ dow.f+ month.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)\n", file=rmd)
                  cat("\n", file=rmd)
            cat("fit3 = glm(days~ dow.f+ cos + sin, family=distribution)\n", file=rmd)
            cat("fit3AR1 = glm(days~ dow.f+ cos + sin + tminus1, family=distribution)\n", file=rmd)
            cat("fit3AR2 = glm(days~ dow.f+ cos + sin + tminus1+ tminus2, family=distribution)\n", file=rmd)
            cat("fit3AR3 = glm(days~ dow.f+ cos + sin + tminus1+ tminus2 + tminus3, family=distribution)\n", file=rmd)
            cat("fit3AR4 = glm(days~ dow.f  + cos + sin + \n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)\n", file=rmd)
            cat("fit3AR5 = glm(days~ dow.f+ cos + sin  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5, family=distribution)\n", file=rmd)
            cat("fit3AR6 = glm(days~ dow.f+ cos + sin  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6, family=distribution)\n", file=rmd)
            cat("fit3AR7 = glm(days~ dow.f+ cos + sin  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7, family=distribution)\n", file=rmd)
                cat("```\n", file=rmd)
                cat("\n", file=rmd)
                cat("\n", file=rmd)
                
                cat("```{r}\n", file=rmd)                
                cat("#Printing AICs\n", file=rmd)
            cat("AR1 <- c(fit1=fit1$aic,fit1AR1=fit1AR1$aic,fit1AR2=fit1AR2$aic,fit1AR3=fit1AR3$aic,
                          fit1AR4=fit1AR4$aic,fit1AR5=fit1AR5$aic,fit1AR6=fit1AR6$aic,fit1AR7=fit1AR7$aic)\n", file=rmd)
                cat("AR2 <- c(fit2=fit2$aic,fit2AR1=fit2AR1$aic,fit2AR2=fit2AR2$aic,fit2AR3=fit2AR3$aic,
                          fit2AR4=fit2AR4$aic,fit2AR5=fit2AR5$aic,fit2AR6=fit2AR6$aic,fit2AR7=fit2AR7$aic)\n", file=rmd)
                cat("AR3 <- c(fit3=fit3$aic,fit3AR1=fit3AR1$aic,fit3AR2=fit3AR2$aic,fit3AR3=fit3AR3$aic,
                          fit3AR4=fit3AR4$aic,fit3AR5=fit3AR5$aic,fit3AR6=fit3AR6$aic,fit3AR7=fit3AR7$aic)\n", file=rmd)
                cat("\n", file=rmd)
                cat("\n", file=rmd)                
              cat("print(AR1)\n", file=rmd)
              cat("print(AR2)\n", file=rmd)
              cat("print(AR3)\n", file=rmd)
                  cat("\n", file=rmd)
                cat("```\n", file=rmd)
                cat("\n", file=rmd)
                
              cat("```{r fig.width=10, fig.height=5}\n", file=rmd)                              
              cat("plot(t,days, type=\"l\",main=\"Poisson regression\")\n", file=rmd)
              cat("lines(fit1$fit, col=\"red\"   , lwd=2)\n", file=rmd)
              cat("lines(fit2$fit, col=\"blue\"  , lwd=2)\n", file=rmd)
              cat("lines(fit3$fit, col=\"green\" , lwd=2)\n", file=rmd)
              cat("legend(\"topleft\",pch=3,col=c(\"red\",\"blue\",\"green\"),
                  c(\"Day-of-Week\", \"Day-of-Week+Month\",\"Day-of-Week+sin/cos\"))\n", file=rmd)
            cat("\n", file=rmd)


            cat("### Negative Binomial Regression with DOW, month or sin/cos wave\n", file=rmd)
            
            cat("```{r}\n", file=rmd)
                cat("require(MASS)\n", file=rmd)                
            cat("fitNB1 = glm.nb(days~ dow.f)\n", file=rmd)
            cat("fitNB1AR1 = glm.nb(days~ dow.f + tminus1)\n", file=rmd)
            cat("fitNB1AR2 = glm.nb(days~ dow.f + tminus1+ tminus2)\n", file=rmd)
            cat("fitNB1AR3 = glm.nb(days~ dow.f + tminus1+ tminus2 + tminus3)\n", file=rmd)
            cat("fitNB1AR4 = glm.nb(days~ dow.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4)\n", file=rmd)
            cat("fitNB1AR5 = glm.nb(days~ dow.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)\n", file=rmd)
            cat("fitNB1AR6 = glm.nb(days~ dow.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)\n", file=rmd)
            cat("fitNB1AR7 = glm.nb(days~ dow.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)\n", file=rmd)
            cat("\n", file=rmd)
            cat("fitNB2 = glm.nb(days~ dow.f+ month.f)\n", file=rmd)
            cat("fitNB2AR1 = glm.nb(days~ dow.f+ month.f + tminus1)\n", file=rmd)
            cat("fitNB2AR2 = glm.nb(days~ dow.f+ month.f + tminus1+ tminus2)\n", file=rmd)
            cat("fitNB2AR3 = glm.nb(days~ dow.f+ month.f + tminus1+ tminus2 + tminus3)\n", file=rmd)
            cat("fitNB2AR4 = glm.nb(days~ dow.f+ month.f +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4)\n", file=rmd)
            cat("fitNB2AR5 = glm.nb(days~ dow.f+ month.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)\n", file=rmd)
            cat("fitNB2AR6 = glm.nb(days~ dow.f+ month.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)\n", file=rmd)
            cat("fitNB2AR7 = glm.nb(days~ dow.f+ month.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)\n", file=rmd)
            cat("\n", file=rmd)
            cat("fitNB3 = glm.nb(days~ dow.f+ cos + sin)\n", file=rmd)
            cat("fitNB3AR1 = glm.nb(days~ dow.f+ cos + sin + tminus1)\n", file=rmd)
            cat("fitNB3AR2 = glm.nb(days~ dow.f+ cos + sin + tminus1+ tminus2)\n", file=rmd)
            cat("fitNB3AR3 = glm.nb(days~ dow.f+ cos + sin + tminus1+ tminus2 + tminus3)\n", file=rmd)
            cat("fitNB3AR4 = glm.nb(days~ dow.f  + cos + sin + \n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4)\n", file=rmd)
            cat("fitNB3AR5 = glm.nb(days~ dow.f+ cos + sin  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5)\n", file=rmd)
            cat("fitNB3AR6 = glm.nb(days~ dow.f+ cos + sin  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6)\n", file=rmd)
            cat("fitNB3AR7 = glm.nb(days~ dow.f+ cos + sin  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4+ tminus5 +tminus6 +tminus7)\n", file=rmd)
                cat("```\n", file=rmd)
                cat("\n", file=rmd)
                cat("\n", file=rmd)
                
                cat("```{r}\n", file=rmd)                
                cat("#Printing AICs\n", file=rmd)
                cat("AR_NB1 <- c(fitNB1=fitNB1$aic,fitNB1AR1=fitNB1AR1$aic,fitNB1AR2=fitNB1AR2$aic,fitNB1AR3=fitNB1AR3$aic,
                          fitNB1AR4=fitNB1AR4$aic,fitNB1AR5=fitNB1AR5$aic,fitNB1AR6=fitNB1AR6$aic,fitNB1AR7=fitNB1AR7$aic)\n", file=rmd)
                cat("AR_NB2 <- c(fitNB2=fitNB2$aic,fitNB2AR1=fitNB2AR1$aic,fitNB2AR2=fitNB2AR2$aic,fitNB2AR3=fitNB2AR3$aic,
                          fitNB2AR4=fitNB2AR4$aic,fitNB2AR5=fitNB2AR5$aic,fitNB2AR6=fitNB2AR6$aic,fitNB2AR7=fitNB2AR7$aic)\n", file=rmd)
                cat("AR_NB3 <- c(fitNB3=fitNB3$aic,fitNB3AR1=fitNB3AR1$aic,fitNB3AR2=fitNB3AR2$aic,fitNB3AR3=fitNB3AR3$aic,
                          fitNB3AR4=fitNB3AR4$aic,fitNB3AR5=fitNB3AR5$aic,fitNB3AR6=fitNB3AR6$aic,fitNB3AR7=fitNB3AR7$aic)\n", file=rmd)
                cat("print(AR_NB1)\n", file=rmd)
            cat("print(AR_NB2)\n", file=rmd)
            cat("print(AR_NB3)\n", file=rmd)
                cat("```\n", file=rmd)
                cat("\n", file=rmd)
                
                cat("```{r fig.width=10, fig.height=5}\n", file=rmd)                              
                cat("plot(t,days, type=\"l\",main=\"Negative Binomial Regression\")\n", file=rmd)
            cat("lines(fitNB1$fit, col=\"red\"   , lwd=2)\n", file=rmd)
            cat("lines(fitNB2$fit, col=\"blue\"  , lwd=2)\n", file=rmd)
            cat("lines(fitNB3$fit, col=\"green\" , lwd=2)\n", file=rmd)
            cat("legend(\"topleft\",pch=3,col=c(\"red\",\"blue\",\"green\"),
                c(\"Day-of-Week\", \"Day-of-Week+Month\",\"Day-of-Week+sin/cos\"))\n", file=rmd)
            cat("\n", file=rmd)
            cat("```\n", file=rmd)
            cat("\n", file=rmd)
                cat("\n", file=rmd)
                
              }


            }
            #require(knitr)
            print(paste0("Summary saved successully into ",getwd(),"/",file.name,".Rmd,and knit into ",file.name,".html"))
            on.exit(close(rmd))
            on.exit(knitr::knit2html(paste0(file.name,".Rmd")), add=TRUE)
            on.exit(setwd(workdir), add=TRUE)

          }
)

##' @name retro_summary
##' @docType methods
##' @export
##' @import MASS 
##' @import knitr 
##' @examples
##' data(lab.daily)
##' my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                   syndromes.var=Syndrome,
##'                                   dates.var=DateofSubmission,
##'                                   date.format="%d/%m/%Y",
##'                                   data=lab.daily)
##' retro_summary(my.syndromicW)
##'
##'
##'my.syndromicW <- raw_to_syndromicW (id=lab.daily$SubmissionID,
##'                                  syndromes.var=lab.daily$Syndrome,
##'                                  dates.var=lab.daily$DateofSubmission,
##'                                  date.format="%d/%m/%Y")
##'wd = getwd()
##'setwd(paste0(wd,"/retro"))
##'retro_summary(my.syndromicW)
##'setwd(wd)
##'



setMethod('retro_summary',
          signature(x = 'syndromicW'),
          function (x,
                    object.name="my.syndromic",
                    file.name="syndromic.retro.summary",
                    frequency=52,
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
   
            cat("matrix.week <- x@observed\n", file=rmd)
            cat(paste0("frequency=",frequency,"\n"), file=rmd)
            
            cat("```\n", file=rmd)
            cat("\n", file=rmd)
    
            
            matrix.week <- x@observed

            #iterate for all syndromic groups
            for (s in 1:dim(matrix.week)[2]) {
              
              syndrome <- colnames(matrix.week)[s]
              
              cat(paste0("## ", syndrome, "\n"), file=rmd)
              
              cat("```{r}\n", file=rmd)
              cat("#create time series\n", file=rmd)
              cat(paste("s=",s,"\n", sep=""),file=rmd)
              cat("week    <-  matrix.week[,s]\n", file=rmd)
              cat("week.ts <-  ts(week, start = c(x@dates$year[1], x@dates$week[1]),\n", file=rmd)
              cat("               frequency = frequency)\n", file=rmd)
              cat("t = 1:length(week)\n", file=rmd)
              cat("\n", file=rmd)
              cat("```\n", file=rmd)
              cat("\n", file=rmd)
              
              cat("```{r fig.width=10, fig.height=5}\n", file=rmd)              
              cat("#Plot series\n", file=rmd)
              cat("plot(week.ts , xlab=\"Weeks\",main=\"Weeks\")\n", file=rmd)
              cat("```\n", file=rmd)
              cat("\n", file=rmd)
              

              cat("### Summary statistics\n", file=rmd)
              

              cat("```{r}\n", file=rmd)
              cat("#Percentiles\n", file=rmd)
              cat("quantile(week,probs=c(0,0.25,0.5,0.75,1),names=FALSE)\n", file=rmd)
              cat("round(mean(week),4)\n", file=rmd)
              cat("\n", file=rmd) 
              cat("#Number of weeks at minimum value\n", file=rmd) 
              cat("(countInMin <- length(which(week == min(week))))\n", file=rmd)
              cat("(percentInMin <- round(((countInMin)/(length(week)))*100,2))\n", file=rmd)
              cat("```\n", file=rmd)
              cat("\n", file=rmd)            

              cat("```{r fig.width=10, fig.height=5}\n", file=rmd)              
              cat("#ACF and PACF\n", file=rmd)
        cat("acf(week,main=\"ACF for weekly data\",lag.max=(frequency+5))\n", file=rmd)
        cat("pacf(week,main=\"PACF for weekly data\",lag.max=(frequency+5))\n", file=rmd)          
        cat("```\n", file=rmd)
        cat("\n", file=rmd)

             
              cat("### Crude (visual) assessment of temporal effects\n", file=rmd)
          
              cat("```{r fig.width=10, fig.height=5}\n", file=rmd)              
              cat("boxplot(week ~ x@dates$week, main=\"Week of the year\")\n", file=rmd)
          cat("boxplot(week ~ x@dates$year, main = \"Year\")\n", file=rmd)           
          cat("```\n", file=rmd)
          cat("\n", file=rmd)

             
              if (short==FALSE){
                
                cat("### POISSON  Regression with sin/cos wave\n", file=rmd)

            cat("```{r}\n", file=rmd)
            cat("distribution=\"poisson\"\n", file=rmd)
                  cat("\n", file=rmd)
            
            
            cat("cos = cos (2*pi*t/frequency)\n", file=rmd)
            cat("sin = sin (2*pi*t/frequency)\n", file=rmd)
            cat("year.f <- as.factor (x@dates$year)\n", file=rmd)   
            cat("tminus1<-c(week[1],week[1:(length(week)-1)])\n", file=rmd)
            cat("tminus2<-c(week[1:2],week[1:(length(week)-2)])\n", file=rmd)
            cat("tminus3<-c(week[1:3],week[1:(length(week)-3)])\n", file=rmd)
            cat("tminus4<-c(week[1:4],week[1:(length(week)-4)])\n", file=rmd)
                  cat("\n", file=rmd)
            
            cat("fit1 = glm(week ~ year.f, family=distribution)\n", file=rmd)
            cat("fit1AR1 = glm(week~ year.f + tminus1, family=distribution)\n", file=rmd)
            cat("fit1AR2 = glm(week~ year.f + tminus1+ tminus2, family=distribution)\n", file=rmd)
            cat("fit1AR3 = glm(week~ year.f + tminus1+ tminus2 + tminus3, family=distribution)\n", file=rmd)
            cat("fit1AR4 = glm(week~ year.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)\n", file=rmd)
                  cat("\n", file=rmd)

            cat("fit2 = glm(week ~ sin + cos, family=distribution)\n", file=rmd)
            cat("fit2AR1 = glm(week~ sin + cos + tminus1, family=distribution)\n", file=rmd)
            cat("fit2AR2 = glm(week~ sin + cos + tminus1+ tminus2, family=distribution)\n", file=rmd)
            cat("fit2AR3 = glm(week~ sin + cos + tminus1+ tminus2 + tminus3, family=distribution)\n", file=rmd)
            cat("fit2AR4 = glm(week~ sin + cos  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)\n", file=rmd)
            cat("\n", file=rmd)
            
            cat("fit3 = glm(week ~ t + sin + cos, family=distribution)\n", file=rmd)
            cat("fit3AR1 = glm(week~ t + sin + cos + tminus1, family=distribution)\n", file=rmd)
            cat("fit3AR2 = glm(week~ t + sin + cos + tminus1+ tminus2, family=distribution)\n", file=rmd)
            cat("fit3AR3 = glm(week~ t + sin + cos + tminus1+ tminus2 + tminus3, family=distribution)\n", file=rmd)
            cat("fit3AR4 = glm(week~ t + sin + cos  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4, family=distribution)\n", file=rmd)
            cat("\n", file=rmd)
            
                cat("```\n", file=rmd)
                cat("\n", file=rmd)
                cat("\n", file=rmd)
                
                cat("```{r}\n", file=rmd)                
                cat("#Printing AICs\n", file=rmd)
            cat("AR1 <- c(fit1=fit1$aic,fit1AR1=fit1AR1$aic,fit1AR2=fit1AR2$aic,fit1AR3=fit1AR3$aic,
                          fit1AR4=fit1AR4$aic)\n", file=rmd)
            cat("AR2 <- c(fit2=fit2$aic,fit2AR1=fit2AR1$aic,fit2AR2=fit2AR2$aic,fit2AR3=fit2AR3$aic,
                          fit2AR4=fit2AR4$aic)\n", file=rmd)
            cat("AR3 <- c(fit3=fit3$aic,fit3AR1=fit3AR1$aic,fit3AR2=fit3AR2$aic,fit3AR3=fit3AR3$aic,
                          fit3AR4=fit3AR4$aic)\n", file=rmd)
            cat("\n", file=rmd)
                cat("\n", file=rmd)                
            cat("print(AR1)\n", file=rmd)
            cat("print(AR2)\n", file=rmd)
            cat("print(AR3)\n", file=rmd)
            cat("\n", file=rmd)
                cat("```\n", file=rmd)
                cat("\n", file=rmd)
                
            cat("```{r fig.width=10, fig.height=5}\n", file=rmd)                              
            cat("plot(t,week, type=\"l\",main=\"Poisson regression\")\n", file=rmd)
            cat("lines(fit1$fit, col=\"red\"   , lwd=2)\n", file=rmd)
            cat("lines(fit2$fit, col=\"blue\"  , lwd=2)\n", file=rmd)
            cat("lines(fit3$fit, col=\"green\" , lwd=2)\n", file=rmd)
            cat("legend(\"topleft\",pch=3,col=c(\"red\",\"blue\",\"green\"),
                  c(\"year\", \"sin/cos\",\"t + sin/cos\"))\n", file=rmd)
            cat("\n", file=rmd)
            

            cat("### Negative Binomial Regression with sin/cos wave\n", file=rmd)
            
            cat("```{r}\n", file=rmd)
                cat("require(MASS)\n", file=rmd)                
            cat("fitNB1 = glm.nb(week ~ year.f)\n", file=rmd)
            cat("fitNB1AR1 = glm.nb(week~ year.f + tminus1)\n", file=rmd)
            cat("fitNB1AR2 = glm.nb(week~ year.f + tminus1+ tminus2)\n", file=rmd)
            cat("fitNB1AR3 = glm.nb(week~ year.f + tminus1+ tminus2 + tminus3)\n", file=rmd)
            cat("fitNB1AR4 = glm.nb(week~ year.f  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4)\n", file=rmd)
            cat("\n", file=rmd)
            
            cat("fitNB2 = glm.nb(week ~ sin + cos)\n", file=rmd)
            cat("fitNB2AR1 = glm.nb(week~ sin + cos + tminus1)\n", file=rmd)
            cat("fitNB2AR2 = glm.nb(week~ sin + cos + tminus1+ tminus2)\n", file=rmd)
            cat("fitNB2AR3 = glm.nb(week~ sin + cos + tminus1+ tminus2 + tminus3)\n", file=rmd)
            cat("fitNB2AR4 = glm.nb(week~ sin + cos  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4)\n", file=rmd)
            cat("\n", file=rmd)
            
            cat("fitNB3 = glm.nb(week ~ t + sin + cos)\n", file=rmd)
            cat("fitNB3AR1 = glm.nb(week~ t + sin + cos + tminus1)\n", file=rmd)
            cat("fitNB3AR2 = glm.nb(week~ t + sin + cos + tminus1+ tminus2)\n", file=rmd)
            cat("fitNB3AR3 = glm.nb(week~ t + sin + cos + tminus1+ tminus2 + tminus3)\n", file=rmd)
            cat("fitNB3AR4 = glm.nb(week~ t + sin + cos  +\n", file=rmd)
            cat("                tminus1+ tminus2+ tminus3+ tminus4)\n", file=rmd)
            cat("\n", file=rmd)
            
            
                cat("```\n", file=rmd)
                cat("\n", file=rmd)
                cat("\n", file=rmd)
                
                cat("```{r}\n", file=rmd)                
                cat("#Printing AICs\n", file=rmd)
            cat("AR_NB1 <- c(fitNB1=fitNB1$aic,fitNB1AR1=fitNB1AR1$aic,fitNB1AR2=fitNB1AR2$aic,fitNB1AR3=fitNB1AR3$aic,
                          fitNB1AR4=fitNB1AR4$aic)\n", file=rmd)
            cat("AR_NB2 <- c(fitNB2=fitNB2$aic,fitNB2AR1=fitNB2AR1$aic,fitNB2AR2=fitNB2AR2$aic,fitNB2AR3=fitNB2AR3$aic,
                          fitNB2AR4=fitNB2AR4$aic)\n", file=rmd)
            cat("AR_NB3 <- c(fitNB3=fitNB3$aic,fitNB3AR1=fitNB3AR1$aic,fitNB3AR2=fitNB3AR2$aic,fitNB3AR3=fitNB3AR3$aic,
                          fitNB3AR4=fitNB3AR4$aic)\n", file=rmd)
            cat("print(AR_NB1)\n", file=rmd)
            cat("print(AR_NB2)\n", file=rmd)
            cat("print(AR_NB3)\n", file=rmd)
            cat("```\n", file=rmd)
            cat("\n", file=rmd)
            
            cat("```{r fig.width=10, fig.height=5}\n", file=rmd)                              
            cat("plot(t,week, type=\"l\",main=\"Negative Binomial Regression\")\n", file=rmd)
            cat("lines(fitNB1$fit, col=\"red\"   , lwd=2)\n", file=rmd)
            cat("lines(fitNB2$fit, col=\"blue\"  , lwd=2)\n", file=rmd)
            cat("lines(fitNB3$fit, col=\"green\" , lwd=2)\n", file=rmd)
            cat("legend(\"topleft\",pch=3,col=c(\"red\",\"blue\",\"green\"),
                  c(\"year\", \"sin/cos\",\"t + sin/cos\"))\n", file=rmd)
            cat("\n", file=rmd)
            cat("```\n", file=rmd)
            cat("\n", file=rmd)
            cat("\n", file=rmd)
            
              }


            }
            #require(knitr)
            print(paste0("Summary saved successully into ",getwd(),"/",file.name,".Rmd,and knit into ",file.name,".html"))
            on.exit(close(rmd))
            on.exit(knit2html(paste0(file.name,".Rmd")), add=TRUE)
            on.exit(setwd(workdir), add=TRUE)

          }
)

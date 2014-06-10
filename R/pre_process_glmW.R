##' @name pre_process_glm
##' @docType methods
##' @keywords methods
##' @export
##' @importFrom MASS glm.nb
##' @examples
##'data(lab.daily)
##'my.syndromicW <- rawD_to_syndromicW (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  data=lab.daily)
##'pre_processed_data <- pre_process_glm(my.syndromicW)
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               syndromes="Musculoskeletal")
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               syndromes=c("GIT","Musculoskeletal"))
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               syndromes=3)
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               syndromes=c(1,3))
##'
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               family="nbinom")
##'pre_processed_data <- pre_process_glm(my.syndromicW,slot="baseline")



setMethod('pre_process_glm',
          signature(x = 'syndromicW'),
          function (x,
                    slot="observed",
                    syndromes=NULL,
                    family="poisson",
                    formula="trend+sin+cos",
                    frequency=52,
                    print.model=TRUE,
                    plot=TRUE)
          {
            
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if (class(syndromes)!="character"&&class(syndromes)!="numeric") {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
            
            
            ##check that valid dates are entered
            if (dim(x@observed)[1]!=dim(x@dates)[1]){
              stop("valid data not found in the slot dates")
            }
            
            
            #make sure syndrome list is always numeric
            #even if user gives as a list of names
            if (class(syndromes)=="numeric") {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
            #pulling data from the object to work out of the object
            if (slot=="baseline"){
              observed.matrix <- x@baseline
            } else {
              observed.matrix <- x@observed }
            
            processed.matrix <- matrix(NA,ncol=dim(observed.matrix)[2],nrow=dim(observed.matrix)[1])
            
            
            loop=0
            for (c in syndromes.num){      
              loop=loop+1
              
              week = observed.matrix[,c]
              t = 1:length(week)
              cos = cos (2*pi*t/frequency)
              sin = sin (2*pi*t/frequency)
              year <- as.factor(x@dates$year)
              AR1<-c(week[1],week[1:(length(week)-1)])
              AR2<-c(week[1:2],week[1:(length(week)-2)])
              AR3<-c(week[1:3],week[1:(length(week)-3)])
              AR4<-c(week[1:4],week[1:(length(week)-4)])
              trend=t
              
              fn.formula=as.formula(paste0("week~",formula))
              
              
              
              if (family=="nbinom"){
                #require(MASS)
                fit1     <- glm.nb(fn.formula)  
                predict1 <- predict(fit1, type="response", se.fit=TRUE)
                series   <- predict1$fit
                
              }else{
                #distribution=family            
                
                fit1 <- glm(fn.formula, family=family)
                predict1 <- predict(fit1, type="response", se.fit=TRUE)
                series   <- predict1$fit
              }    
              
              residuals <- week-series
              
              syndrome.name <- colnames(observed.matrix)[c]
              
              ##plotting and prinitng    
              if (print.model==TRUE){
                print(syndrome.name)
                print(fit1)    
              }
              
              
              if (plot==TRUE) {
                
                if (loop==1){
                  par(mfrow=c(length(syndromes),1),mar=c(2,4,2,2))}
                
                plot(residuals, type="l",ylab=syndrome.name)
                lines(week,col="grey")
                legend("topleft", pch=3,col=c("black","grey"),
                       c("Pre-processed series","Original series"))
              }       
              
              processed.matrix[,c] <- residuals
              
            }
            
            return(processed.matrix)
            
          }
)

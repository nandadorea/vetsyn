##' \code{pre_process_glm}
##'
##' Function to remove known temporal effects from time series. It fits
##' a glm model to the time series, and delivers the residuals. 
##' 
##' This function is provided for users interested in capturing
##' (saving or plotting) the results of this pre-processing step. 
##' However, in the context of syndromic
##' surveillance through objects of the class syndromic (\code{syndromicD} or \code{syndromicW}),
##' pre-processing is performed in conjunction with the application of
##' control-charts, saving results into an object of the 
##' class \code{syndromic} (D or W) (within
##' detection algorithms. - See ewma_synd(), shew_synd() and cusum_synd())
##'
##' @param x a syndromic (\code{syndromicD} or \code{syndromicW})
##'  object, which must have at least 
##' the slot of observed data and a data frame in the slot dates.
##' @param ... Additional arguments to the method.
##' @param slot the slot in the \code{syndromic} object to be processed,
##' by default, "observed", but this argument can be used to
##' change it to "baseline"
##' @param syndromes an optional parameter, if not specified, all
##' columns in the slot observed (or baseline if that
##' was chosen in the previous parameter) of the \code{syndromic} object
##' will be used. The user can choose to restrict the analyses to 
##' a few syndromic groups listing their name or column position
##' in the observed matrix. See examples.
##' @param family the GLM distribution family used, by default 
##' "poisson". if "nbinom" is used, the function
##' glm.nb is used instead.
##' @param formula the regression formula to be used, in the R formula format: y~x1+x2...
##' If none is provided, the function looks for formulas in the @formula slot of the syndromic object.
##' If a formula is provided when this function is called, then that formula is used. We
##' recommend providing a formula to test various models, but once a model is chosen,
##' we recommend saving that formula in the syndromic object using: 
##' my.syndromic@formula <- list(formula1,formula2...), for as many syndromes as the syndromic object has (columns in observed).
##' NA can be provided when a syndrome is not to be associated with a particular formula.
##' Any variables (x1, x2...) must be given the same name they have in the
##' slot @dates. When providing a formula, two options are possible: providing a single formula to be applied to all 
##' syndromes, or providing the same number of formulas (in a list) as the number of syndromes in the observed object, 
##' even if not of them will be used (see examples!)
##' The variables that are standard in that slot for DAILY data (\code{syndromicD}) are: 
##' trend (for a monotonic trend), year, month, dow (day of week),
##' sin, cos, Ar1 (auto-regressive for 1 days) to AR7. For WEEKLY data
##' (\code{syndromicW}): trend, sin, cos, year, and 1 to 4 
##' autoregressive variables.
##' These elements can be combined into any formula. 
##' Since the @dates slot can be customized by the user, any variables in the dates 
##' data.frame can be called into the formula
##' @param frequency in case pre-processing is applied using "glm" AND the sin/cos functions 
##' are used, the cycle of repetitions need to be set. The default is a year
##' (365 days or 52 weeks).
##' @param plot whether plots comparing observed data and the result of 
##' the pre-processing should be displayed.
##' @param print.model whether the result of model fitting should be
##' printed on the console. This is recommended when the user is 
##' exploring which dependent variables to keep or drop.
##' 
##' @return A matrix with all the pre-processed vectors. 
##'
##' @rdname pre_process_glm-methods
##' @docType methods
##' 
##' @keywords methods
##' @importFrom MASS glm.nb
##' @references Fernanda C. Dorea, Crawford W. Revie, Beverly J. McEwen, 
##' W. Bruce McNab, David Kelton, Javier Sanchez (2012). Retrospective 
##' time series analysis of veterinary laboratory data: 
##' Preparing a historical baseline for cluster detection in syndromic 
##' surveillance. Preventive Veterinary Medicine. 
##' DOI: 10.1016/j.prevetmed.2012.10.010.
##' @examples
##'## DAILY
##'data(lab.daily)
##'my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  remove.dow=c(6,0),
##'                                  add.to=c(2,1),
##'                                  data=lab.daily)
##'pre_processed_data <- pre_process_glm(my.syndromicD,
##'                               syndromes="Musculoskeletal",
##'                                      formula=list(y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7))
##'pre_processed_data <- pre_process_glm(my.syndromicD,
##'                               syndromes=c("GIT","Musculoskeletal"),
##'                               formula=list(NA,y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7,
##'                               days~dow+month,NA,NA))
##'pre_processed_data <- pre_process_glm(my.syndromicD,
##'                               syndromes=c("GIT","Musculoskeletal"),
##'                               formula=list(y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7))
##'                               
##'pre_processed_data <- pre_process_glm(my.syndromicD,
##'                               syndromes=3,
##'                                      formula=list(y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7))
##'pre_processed_data <- pre_process_glm(my.syndromicD,
##'                               syndromes=c(2,3),
##'                               formula=list(NA,y~dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7,
##'                               days~dow+month,NA,NA))
##' ##WEEKLY
##'data(lab.daily)
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  data=lab.daily)
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               syndromes="Musculoskeletal",
##'                               formula=list(y~year))
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               syndromes=c("GIT","Musculoskeletal"),
##'                               formula=list(NA,y~year,weeks~trend+sin+cos,NA,NA))
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               syndromes=3,
##'                               formula=list(y~year))
##'pre_processed_data <- pre_process_glm(my.syndromicW,
##'                               syndromes=c(1,3),
##'                               formula=list(y~year,NA,weeks~trend+sin+cos,NA,NA))
##'

                             
setGeneric('pre_process_glm',
           signature = 'x',
           function(x, ...) standardGeneric('pre_process_glm'))

##' @rdname pre_process_glm-methods
##' @export

setMethod('pre_process_glm',
          signature(x = 'syndromicD'),
          function (x,
                    slot="observed",
                    syndromes=NULL,
                    family="poisson",
                    formula=NULL,
                    frequency=365,
                    print.model=TRUE,
                    plot=TRUE)
          {
            
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if ((!is.character(syndromes))&&(!is.numeric(syndromes))) {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
            
            ##check that valid dates are entered
            if (dim(x@observed)[1]!=dim(x@dates)[1]){
              stop("valid data not found in the slot dates")
            }
            
            
            #make sure syndrome list is always numeric
            #even if user gives as a list of names
            if (is.null(syndromes)){
              syndromes<-1:dim(x@observed)[2]
            }
            if (is.numeric(syndromes)) {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
            
            #checking that a formula is available
            if ((class(formula)=="character")|(class(formula)=="NULL"&&length(x@formula)<dim(x@observed)[2])){
              stop("Formula needs to be provided as a LIST, each element in the
                   FORMULA format (y~x1 + x2...) (as per update in july 2017). You can provide a
                   single formula, or a list of formulas with as many elements as the number of syndromes
                   in the syndromic object (even if not all are being evaluated at this time). If a formula
                   is not provided, the function looks for one in the slot @formula of the syndromic object. See help")
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
              
              
              days = observed.matrix[,c]
              t = 1:length(days)
               cos = cos (2*pi*t/frequency)
              sin = sin (2*pi*t/frequency)
              AR1<-c(days[1],days[1:(length(days)-1)])
              AR2<-c(days[1:2],days[1:(length(days)-2)])
              AR3<-c(days[1:3],days[1:(length(days)-3)])
              AR4<-c(days[1:4],days[1:(length(days)-4)])
              AR5<-c(days[1:5],days[1:(length(days)-5)])
              AR6<-c(days[1:6],days[1:(length(days)-6)])
              AR7<-c(days[1:7],days[1:(length(days)-7)])
              
              var <- data.frame(days = days,
                                trend=t,
                                cos,sin,
                                AR1,AR2,AR3,AR4,AR5,AR6,AR7)
              var <- cbind(var,x@dates)
              var$month <- as.factor(var$month)
              var$dow <- as.factor(var$dow)
              var$year <- as.factor(var$year)
              

              if(class(formula)=="NULL"){
                formula.c <- x@formula[[c]]
              }else{
              if(length(formula)>1){
                formula.c <- formula[[c]]
              }  else{
                formula.c<-formula[[1]]
              } 
              }
              
              
              v<-all.vars(formula.c)
              v[[1]]<-"days"
              v<-v[!is.na(v)]
              m<-match(v,colnames(var))
              m<-m[!is.na(m)]
              
              var <- var[,m,drop=FALSE]
              
              if(length(v)==0){
                stop("Formula assignment did not work properly, check that
                      you have provided the formula as a list, even if with only a single object,
                     or if a true list is being provided, make sure there is a formula for 
                     each of the syndromes in the syndromic object (you can assign NA as the formula for 
                     syndromes you are not trying to evaluate, but you still need to provide 
                     a formula for each syndrome, unless you provide a list with a single formula.
                     See examples in the help for thie function")
              }else{
                fn.formula=as.formula(paste0("days~",paste0(v[-1],collapse="+")))
                }
              
           
              
              
              if (family=="nbinom"){
                #require(MASS)
                fit1     <- glm.nb(fn.formula, data=var)  
                predict1 <- predict(fit1, type="response", se.fit=TRUE)
                series   <- predict1$fit
                
              }else{
                #distribution=family            
                
                fit1 <- glm(fn.formula, family=family, data=var)
                predict1 <- predict(fit1, type="response", se.fit=TRUE)
                series   <- predict1$fit
              }    
              
              residuals <- days-series
              
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
                lines(days,col="grey")
                legend("topleft", pch=3,col=c("black","grey"),
                       c("Pre-processed series","Original series"))
              }       
              
              processed.matrix[,c] <- residuals
              
            }
            
            return(processed.matrix)
            
          }
)

##' @rdname pre_process_glm-methods
##' @export

# weekly ----

setMethod('pre_process_glm',
          signature(x = 'syndromicW'),
          function (x,
                    slot="observed",
                    syndromes=NULL,
                    family="poisson",
                    formula=NULL, 
                    frequency=52,
                    print.model=TRUE,
                    plot=TRUE)
          {
            
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if ((!is.character(syndromes))&&(!is.numeric(syndromes))) {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
            
            
            ##check that valid dates are entered
            if (dim(x@observed)[1]!=dim(x@dates)[1]){
              stop("valid data not found in the slot dates")
            }
            
            
            #make sure syndrome list is always numeric
            #even if user gives as a list of names
            if (is.null(syndromes)){
              syndromes<-1:dim(x@observed)[2]
            }
            if (is.numeric(syndromes)) {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
            #checking that a formula is available
            if ((class(formula)=="character")|(class(formula)=="NULL"&&length(x@formula)<dim(x@observed)[2])){
              stop("Formula needs to be provided as a LIST, each element in the
                   FORMULA format (y~x1 + x2...) (as per update in july 2017). You can provide a
                   single formula, or a list of formulas with as many elements as the number of syndromes
                   in the syndromic object (even if not all are being evaluated at this time). If a formula
                   is not provided, the function looks for one in the slot @formula of the syndromic object. See help")
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
              
              AR1<-c(week[1],week[1:(length(week)-1)])
              AR2<-c(week[1:2],week[1:(length(week)-2)])
              AR3<-c(week[1:3],week[1:(length(week)-3)])
              AR4<-c(week[1:4],week[1:(length(week)-4)])
              
              var <- data.frame(week = week,
                                trend=t,
                                cos,sin,
                                AR1,AR2,AR3,AR4)
              var <- cbind(var,x@dates)
              var$year <- as.factor(var$year)
              
              
              
              if(class(formula)=="NULL"){
                formula.c <- x@formula[[c]]
              }else{
                if(length(formula)>1){
                  formula.c <- formula[[c]]
                }  else{
                  formula.c<-formula[[1]]
                }
              }
              
              
              v<-all.vars(formula.c)
              v[[1]]<-"week"
              v<-v[!is.na(v)]
              m<-match(v,colnames(var))
              m<-m[!is.na(m)]
              
              var <- var[,m,drop=FALSE]
              
              
              if(length(v)==0){
                stop("Formula assignment did not work properly, check that
                      you have provided the formula as a list, even if with only a single object,
                     or if a true list is being provided, make sure there is a formula for 
                     each of the syndromes in the syndromic object (you can assign NA as the formula for 
                     syndromes you are not trying to evaluate, but you still need to provide 
                     a formula for each syndrome, unless you provide a list with a single formula.
                     See examples in the help for thie function")
              }else{
                fn.formula=as.formula(paste0("week~",paste0(v[-1],collapse="+")))
              }
              
              
              if (family=="nbinom"){
                #require(MASS)
                fit1     <- glm.nb(fn.formula, data=var)  
                predict1 <- predict(fit1, type="response", se.fit=TRUE)
                series   <- predict1$fit
                
              }else{
                #distribution=family            
                
                fit1 <- glm(fn.formula, family=family, data=var)
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

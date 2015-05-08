##' \code{ewma_synd}
##'
##' This function applies the \code{ewma()} algorithm (available in
##' the {qcc} R package. Here
##' it is employed as part of an iterative process to allow 
##' detection of outbreak signals. The additional features compared
##' to the regular \code{ewma()} algorithm are: 
##'  \itemize{
##'  \item{pre-processing:}{  Instead of applying EWMA directly to the time-series,
##'  it is possible to choose one of two pre-processing methods: (1) modeling
##'  and removing temporal effects with a GLM regression model (families
##'  "poisson","nbinom" or "gaussian"); (2) differencing to remove for 
##'  instance day-of-week effects. The user can of course also set pre-processing
##'  to FALSE, and apply no temporal effects removal to the data.}
##'  \item{iterative application:} {  the algorithm is applied to a
##'  range of time points in an iterative manner, so if syndromic
##'  data need to be evaluated for the past 30 days, for instance,
##'  the function is called once and the internal loops evaluate
##'  one day at a time.}
##'  \item{Detection of deviations one day at a time:} {  in this 
##'  implementation rather than running the algorithm to multiple time
##'  units in a "batch", it applies the algorithm one time unit (e.g., day)
##'   at a time, so that aberrations detected in any given time unit
##'   can be corrected, before proceeding to the next. The correction
##'   of aberrations can be performed using this algorithm, or if
##'   the time series has already been corrected using another algorithm
##'   (with results saved in the slot \code{baseline} of the
##'   \code{syndromic} object being analysed), the corrected baseline
##'   will always be considered as trainig data, rather than the
##'   observed data (which may contain aberrations)}
##'   \item{guard-band:}{  The user can set a guard-band between the
##'   time unit being evaluated and the start of the window used
##'   as training data, in order to avoid contamination of the baseline
##'   with undetected outbreak-signals}.  
##'  \item{recording of the detection limits:}{  that is already a feature
##'  of the \code{ewma()} function, and in the syndromic application
##'  the LCL and UCL limits are stored in the appropriate slot of the object
##'  \code{syndromic}. The main innovation here is that if pre-processing
##'  methods are being used, the LCL and UCL are recorded after transformation of
##'  the values back to the scale of the original data, rather than being recorded
##'  in the scale of the residuals of pre-processing, which are the actual
##'  values used by the control-chart method.}
##'  \item{data correction:}{  in case an observation is found to be greater
##'  than the confidence interval of the forecast, the user can 
##'  choose to update the outbreak-free baseline by substituting the
##'  observed value with the UCL value. As mentioned before, this feature
##'  should not be used if the baseline was already constructed using another algorithm.}
##'  \item{multiple limits:}{  the user can apply the algorithm with multiple
##'   detection limits - that is to say, different
##'  confidence intervals}
##' } 
##'
##' @name ewma_synd
##' @docType methods
##'
##' @param x a syndromic (\code{syndromicD} or \code{syndromicW}) object. 
##' If pre-processing using
##' regression is going to be used, the slot dates must contain
##' a data.frame containing at least the columns for the regression 
##' variables chosen to be used.
##' @param syndromes an optional parameter, if not specified, all
##' columns in the slot observed of the syndromic object
##' will be used. The user can choose to restrict the analyses to 
##' a few syndromic groups listing their name or column position
##' in the observed matrix. See examples.
##' @param evaluate.window the number of time points to be evaluated.
##' By default only the last time point is evaluated, but the user can set 
##' any window (as long as the number of time points in the time series
##' allows so). 
##' @param baseline.window The baseline used to train the algorithm in order 
##' to provide a forecast, which will serve to decide whether the current 
##' observed data is expected. Normally 1-2 years.
##' @param lambda The lambda parameter to be passed to the function ewma()
##' @param limit.sd The limit of detection to be used, that is, the cut-off
##' of the confidence interval that decides when an observed is abnormal,
##' provided in number of standard deviations. This can be provided as 
##' a single value or as a vector. When a vector is provided, multiple 
##' detection results are given, and the alarm result stored is a sum
##' of how many detection limits were exceeded. 
##' @param alarm.dim The \code{syndromic} object is set to accept the result of 
##' multiple detection algorithms. These results are stored as a third 
##' dimension in the slot alarms. Here the user can choose which order in that 
##' dimension should store the results of this algorithm.
##' @param guard.band The number of time units used to separate the current
##' time unit evaluated and the baseline window. The default is 7 (assuming
##' weekly data). 
##' @param correct.baseline besides detecting abnormal observations, the algorithm
##' can also be used to correct the data, removing these observations and 
##' substituting them by the limit of the confidence interval of the prediction
##' provided by the ewma() algorithm. If that is to be carried out, 
##' the user needs to specify which of the provided limits is to be used
##' for the correction. This variable should be filled with the INDEX of the
##' limit to be used. For example, if limit.sd was provided as "c(2.5,3.0,3.5)", 
##' the use of correct.baseline=1 will cause the algorithm to substitute
##' any observations above 2.5 standard deviations from the predicted value with
##' this exact cut-off limit. If using correct.baseline=2, only observations
##' above a standard deviation of 3 (limit.sd[2]) will be corrected. To avoid that 
##' a baseline is generated or modified, set this argument to zero or NULL.
##' @param UCL the minimum number that would have generated an alarm, for every time point,
##' can be recorded in the slot UCL of the \code{syndromic} object.The user must provide 
##' the INDEX in the limit.sd vector for which the UCL values should be corrected 
##' (as explained for the argument correct.baseline above). Set to FALSE to prevent
##' the recording.
##' @param LCL default is FALSE. If set to an index of limit.sd (see UCL above)
##' then alarms are also generated when 
##' the observed number of events is LOWER than expected, 
##' and the maximum number of observations that 
##' would have generated a low alarm is recorded in the slot LCL. In this case
##' alarms are recorded as -1 for each detection limit met.
##' @param pre.process whether to pre-process the time series in order to
##' remove temporal effects before applying the control-chart. Set to FALSE to
##' apply the control-chart to the original, observed time series, using the 
##' data in the slot baseline as training (if the slot is empty, observed data will 
##' be copied into it). Set to "diff" to apply simple differencing. Set to  "glm"
##' to apply a regression model and deliver only the residuals to the control-chart.
##' The next arguments set details of either method.
##' @param diff.window only relevant if "pre.process" is set to "diff". 
##' Corresponds to the number of time units of differencing, default is
##' 7 (weekly differencing). Change to 5 if weekends do not contain weekend days.
##' @param family when using pre-processing using glm,
##' the GLM distribution family used, by default 
##' "poisson". if "nbinom" is used, the function
##' glm.nb is used instead.
##' @param formula the regression formula to be used. The following arguments
##' are accepted for DAILY data (\code{syndromicD} class objects provided): 
##' trend (for a monotonic trend), year, month, dow (day of week),
##' sin, cos, Ar1 (auto-regressive for 1 days) to AR7. For WEEKLY data
##' (\code{syndromicW} class objects provided): trend, sin, cos, year and 1 to 4 
##' autoregressive variables.
##' These elements can be combined
##' into any formula. The default for DAILY data is 
##' formula="dow+sin+cos+Ar1+Ar2+AR3+AR4+AR5" and for WEEKLY data
##' "trend+sin+cos" See examples.
##' @param frequency in case pre-processing is applied using "glm" AND the sin/cos functions 
##' are used, the cycle of repetitions need to be set. The default is a yearly cycle
##' (365 days or 52 weeks).
##' 
##' @seealso pre_process_glm
##' @seealso shew_synd
##' @seealso ewma_synd
##' @seealso holt_winters_synd
##' 
##' @return An object of the class syndromic (\code{syndromicD} or \code{syndromicW},
##' depending on which one was provided) which contains all 
##' elements from the object provided in x, but in which
##' the slot alarm has been filled in the following way: for the
##' rows assigned in evaluate.window, columns indicated in
##'  syndromes (or all columns from observed if syndromes is left undefined),
##'  and for the third dimension specified in alarm.dim (1 by default), 
##'  zeros have been assigned if no alarm was generated; otherwise a numerical
##'  value gives the number of alarms detected. That is, how many of the
##'  limits given in limits.sd detected an abnormal observation. See examples.
##'  If the user sets a correct.baseline value, the baseline will also have 
##'  been modified.
##' 
##' @keywords methods
##' @export
##' @importFrom stringr str_replace_all
##' @importFrom qcc ewma
##' @importFrom MASS glm.nb
##' @import abind
##' @examples
##'data(lab.daily)
##'my.syndromicD <- raw_to_syndromicD (id=SubmissionID,
##'                                  syndromes.var=Syndrome,
##'                                  dates.var=DateofSubmission,
##'                                  date.format="%d/%m/%Y",
##'                                  remove.dow=c(6,0),
##'                                  add.to=c(2,1),
##'                                  data=lab.daily)
##'my.syndromicD <- ewma_synd(x=my.syndromicD,
##'                          syndromes="Musculoskeletal",
##'                          evaluate.window=30,
##'                          baseline.window=260,
##'                          lambda=0.2,
##'                          limit.sd=c(2.5,3,3.5),
##'                          guard.band=5,
##'                          correct.baseline=FALSE,
##'                          alarm.dim=2,
##'                          pre.process="glm",
##'                          family="nbinom",
##'                          formula="dow+sin+cos+AR1+AR2+AR3+AR4+AR5",
##'                          frequency=260)
##'
##'my.syndromicD <- ewma_synd(x=my.syndromicD,
##'                           syndromes= c(1,2,4,5),
##'                           evaluate.window=30,
##'                           baseline.window=260,
##'                           lambda=0.2,
##'                           limit.sd=c(2.5,3,3.5),
##'                           guard.band=5,
##'                           correct.baseline=FALSE,
##'                           alarm.dim=2,
##'                           pre.process="diff",
##'                           diff.window=5)

setGeneric('ewma_synd',
           signature = 'x',
           function(x, ...) standardGeneric('ewma_synd'))

setMethod('ewma_synd',
          signature(x = 'syndromicD'),
          function (x,
                    syndromes=NULL,
                    evaluate.window=1,
                    baseline.window=365,
                    lambda=0.2,
                    limit.sd=c(2.5,3,3.5),
                    guard.band=7,
                    correct.baseline=FALSE,
                    alarm.dim=2,
                    UCL=1,
                    LCL=FALSE,
                    pre.process=FALSE,
                    diff.window=7,
                    family="poisson",
                    formula="dow+sin+cos+year+AR1+AR2+AR3+AR4+AR5+AR6+AR7",
                    frequency=365
          )
        {
    
            ##check that syndromes is valid
            if (class(syndromes)=="NULL"){
              syndromes <- colnames(x@observed)
            }else{
              if (class(syndromes)!="character"&&class(syndromes)!="numeric") {
                stop("if provided, argument syndromes must be a character or numeric vector")
              }
            }
            
            #syndromes index to be always numeric
            if (class(syndromes)=="numeric") {
              syndromes.num <- syndromes
            }else{
              syndromes.num <- match(syndromes,colnames(x@observed))
            }
            
           #pulling to a new object to modify directly in the object
y <- x            

            #require(abind)
            
            #if slot alarms does not exist at all, fill it with NA
            #for the minmum dimensions required
            if (dim(y@alarms)[1]==0){
              setAlarmsD(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
              if (length(dimnames(y@observed)[[2]])==1) {
                dimnames(y@alarms)[[2]] <- list(dimnames(y@observed)[[2]])
              } else{
                dimnames(y@alarms)[[2]] <- dimnames(y@observed)[[2]]
              }    
              dimnames(y@alarms)[[3]][alarm.dim] <- "EWMA"
              
            }
            
            #if slot existed, but not up to alarm.dim, add the needed dimensions
            if (dim(y@alarms)[3]<alarm.dim){
              add <- alarm.dim-dim(y@alarms)[3]
              for (d in (dim(y@alarms)[3]+1):(dim(y@alarms)[3]+add) ){
              y@alarms<-abind(y@alarms,
                              matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                              along=3)
              }
              dimnames(y@alarms)[[3]][alarm.dim] <- "EWMA"              
            }
            
            #all the same for UCL
            if (UCL!=FALSE){
            if (dim(y@UCL)[1]==0){
              setUCLD(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
              if (length(dimnames(y@observed)[[2]])==1) {
                dimnames(y@UCL)[[2]] <- list(dimnames(y@observed)[[2]])
              } else{
                dimnames(y@UCL)[[2]] <- dimnames(y@observed)[[2]]
              }   
              dimnames(y@UCL)[[3]][alarm.dim] <- "EWMA"
              
            }
            
            #if slot existed, but not up to alarm.dim, add the needed dimensions
            if (dim(y@UCL)[3]<alarm.dim){
              add <- alarm.dim-dim(y@UCL)[3]
              for (d in (dim(y@UCL)[3]+1):(dim(y@UCL)[3]+add) ){
                y@UCL<-abind(y@UCL,
                                matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                                along=3)
              }
              dimnames(y@UCL)[[3]][alarm.dim] <- "EWMA"              
            }
            }
            
            
            #all the same for LCL
            if (LCL!=FALSE){
              if (dim(y@LCL)[1]==0){
                setLCLD(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
                dimnames(y@LCL)[[2]] <- dimnames(y@observed)[[2]]
                dimnames(y@LCL)[[3]][alarm.dim] <- "EWMA"
                
              }
              
              #if slot existed, but not up to alarm.dim, add the needed dimensions
              if (dim(y@LCL)[3]<alarm.dim){
                add <- alarm.dim-dim(y@LCL)[3]
                for (d in (dim(y@LCL)[3]+1):(dim(y@LCL)[3]+add) ){
                  y@LCL<-abind(y@LCL,
                               matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2]),
                               along=3)
                }
                dimnames(y@LCL)[[3]][alarm.dim] <- "EWMA"                
              }
            }            
            
           #if Baseline does not exist at all, fill it with NA for the dimensions required 
            if (dim(y@baseline)[1]==0){
              setBaselineD(y)<-matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2],
                                     dimnames=dimnames(y@observed))
            }
            
            
 #number of time points to iterate           
range <- (dim(y@observed)[1]-evaluate.window+1):dim(y@observed)[1]                
            
            
for (syndrome in syndromes.num){
  
  if (sum(y@baseline[,syndrome],na.rm=TRUE)==0){
    y@baseline[,syndrome]<-y@observed[,syndrome]
  }
  
  for (tpoint in range){
      
      #if baseline for the syndrome in question is not available
      #(filled with NA just to reach dimensions necessary), then
      #for the syndrome in use replace with data from observed
      if (sum(y@baseline[,syndrome],na.rm=TRUE)==0){
        y@baseline[,syndrome]<-y@observed[,syndrome]
      }
    
      
if (pre.process=="diff"){
  
  start = tpoint-baseline.window-guard.band-diff.window
  end   = tpoint-1
  
  baseline <- c(y@baseline[start:end,syndrome],y@observed[tpoint,syndrome])
  to.cc    <- c(rep(NA,diff.window),diff(baseline,diff.window))
  
  correct  <- y@baseline[(tpoint-diff.window),syndrome]  
  
  
} else {
  if (pre.process=="glm"){
    
    
    start = tpoint-baseline.window-guard.band
    end   = tpoint-1
    
    attach(y@dates[start:end,],warn.conflicts=FALSE)
    
    days <- y@baseline[start:end,syndrome]
    t = 1:length(days)
    month = as.factor(y@dates$month[start:end])
    dow <- as.factor(y@dates$dow[start:end])
    cos = cos(2*pi*t/frequency)
    sin = sin(2*pi*t/frequency)
    year <- as.factor(y@dates$year[start:end])
    AR1<-y@baseline[(start-1):(end-1),syndrome]
    AR2<-y@baseline[(start-2):(end-2),syndrome]
    AR3<-y@baseline[(start-3):(end-3),syndrome]
    AR4<-y@baseline[(start-4):(end-4),syndrome]
    AR5<-y@baseline[(start-5):(end-5),syndrome]
    AR6<-y@baseline[(start-6):(end-6),syndrome]
    AR7<-y@baseline[(start-7):(end-7),syndrome]
    trend=t
    
    if(length(y@dates$holidays)>0){
      holidays <- y@dates$holidays
    }
    if(length(y@dates$afterholidays)>0){
      afterholidays <- y@dates$afterholidays
    }
    
    fn.formula=as.formula(paste0("days~",formula))
    
    
    #####for prediction part
    t.new = c((t[length(t)-guard.band+2]:t[length(t)]),(t[length(t)]+1))
    month.new = as.factor(y@dates$month[(tpoint-guard.band+1):(tpoint)])
    dow.new <- as.factor(y@dates$dow[(tpoint-guard.band+1):(tpoint)])
    cos.new = cos(2*pi*t.new/frequency)
    sin.new = sin(2*pi*t.new/frequency)
    year.new <- as.factor(y@dates$year[(tpoint-guard.band+1):(tpoint)])
    AR1.new<-y@baseline[(tpoint-guard.band):(tpoint-1),syndrome]
    AR2.new<-y@baseline[(tpoint-1-guard.band):(tpoint-2),syndrome]
    AR3.new<-y@baseline[(tpoint-2-guard.band):(tpoint-3),syndrome]
    AR4.new<-y@baseline[(tpoint-3-guard.band):(tpoint-4),syndrome]
    AR5.new<-y@baseline[(tpoint-4-guard.band):(tpoint-5),syndrome]
    AR6.new<-y@baseline[(tpoint-5-guard.band):(tpoint-6),syndrome]
    AR7.new<-y@baseline[(tpoint-6-guard.band):(tpoint-7),syndrome]
    
    
    if(length(y@dates$holidays)>0){
      holidays.new <- y@dates$holidays[(tpoint-guard.band+1):(tpoint)]
    }
    if(length(y@dates$afterholidays)>0){
      afterholidays.new <- y@dates$afterholidays[(tpoint-guard.band+1):(tpoint)]
    }
    
    
    new.data <- data.frame(t.new,month.new,dow.new,cos.new,sin.new,year.new,
                           AR1.new,AR2.new,AR3.new,AR4.new,AR5.new,
                           AR6.new,AR7.new)
    colnames(new.data) <- c("trend","month","dow","cos","sin","year",
                            "AR1","AR2","AR3","AR4","AR5",
                            "AR6","AR7")
    
    
     if(length(y@dates$holidays)>0){
      new.data <- cbind(new.data,holidays=holidays.new)
    }
    if(length(y@dates$afterholidays)>0){
      new.data <- cbind(new.data,afterholidays=afterholidays.new)
    }
    
    
    
    regular=colnames(new.data)
    formula <- str_replace_all(formula, pattern=" ", repl="")
    formula.items <- strsplit(formula,split="[+]")[[1]]
    
    
    if (length(which(is.na(match(formula.items,regular))==TRUE))>0){
      new <- which(is.na(match(formula.items,regular))==TRUE)
      
      for (n in new){
        assign(paste0(formula.items[n],".new"),
               with(y@dates,
                    get(formula.items[n])[(tpoint-guard.band+1):(tpoint)])
        )
        
        colnames2 <- c(colnames(new.data),formula.items[n])
        new.data <- cbind(new.data,get(paste0(formula.items[n],".new")))
        colnames(new.data) <- colnames2
        
      }
    }
      
    
    if (family=="nbinom"){
      #require(MASS)
      fit1     <- glm.nb(fn.formula)
      
    }else{
      #distribution=family
      fit1 <- glm(fn.formula, family=family)
    }
    
    predict.bas <- predict.glm(fit1,type="response")
    predict.new <- predict.glm(fit1, newdata=new.data,type="response")
    
    to.cc <- days - predict.bas
    to.cc <- c(to.cc,
                      (y@observed[tpoint] - predict.new[guard.band]) )
    correct <- predict.new[guard.band]
    
  }else{
    
    warning("You have not provided a valid pre-processing method,
              EWMA will be applied to your raw data - 
            acceptable methods are glm or diff, see help")
    
    start = tpoint-baseline.window-guard.band
    end   = tpoint-1
    
    to.cc <- c(y@baseline[start:end,syndrome],y@observed[tpoint,syndrome])
    correct <- 0
    
  }
}     
    
      for (l in 1:length(limit.sd)){
        #require(qcc)
        ewma1 = ewma1(to.cc,
                     center=mean(to.cc[1:(length(to.cc)-guard.band)],na.rm=TRUE),
                     std.dev=sd(to.cc[1:(length(to.cc)-guard.band)],na.rm=TRUE),
                     lambda=lambda,nsigmas=limit.sd[l],plot=FALSE)
        
        UCL.value= ceiling(correct  +  ewma1$limits[[length(ewma1$limits[,2]),2]])
        LCL.value= floor(correct    +  ewma1$limits[[length(ewma1$limits[,1]),1]])
        #before deciding if an alarm exists, a zero is automatically added to the
        #time point if this is the first loop for two reasons:
        #1-because if the data were never analysed, the slot had a NA before,
        #and adding 0 will signal that it has now been processed
        #2-because if the data HAS been analyzed before, we want the results of these
        #analyses to OVERRIDE, not to SUM to the previous analyses.
        if(l==1){
          y@alarms[tpoint,syndrome,alarm.dim]<-0
        }
        
        if (l==UCL){
          y@UCL[tpoint,syndrome,alarm.dim]<-UCL.value
        }
        
        if (l==LCL){
          y@LCL[tpoint,syndrome,alarm.dim]<-LCL.value
        }
        
        #ADD a one if the result of this loop was a detection
        if (y@observed[tpoint,syndrome]>max(0,UCL.value)){
          y@alarms[tpoint,syndrome,alarm.dim]<-y@alarms[tpoint,syndrome,alarm.dim]+1
        }
        
        if (LCL==l&&y@observed[tpoint,syndrome]<LCL.value){
          y@alarms[tpoint,syndrome,alarm.dim]<-y@alarms[tpoint,syndrome,alarm.dim]-1
        }
        
        
        #Correct baseline IF the user indicated so
        if (correct.baseline==l){
          y@baseline[tpoint,syndrome]<- y@observed[tpoint,syndrome]
          if (y@observed[tpoint,syndrome]>max(0,UCL.value)){
            y@baseline[tpoint,syndrome] <- max(0,UCL.value)
          }
        }
        
        
        
        
    }
  }
}
            
              return(y)          
            }
)

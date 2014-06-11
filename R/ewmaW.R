##' @name ewma_synd
##' @docType methods
##' @keywords methods
##' @export
##' @importFrom qcc ewma
##' @import abind
##' @importFrom MASS glm.nb
##' @examples
##'data(lab.daily)
##'my.syndromicW <- raw_to_syndromicW (id=SubmissionID,
##'                                     syndromes.var=Syndrome,
##'                                     dates.var=DateofSubmission,
##'                                     date.format="%d/%m/%Y",
##'                                     data=lab.daily)
##'my.syndromicW <- ewma_synd(x=my.syndromicW,
##'                          syndromes="Musculoskeletal",
##'                          evaluate.window=10,
##'                          baseline.window=104,
##'                          lambda=0.2,
##'                          limit.sd=c(2.5,3,3.5),
##'                          guard.band=2,
##'                          correct.baseline=FALSE,
##'                          alarm.dim=2,
##'                          pre.process="glm",
##'                          family="nbinom",
##'                          formula="trend+sin+cos",
##'                          frequency=52)
##'
##'my.syndromicW <- ewma_synd(x=my.syndromicW,
##'                           syndromes= c(1,2,4,5),
##'                           evaluate.window=10,
##'                           baseline.window=104,
##'                           lambda=0.2,
##'                           limit.sd=c(2.5,3,3.5),
##'                           guard.band=2,
##'                           correct.baseline=FALSE,
##'                           alarm.dim=2,
##'                           pre.process="diff",
##'                           diff.window=4)

setMethod('ewma_synd',
          signature(x = 'syndromicW'),
          function (x,
                    syndromes=NULL,
                    evaluate.window=1,
                    baseline.window=52,
                    lambda=0.2,
                    limit.sd=c(2.5,3,3.5),
                    guard.band=2,
                    correct.baseline=FALSE,
                    alarm.dim=2,
                    UCL=1,
                    LCL=FALSE,
                    pre.process=FALSE,
                    diff.window=4,
                    family="poisson",
                    formula="trend+sin+cos",
                    frequency=52
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
              setAlarmsW(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
              dimnames(y@alarms)[[2]] <- dimnames(y@observed)[[2]]
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
                setUCLW(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
                dimnames(y@UCL)[[2]] <- dimnames(y@observed)[[2]]
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
                setLCLW(y)<-array(NA,dim=c(dim(y@observed)[1],dim(y@observed)[2],alarm.dim))
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
              setBaselineW(y)<-matrix(NA,nrow=dim(y@observed)[1],ncol=dim(y@observed)[2],
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
                    
                    week <- y@baseline[start:end,syndrome]
                    t = 1:length(week)
                    cos = cos(2*pi*t/frequency)
                    sin = sin(2*pi*t/frequency)
                    year <- as.factor(y@dates$year[start:end])
                    AR1<-y@baseline[(start-1):(end-1),syndrome]
                    AR2<-y@baseline[(start-2):(end-2),syndrome]
                    AR3<-y@baseline[(start-3):(end-3),syndrome]
                    AR4<-y@baseline[(start-4):(end-4),syndrome]
                    trend=t
                    
                    fn.formula=as.formula(paste0("week~",formula))
                    
                    
                    #####for prediction part
                    t.new = c((t[length(t)-guard.band+2]:t[length(t)]),(t[length(t)]+1))
                    cos.new = cos(2*pi*t.new/frequency)
                    sin.new = sin(2*pi*t.new/frequency)
                    year.new <- as.factor(y@dates$year[(tpoint-guard.band+1):(tpoint)])
                    AR1.new<-y@baseline[(tpoint-guard.band):(tpoint-1),syndrome]
                    AR2.new<-y@baseline[(tpoint-1-guard.band):(tpoint-2),syndrome]
                    AR3.new<-y@baseline[(tpoint-2-guard.band):(tpoint-3),syndrome]
                    AR4.new<-y@baseline[(tpoint-3-guard.band):(tpoint-4),syndrome]
                    
                    
                    new.data <- data.frame(t.new,cos.new,sin.new,year.new,
                                           AR1.new,AR2.new,AR3.new,AR4.new)
                    colnames(new.data) <- c("trend","cos","sin","year",
                                            "AR1","AR2","AR3","AR4")
                    
                    
                    if (family=="nbinom"){
                      #require(MASS)
                      fit1     <- glm.nb(fn.formula)
                      
                    }else{
                      #distribution=family
                      fit1 <- glm(fn.formula, family=family)
                    }
                    
                    predict.bas <- predict.glm(fit1,type="response")
                    predict.new <- predict.glm(fit1, newdata=new.data,type="response")
                    
                    to.cc <- week - predict.bas
                    to.cc <- c(to.cc,
                               (y@observed[tpoint] - predict.new[guard.band]) )
                    correct <- predict.new[guard.band]
                    
                  }else{
                    
                    start = tpoint-baseline.window-guard.band
                    end   = tpoint-1
                    
                    to.cc <- c(y@baseline[start:end,syndrome],y@observed[tpoint,syndrome])
                    correct <- 0
                    
                  }
                }     
                
                for (l in 1:length(limit.sd)){
                  #require(qcc)
                  ewma1 = ewma(to.cc,
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

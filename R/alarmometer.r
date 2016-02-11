##' \code{alarmometer}
##'
##' Code to plot a syndromic final score when an alarm scale is used 
##'
##' @rdname alarmometer-methods
##' @docType methods
##' 
##' @param score the observed alarm score for which an alarm meter is desired.
##' @param scale the maximum possible score (scale of alarms). Based on the number
##' of detection algorithms used, and the number of detection limits set to each
##' (3 algorithms with 5 detection limits for each, for instance, will give a
##' scale of 15)
##' @param radius the radius of the circle to be drawn. It's generally not necessary 
##' to set this parameter, since the resulting figure doesn't show the scale.
##' @param center the center of plotting, but the scale is not shown, so generally
##' should not need to be changed.
##' @param fill.colors the colors to be used for the 3 degrees of alarm. By default
##' they are yellow, orange and light red (tomato).
##' @param arrow.colors the colors of the arrow for varying levels of alarm. By
##' default green is used when the score is zero, and a progressive scale
##' is used for increasing scores: orange, tomato and red. 
##' 
##' @export
##' @examples
##'alarmometer(0)
##'alarmometer(score=2,scale=9)
##'alarmometer(score=9,scale=12)
##'
alarmometer <-  function (score, 
                    scale=9, 
                    radius=4, 
                    center=5.5, 
                    fill.colors=c("yellow2","orange","tomato"),
                    arrow.colors=c("green","orange","tomato","red"))
          {

  
  base.angle <- (180/scale)*pi/2*(1/90)

  angle.vector <- rep (NA, scale+1)
  angle.vector[1] <- 0
  angle.vector[2] <- base.angle
  for (i in 3:(scale+1)) {angle.vector[i] <- base.angle+angle.vector[i-1]}
  x.vector <- angle.vector
  x.vector[1] <- center-radius
  for (i in 2:(scale+1)) {x.vector[i] <- center - radius*cos(angle.vector[i]) }
  y.vector <- angle.vector
  y.vector[1] <- 0; y.vector[scale+1] <- 0
  for (i in 2:(scale+1)) {y.vector[i] <- radius*sin(angle.vector[i])}
  
  
  
  #score.circle <- function() {
    
    plot(x=1:(radius*2 +2),y=rep(0,(radius*2 +2)),ylim=c(0,(radius+2)), col="white",axes=FALSE, ylab="", xlab="")  
    
  for (i in 1:(scale+1)){
  
    polygon(x=c(center,x.vector[i],x.vector[i+1]),y=c(0,y.vector[i],y.vector[i+1]),
            col="grey", border=NA)
  }

    
  #  }
  
      if (score>0){
    for (i in 1:score){
  fill.color <- fill.colors[1]
  if (i/scale>1/3&&i/scale<=2/3){fill.color <- fill.colors[2]}
  if (i/scale>2/3){fill.color <- fill.colors[3]}
  
  polygon(x=c(center,x.vector[i],x.vector[i+1]),y=c(0,y.vector[i],y.vector[i+1]),
          col=fill.color, border=NA)
  }
  }
    
  
  arrow.color <- arrow.colors[1]
  if (score>0&&score/scale<=1/3) {arrow.color <- arrow.colors[2]}
  if (score/scale>1/3&&score/scale<=2/3) {arrow.color <- arrow.colors[3]}
  if (score/scale>2/3) {arrow.color <- arrow.colors[4]}
  arrows(x0=5.5,y0=0,
         x1=x.vector[score+1],y1=y.vector[score+1],
         length=0.4,angle=30,code=2,lwd=10,
         col=arrow.color )
  

for (i in 1:(scale+1)){
  text(i-1,x=x.vector[i],y=y.vector[i],cex=2) }     
  
}

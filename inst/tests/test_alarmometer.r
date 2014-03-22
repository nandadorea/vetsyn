require(plotrix)

limit=9
score=5


alarmometer <- function(score, limit=9, radius=4, center=5.5, 
                        fill.colors=c("yellow2","orange","tomato"),
                        arrow.colors=c("green","orange","tomato","red")){
  
  
  base.angle <- (180/limit)*pi/2*(1/90)

  angle.vector <- rep (NA, limit+1)
  angle.vector[1] <- 0
  angle.vector[2] <- base.angle
  for (i in 3:(limit+1)) {angle.vector[i] <- base.angle+angle.vector[i-1]}
  x.vector <- angle.vector
  x.vector[1] <- center-radius
  for (i in 2:(limit+1)) {x.vector[i] <- center - radius*cos(angle.vector[i]) }
  y.vector <- angle.vector
  y.vector[1] <- 0; y.vector[limit+1] <- 0
  for (i in 2:(limit+1)) {y.vector[i] <- radius*sin(angle.vector[i])}
  
  
  
  #score.circle <- function() {
    
    plot(x=1:10,y=rep(0,10),ylim=c(0,radius), col="white",axes=FALSE, ylab="", xlab="")  
    draw.circle(center,0,radius=radius,nv=100,border=NULL,col="grey",lty=1,lwd=1)
    draw.circle(center,0,radius=1,nv=100,border=NULL,col="white",lty=1,lwd=1)
    
  #  }
  
      if (score>0){
    for (i in 1:score){
  fill.color <- fill.colors[1]
  if (i/limit>1/3&&i/limit<=2/3){fill.color <- fill.colors[2]}
  if (i/limit>2/3){fill.color <- fill.colors[3]}
  
  polygon(x=c(center,x.vector[i],x.vector[i+1]),y=c(0,y.vector[i],y.vector[i+1]),
          col=fill.color, border=NA)
  }
  }
    
  
  arrow.color <- arrow.colors[1]
  if (score>0&&score/limit<=1/3) {arrow.color <- arrow.colors[2]}
  if (score/limit>1/3&&score/limit<=2/3) {arrow.color <- arrow.colors[3]}
  if (score/limit>2/3) {arrow.color <- arrow.colors[4]}
  arrows(x0=5.5,y0=0,
         x1=x.vector[score+1],y1=y.vector[score+1],
         length=0.4,angle=30,code=2,lwd=10,
         col=arrow.color )
  

for (i in 1:(limit+1)){
  text(i-1,x=x.vector[i],y=y.vector[i],cex=2) }     
  
}
  

alarmometer(0,9)
alarmometer(1,9)
alarmometer(2,9)
alarmometer(3,9)
alarmometer(4,9)
alarmometer(5,9)
alarmometer(6,9)
alarmometer(7,9)
alarmometer(8,9)
alarmometer(9,9)

alarmometer(0,12)
alarmometer(1,12)
alarmometer(2,12)
alarmometer(3,12)
alarmometer(4,12)
alarmometer(5,12)
alarmometer(6,12)
alarmometer(7,12)
alarmometer(8,12)
alarmometer(9,12)
alarmometer(10,12)
alarmometer(11,12)
alarmometer(12,12)


alarmometer(0,8)
alarmometer(1,8)
alarmometer(2,8)
alarmometer(3,8)
alarmometer(4,8)
alarmometer(5,8)
alarmometer(6,8)
alarmometer(7,8)
alarmometer(8,8)

alarmometer(0,3)
alarmometer(1,3)
alarmometer(2,3)
alarmometer(3,3)

alarmometer(0,2)
alarmometer(1,2)
alarmometer(2,2)

alarmometer(0,1)
alarmometer(1,1)



dates_df <- function (min.date, max.date,
                      by="days",
                      date.format="%d/%m/%Y") {

  start <- as.Date(min.date, format = date.format)
  end   <- as.Date(max.date, format = date.format)

  require(ISOweek)
  
  dates.v <- seq(from=start,to=end, by=by)
  date <- strptime (as.character(dates.v), format = "%Y-%m-%d")
  year <- (date$year+1900)
  month <- date$mon
  mday <- date$mday
  yday <- date$yday
  dow <- date$wday
  week <- as.numeric(substring(ISOweek(date),7,8))
  weekday <- rep(1,length(date))
      weekday[dow==0|dow==6] <- 0
  dates <- as.data.frame(dates.v)
  colnames(dates) <- "dates"
  dates <- cbind(dates, mday, month, year, yday, week, dow, weekday)

  return(dates)

}





ewmaSmooth1 <- function (x, y, lambda = 0.2, start, ...) 
{
  if (length(y) != length(x)) 
    stop("x and y must have the same length!")
  if (abs(lambda) > 1) 
    stop("lambda parameter must be between 0 and 1")
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  n <- length(y)
  if (missing(start)) 
    start <- y[1]
  S1 <- diag(rep(1, n))
  nrow=dim(S1)[1]
  j <- rep(1:dim(S1)[1],dim(S1)[2])
  i <- rep(1:dim(S1)[2],each=dim(S1)[1])
  S1 <- as.vector(S1)
  S1 <- (1 - lambda)^(j - i)
  S1 <- (matrix(S1,nrow=nrow))
  S1[upper.tri(S1)] <- 0
  
  S2 <- (1 - lambda)^seq(1, n)
  z <- lambda * (S1 %*% y) + S2 * start
  list(x = x, y = z, lambda = lambda, start = start)
}



ewma1 <- function (data, sizes, center, std.dev, lambda = 0.2, nsigmas = 3, 
                   data.name, labels, newdata, newsizes, newlabels, plot = TRUE, 
                   ...) 
{
  call <- match.call()
  if (missing(data)) 
    stop("'data' argument is not specified")
  if (missing(data.name)) 
    data.name <- deparse(substitute(data))
  data <- data.matrix(data)
  if (missing(sizes)) {
    sizes <- apply(data, 1, function(x) sum(!is.na(x)))
  }
  else {
    if (length(sizes) == 1) 
      sizes <- rep(sizes, nrow(data))
    else if (length(sizes) != nrow(data)) 
      stop("sizes length doesn't match with data")
  }
  type <- if (any(sizes == 1)) 
    "xbar.one"
  else "xbar"
  if (missing(labels)) {
    if (is.null(rownames(data))) 
      labels <- 1:nrow(data)
    else labels <- rownames(data)
  }
  stats <- paste("stats.", type, sep = "")
  if (!exists(stats, mode = "function")) 
    stop(paste("function", stats, "is not defined"))
  stats <- do.call(stats, list(data, sizes))
  statistics <- stats$statistics
  if (missing(center)) 
    center <- stats$center
  sd <- paste("sd.", type, sep = "")
  if (!exists(sd, mode = "function")) 
    stop(paste("function", sd, "is not defined!"))
  if (missing(std.dev)) {
    std.dev <- switch(type, xbar = {
      if (any(sizes > 25)) "RMSDF" else "UWAVE-R"
    }, NULL)
    std.dev <- do.call(sd, list(data, sizes, std.dev))
  }
  else {
    if (is.character(std.dev)) {
      std.dev <- do.call(sd, list(data, sizes, std.dev))
    }
    else {
      if (!is.numeric(std.dev)) 
        stop("if provided the argument 'std.dev' must be a method available or a numerical value. See help(qcc).")
    }
  }
  names(statistics) <- rownames(data) <- labels
  names(dimnames(data)) <- list("Group", "Samples")
  object <- list(call = call, type = "ewma", data.name = data.name, 
                 data = data, statistics = statistics, sizes = sizes, 
                 center = center, std.dev = std.dev)
  if (!missing(newdata)) {
    newdata.name <- deparse(substitute(newdata))
    newdata <- data.matrix(newdata)
    if (missing(newsizes)) {
      newsizes <- apply(newdata, 1, function(x) sum(!is.na(x)))
    }
    else {
      if (length(newsizes) == 1) 
        newsizes <- rep(newsizes, nrow(newdata))
      else if (length(newsizes) != nrow(newdata)) 
        stop("newsizes length doesn't match with newdata")
    }
    stats <- paste("stats.", type, sep = "")
    if (!exists(stats, mode = "function")) 
      stop(paste("function", stats, "is not defined"))
    newstats <- do.call(stats, list(newdata, newsizes))$statistics
    if (missing(newlabels)) {
      if (is.null(rownames(newdata))) {
        start <- length(statistics)
        newlabels <- seq(start + 1, start + length(newstats))
      }
      else {
        newlabels <- rownames(newdata)
      }
    }
    names(newstats) <- newlabels
    object$newstats <- newstats
    object$newdata <- newdata
    object$newsizes <- newsizes
    object$newdata.name <- newdata.name
    statistics <- c(statistics, newstats)
    sizes <- c(sizes, newsizes)
  }
  n <- length(statistics)
  indices <- 1:length(statistics)
  ewma <- ewmaSmooth1(indices, statistics, lambda = lambda, 
                      start = center)
  sigma2 <- std.dev^2/sizes * ((lambda/(2 - lambda)) * (1 - 
                                                          (1 - lambda)^(2 * (1:n))))
  ucl <- center + nsigmas * sqrt(sigma2)
  lcl <- center - nsigmas * sqrt(sigma2)
  object$x <- ewma$x
  y <- as.vector(ewma$y)
  names(y) <- c(names(object$statistics), names(object$newstats))
  object$y <- y
  object$sigma <- sqrt(sigma2)
  object$lambda <- lambda
  object$nsigmas <- nsigmas
  limits <- cbind(lcl, ucl)
  colnames(limits) <- c("LCL", "UCL")
  object$limits <- limits
  object$violations <- which(y < lcl | y > ucl)
  class(object) <- "ewma.qcc"
  if (plot) 
    plot(object, ...)
  return(object)
}
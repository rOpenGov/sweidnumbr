#' Generate a vector of random \code{pin}
#'
#' @description 
#' A function that generates random \code{pin}s (see \code{\link{as.pin}}). 
#' The generated \code{pin} is uniformely distributed over the time period.
#'
#' @param n number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' @param start_date Smallest possible \code{pin}. Default is 1900-01-01.
#' @param end_date Largest possible \code{pin}. Default is the current date.
#' @param p.male Proportion of males. Default is 0.5.
#' @param p.coordn Proportion of coordination numbers. Default is 0.1.
#' 
#' @return a vector of generated \code{pin}s.
#' 
#' @examples 
#' x <- rpin(3)
#' pin_ctrl(x)
#' pin_sex(x)
#' pin_age(x)
#' 
#' @export
rpin <- function(n, start_date = "1900-01-01", end_date = Sys.Date(), p.male = 0.1, p.coordn = 0.1){
  if(length(n) > 1) n <- length(n)
  sd <- lubridate::ymd(start_date)
  ed <- lubridate::ymd(end_date)
  days <- as.numeric(ed - sd) + 1
  rd <- sd + lubridate::days(floor(stats::runif(n) * days))
  coordn <- sample(c(0, 60), size = n, replace = TRUE, prob = c(1-p.coordn,p.coordn))
  
  dd <- lubridate::day(rd) + coordn
  dd <- formatC(dd, width = 2, format = "d", flag = "0")
  yyyymmdd <- paste0(format(rd, "%Y%m"), dd)
  
  xx <- floor(stats::runif(n)*100)
  xx <- formatC(xx, width = 2, format = "d", flag = "0")
  x <- sample(x = as.character(c(0,2,4,6,8,1,3,5,7,9)), prob = c(rep(1-p.male,5), rep(p.male,5)), replace = TRUE, size = n)
  yyyymmddxxx <- paste0(yyyymmdd, xx, x)
  c <- as.character(luhn_algo(yyyymmddxxx, c(2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)))
  as.pin(paste0(yyyymmddxxx,c))
}
  
#' Generate a vector of random \code{oin}
#'
#' @description 
#' A function that generates random \code{oin}s (see \code{\link{as.pin}}).
#' The generated \code{oin} is uniformely distributed over all possible \code{oin}s.
#'
#' @param n number of observations. If \code{length(n) > 1}, the length is taken to be the number required.
#' 
#' @return a vector of generated \code{oin}s.
#' 
#' @examples 
#' x <- roin(3)
#' oin_ctrl(x)
#' oin_group(x)
#' 
#' @export
roin <- function(n){
  if(length(n) > 1) n <- length(n)
  g <- sample(x = c("1", "2", "3", "5", "6", "7", "8", "9"), replace = TRUE, size = n)
  x <- sample(x = as.character(c(0:9)), replace = TRUE, size = n)
  xxxx <- floor(stats::runif(n)*8000) + 2000
  xxx <- floor(stats::runif(n)*1000)
  xxx <- formatC(xxx, width = 3, format = "d", flag = "0")

  gxxxxx <- paste0(g,x,xxxx)
  c <- as.character(luhn_algo(paste0(gxxxxx, xxx), c(2, 1, 2, 1, 2, 1, 2, 1, 2)))
  as.oin(paste(gxxxxx,"-", xxx, c, sep=""))
}



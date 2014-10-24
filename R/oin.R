#' @title
#' Parse organizational identity numbers
#' 
#' @details
#' Check and convert a vector of organizational identity numbers. 
#' The following format is accepted: 
#' \itemize{
#'   \item character: \code{GNNNNN-NNNC}
#' }
#' 
#' @param oin Vector with swedish organizational identity numbers in character format. See details.
#' 
#' @references 
#' \href{http://www.skatteverket.se/download/18.70ac421612e2a997f85800040284/1359707510840/70909.pdf}{SKV 709}
#' 
#' @return
#' Character vector (of class \code{oin}) with swedish organizational identity numbers.
#'
#' @examples
#' ex_oin <- c("556000-4615", "232100-0156", "802002-4280", "8020024280", "AA2002-4280")
#' as.oin(ex_oin)
#' 
#' @export
#' 
as.oin <- function(oin){
  suppressWarnings(
    correct <- 
      is.character(oin) &
        !is.na(as.numeric(substr(oin,1,6))) & 
        grepl(pattern = "-", substr(oin,7,7)) &
        !is.na(as.numeric(substr(oin,8,11))) &
        as.numeric(substr(oin,3,3)) >= 2
  )
  newoin <- oin
  newoin[!correct] <- NA
  
  # Warning for incorrect pin
  isna <- is.na(newoin)
  if(any(isna)) {
    warning("The following personal identity numbers are incorrect: ", 
            paste(which(isna), collapse = ", "), 
            call. = FALSE)
  }
  
  # Add class
  class(newoin) <- c("oin", "character")
  
  return(newoin)
}



#' @title
#' Test if a character vector contains correct \code{oin}
#' 
#' @description
#' Test which elements in a text vector that contains organization identity 
#' number.
#' 
#' @param oin Character vector to be tested if it is an \code{oin} of the right format.
#' 
#' @return
#' Logical vector indicating if the elements can be an organization identity number.
#'
#' @examples
#' ex_oin <- c("556000-4615", "232100-0156", "802002-4280", "8020024280", "AA2002-4280")
#' is.oin(ex_oin)
#' 
#' @export
is.oin <- function(oin){
  "oin" %in% class(oin)
}

#' @title
#' Check the control numbers for \code{oin}
#' 
#' @description
#' Calculates the control number using the Luhn algorithm and compare it with the control number in the organization identity number (oin).
#' 
#' @param oin Vector with swedish organization identity numbers (oin) in \code{NNNNNN-NNNN} format.
#' 
#' @references 
#' \href{http://www.skatteverket.se/download/18.70ac421612e2a997f85800040284/1302507382017/70909.pdf}{SKV 709}
#' 
#' @return
#' Logical vector indicating if a oin is correct (\code{TRUE}) or not (\code{FALSE})
#'
#' @examples
#' ex_oin <- c("556000-4615", "232100-0156", "802002-4280", "232100-0157", "802002-4281")
#' oin_ctrl(ex_oin)
#' 
#' @export
oin_ctrl <- function(oin){
  if(!is.oin(oin)) oin <- as.oin(oin)
  
  oin_char <- paste0(substr(oin,1,6), substr(oin,8,11))
  res <- vapply(oin_char, luhn_algo, integer(1), USE.NAMES = FALSE, 
                multiplier = c(2, 1, 2, 1, 2, 1, 2, 1, 2, 0))
  as.integer(substr(oin, 11, 11)) == res
}

#' @title
#' Calculate organization group from \code{oin}
#' 
#' @description
#' Calculates the organization group from the organization number.
#' 
#' @inheritParams oin_ctrl
#' 
#' @references 
#' \href{http://www.skatteverket.se/download/18.70ac421612e2a997f85800040284/1302507382017/70909.pdf}{SKV 709}
#' 
#' @return
#' Factor with organization categories.
#' 
#' @examples
#' ex_oin <- c("556000-4615", "232100-0156", "802002-4280")
#' oin_group(ex_oin)
#'
#' @export
oin_group <- function(oin){
  if(!is.oin(oin)) oin <- as.oin(oin)
  
  as.factor(vapply(X = oin, 
                   FUN = oin_group_element, 
                   FUN.VALUE = character(1), 
                   USE.NAMES = FALSE))
}

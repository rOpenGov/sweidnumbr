#' @title
#' is.oin
#' 
#' @description
#' Test which elements in a text vector that contains organization identity 
#' number.
#' 
#' @param oin Character vector to be tested if it is an oin of the right format.
#' 
#' @return
#' Logical vector indicating if the elements can be an organisation identity number.
#'
#' @export
is.oin <- function(oin){
  suppressWarnings(
  is.character(oin) &
    !is.na(as.numeric(substr(oin,1,6))) & 
    grepl(pattern = "[-–]", substr(oin,7,7)) &
    !is.na(as.numeric(substr(oin,8,11))) &
    as.numeric(substr(oin,3,3)) >= 2
  )
}

#' @title
#' oin_ctrl
#' 
#' @description
#' Calculates the control number and compare it with the control number in the organization identity number (oin).
#' 
#' @param oin Vector with swedish organisation identity numbers.
#' 
#' @references 
#' http://www.skatteverket.se/download/18.70ac421612e2a997f85800040284/1302507382017/70909.pdf
#' 
#' @return
#' Logical vector indicating if a oin is correct (TRUE) or not (FALSE)
#'
#' @export
oin_ctrl <- function(oin){

  oin_char <- paste(substr(oin,1,6), substr(oin,8,11), sep="")
  res <- vapply(oin_char, luhn_algo, integer(1), USE.NAMES = FALSE, 
                multiplier = c(2, 1, 2, 1, 2, 1, 2, 1, 2, 0))
  as.integer(substr(oin, 11, 11)) == res
}

#' @title
#' oin_group
#' 
#' @description
#' Calculates the organization group from the organization number.
#' 
#' @param oin Character vector with organization identity number.
#' 
#' @references 
#' http://www.skatteverket.se/download/18.70ac421612e2a997f85800040284/1302507382017/70909.pdf
#' 
#' @return
#' Factor with organization categories.
#'
#' @export
oin_group <- function(oin){
  as.factor(vapply(X = oin, 
                   FUN = oin_group_element, 
                   FUN.VALUE = character(1), 
                   USE.NAMES = FALSE))
}

#' @title
#' pin_convert
#' 
#' @description
#' Converts one pin to standard format
#' 
#' @param pin A character element of length one.
#' @param format Which format should be converted. See \link{as.pin}.
#'  
#' @return
#' Character element on standard format.
#' 
#'

pin_convert <- function(pin, format){
  if(length(pin) == 0) return(pin)
  switch(EXPR = format,
         "1" = pin,
         "2" = gsub("-", "", pin),
         "3" = paste0(ifelse(substr(pin,start=7,7) == "-",
                             as.character(pin_century(pin)),
                             as.character(pin_century(pin)-1)), 
                      substr(pin, 1, 6),
                      substr(pin, 8, 11)),
         "4" = paste0(as.character(pin_century(pin)), pin))
}

#' @title
#' pin_century
#' 
#' @description
#' Assuming that the person is less than 100 years old, calculate the century of birth.
#' 
#' @param pin_short Vector with swedish personal identity numbers on standard format.
#' 
#' @return
#' Century vector in numeric format
#' 
pin_century <- function(pin_short){
  pin_date <- as.Date(paste(paste0(substr(Sys.Date(),1,2), substr(pin_short, 1,2)),
        substr(pin_short, 3,4), substr(pin_short, 5,6), sep="-"))
  ifelse(pin_date > Sys.Date(), 
         as.numeric(substr(Sys.Date(),1,2))-1,
         as.numeric(substr(Sys.Date(),1,2)))
}

#' @title
#' pin_coordn_correct
#' 
#' @description
#' Remove the change of day in coordination numbers (to enable age calculation).
#' 
#' @param pin Vector of pins at format atandard fromat 'YYYYMMDDNNNC'. See \link{as.pin}.
#' 
#' @return
#' Character vector with pin, corrected for coordination numbers.
#' 
pin_coordn_correct <- function(pin){
  coordn <- pin_coordn(pin)
  pin <- ifelse(coordn,
                paste0(
                  substr(pin,1,6), 
                  as.character(as.numeric(substr(pin,7,7)) - 6),
                  substr(pin, 8, 12)), pin)
  return(pin)
}



#' @title
#' pin_birthplace_internal
#' 
#' @description
#' Internal computation of birthplace (one for each pin)
#' 
#' @param pin Character element with pin at standard format 'YYYYMMDDNNNC'. See \link{as.pin}.
#' @param birth_vector Vector mapping birth number to birthplace. See \link{pin_birthplace}.
#' 
#' @return
#' Character element containing birthplace
pin_birthplace_internal <- function(pin, birth_vector){
  if(is.na(pin)) return(pin)
  born <- as.numeric(substr(pin, 1, 4))
  if(born >= 1990){
    res <- "Born after 31 december 1989"
  } else {
    birth_number <- as.numeric(substr(pin, 9, 10))
    if(born >= 1968 & birth_number <= 13) {
      res <- birth_vector[11]
    } else {
      res <- birth_vector[as.numeric(substr(pin, 9, 10)) + 1]
    }
  }
  return(res)
}



#' @title
#' luhn_algo
#' 
#' @description
#' Calculates the control number for a Swedish personal/organisational identity number using the Luhn algorithm.
#' 
#' @param id Element with swedish personal identity number.
#' @param multiplier What should each element in id be multiplied with
#' 
#' @references 
#' https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf
#' https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf
#' 
#' @return
#' The control number (last digit in the personal identification number) calculated from \code{id} (as integer).
#' 
#' @examples
#' luhn_algo("121212121212", c(0,0,2,1,2,1,2,1,2,1,2,0))
#' luhn_algo(  "121212121",  c(      2,1,2,1,2,1,2,1,2))

#' 
#' ## If no multiplier, the default is 
#' ## to find one that match the format of id
#' luhn_algo("121212121212")
#' luhn_algo("12121212121")
#' luhn_algo("1212121212")
#' luhn_algo("121212121")
#' 
#' ## Also for multiple pin 
#' ## (as long they are all of the same format)
#' luhn_algo(c("12121212121", "19850504333"))
#' \donttest{
#' luhn_algo(c("12121212121", "850504333")) ## Different formats should fail!
#' }
#' 
#' @export
luhn_algo <- function(id, multiplier){
  all_ids <- id
  id <- as.character(all_ids[!is.na(all_ids)])
  n <- as.character(stringr::str_length(id))
  
  if (!(all(n == 9) || all(n == 10) || all(n == 11) || all(n == 12))){
    stop("All elements of x must have the same length with 9-12 digits!")
  } 
  
  ## multiplier might be adopted to id if multiplier is missing
  if (missing(multiplier)){
    multiplier <- switch(n[1],
                         '12' = c(0, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0),
                         '11' = c(0, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2),
                         '10' = c(      2, 1, 2, 1, 2, 1, 2, 1, 2, 0),
                          '9' = c(      2, 1, 2, 1, 2, 1, 2, 1, 2),
                  )
    message("'multiplier' set to: c(", 
            paste(multiplier, collapse = ", "), ")"
    )
    
  ## If multiplier specified, it should match the length of id
  } else if (!is.atomic(multiplier)){
      stop("'multiplier' is not an atomic vector!")
  }else if (length(multiplier) != n[1]){
      stop("Number of digits in 'id', does not match the length of 'multiplier'!")
  }
  
    
  ## control number for one id and one multiplier
  luhn_algo1 <- function (id, multiplier) {
    ret <- as.numeric(unlist(strsplit(id, split="")))
    calc <- ret * multiplier
    sumValue <- sum(calc %% 10) + sum(calc %/% 10)
    output <- as.integer((10 - sumValue %% 10) %% 10)
    output
  }
  
  ## control number for all id:s
  output <- vapply(id, FUN = luhn_algo1, FUN.VALUE = integer(1), 
                   multiplier = multiplier, USE.NAMES = FALSE)  
  all_output <- rep(as.integer(NA), length(all_ids))
  all_output[!is.na(all_ids)] <- output
  all_output
}


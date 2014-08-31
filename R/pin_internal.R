#' @title
#' pin_convert
#' 
#' @description
#' Converts one pin to standard format
#' 
#' @param pin A character element of length one.
#'  
#' @return
#' Character element on standard format.
#'
pin_convert <- function(pin){
  switch(as.character(nchar(pin)),
         "13" = paste(as.character(substr(pin, 1, 8)), as.character(substr(pin, 10, 13)) ,sep=""),
         "11" = paste(ifelse(substr(pin,start=7,7) == "-",
                             as.character(pin_century(pin)),
                             as.character(pin_century(pin)-1)), 
                      substr(pin, 1, 6),
                      substr(pin, 8, 11), sep=""),
         "10" = paste(as.character(pin_century(pin)), pin ,sep=""),
         pin)
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
  pin_date <- as.Date(paste(paste(substr(Sys.Date(),1,2), substr(pin_short, 1,2), sep=""),
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
#' @param pin Vector of pins at format atandard fromat 'YYYYMMDDNNNC'. See \link{pin_format}.
#' 
#' @return
#' Character vector with pin, corrected for coordination numbers.
#' 
pin_coordn_correct <- function(pin){
  coordn <- pin_coordn(pin)
  pin <- ifelse(coordn,
                paste(
                  substr(pin,1,6), 
                  as.character(as.numeric(substr(pin,7,7)) - 6),
                  substr(pin, 8, 12), sep=""), pin)
  return(pin)
}



#' @title
#' pin_birthplace_internal
#' 
#' @description
#' Internal computation of birthplace (one for each pin)
#' 
#' @param pin Character element with pin at standard format 'YYYYMMDDNNNC'. See \link{pin_format}.
#' @param birth_vector Vector mapping birth number to birthplace. See \link{pin_birthplace}.
#' 
#' @return
#' Character element containing birthplace
pin_birthplace_internal <- function(pin, birth_vector){
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


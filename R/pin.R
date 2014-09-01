#' @title
#' pin_format
#' 
#' @description
#' Converts a number of different pin formats to standard (ABS) pin format 'YYYYMMDDNNNC'.
#' 
#' @details
#' The function converts different formats of swedish personal identity numbers to
#' the standard format. The formats that are converted are:
#' - numeric: YYYYMMDDNNNC
#' - numeric: YYMMDDNNNC
#' - character: "YYYYMMDDNNNC"
#' - character: "YYMMDD-NNNC"
#' - character: "YYYYMMDD-NNNC"
#' - character: "YYMMDDNNNC" (assuming < 100 years of age)
#' 
#' @param pin Vector with swedish personal identity numbers in character or numeric format. See details.
#' 
#' @references 
#' https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf
#' https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf
#' 
#' @return
#' Character vector with swedish personal identity numbers with standard format "YYYYMMDDNNNC".
#'
#' @export
pin_format <- function(pin){
  pin <- as.character(pin)
  
  if(any(nchar(pin) == 10)) message("Assumption: All are less than 100 years old.")

  # Convert
  pin <- vapply(X = pin, FUN = pin_convert, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  ispin <- is.pin(pin = pin)
  if(any(!ispin)) {
    pin[!ispin] <- NA
    warning("The following personal identity numbers are incorrect: ", 
            paste(which(!ispin), sep=", "), 
            call. = FALSE)
  }  
  return(pin)
}


#' @title
#' is.pin
#' 
#' @description
#' Test if a vector is of class "pin".
#' 
#' @param pin R object to be tested if it is a pin of the right format.
#' 
#' @return
#' Logical value indicating if the object is of class "pin"
#'
#' @export
is.pin <- function(pin){
  date <- as.Date(pin_coordn_correct(pin),"%Y%m%d")
  suppressWarnings(
  is.character(pin) &
    !is.na(as.numeric(pin)) & 
    nchar(pin) == 12 & 
    !is.na(date) &
    date <= Sys.Date()
  )
}

#' @title
#' pin_ctrl
#' 
#' @description
#' Calculates the control number and compare it with the control number in the personal identity number (pin).
#' 
#' @param pin Vector with swedish personal identity numbers (of class "pin").
#' 
#' @references 
#' https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf
#' https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf
#' 
#' @return
#' Logical vector indicating if a pin is correct (TRUE) or not (FALSE)
#'
#' @export
pin_ctrl <- function(pin){
  
  vapply(pin, pin_ctrl_internal, logical(1), USE.NAMES = FALSE)

}

#' @title
#' pin_sex
#' 
#' @description
#' Calculates the sex of from the personal identification number att the format given by \code{pin_format}.
#' 
#' @param pin Character vector with personal number with format 'YYYYMMDDNNNC'.
#' 
#' @references 
#' https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf
#' https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf
#' 
#' @return
#' Factor with label 'Male' and 'Female'.
#'
#' @export
pin_sex <- function(pin){
  stopifnot(is.pin(pin))
  female <- as.numeric(substr(pin,11,11)) %% 2 == 0
  output <- factor(ifelse(female, "Female", "Male"))
  return(output)
}


#' @title
#' pin_coordn
#' 
#' @description
#' Calculates if the personal identity number is a coordination number.
#' 
#' @param pin Character vector with personal number with format 'YYYYMMDDNNNC'. See \link{pin_format}.
#' 
#' @references 
#' https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf
#' https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf
#' http://www.skatteverket.se/download/18.3dfca4f410f4fc63c86800016383/70702svartvit.pdf
#' 
#' @return
#' Logical vector indicating if the pin is a coordination number (TRUE) or pin (FALSE).
#'
#' @export
pin_coordn <- function(pin) {
  as.numeric(substr(pin,7,8)) > 60
}


#' @title
#' pin_age
#' 
#' @description
#' Calculate the age in full years for a given date.
#' 
#' @param pin Character vector with personal number with format 'YYYYMMDDNNNC'. See \link{pin_format}.
#' @param date Date at which age is calculated.
#' 
#' @return
#' Age as an integer vector.
#'
#' @export
pin_age <- function(pin, date=Sys.Date()) {
  require(lubridate)
  date <- as.Date(date)
  pin <- pin_coordn_correct(pin)
  diff <- interval(ymd(paste(substr(pin,1,4), 
                             substr(pin,6,7), 
                             substr(pin,9,10), sep="-")),
                   ymd(date))
  message(paste("The age has been calculated at ", as.character(date), ".", sep=""))
  return(as.integer(diff %/% years(1)))
}


#' @title
#' pin_birthplace
#' 
#' @description
#' Calculate the birthplace for a given personal identity number.
#' 
#' @details
#' It is possible to calculate where people where born (and/or if a person has immigrated) 
#' through their personal identity number. This is possible for people that was born 
#' before 1990 and after 1945. 
#' 
#' For people born before 1946 the birthplace identifier contains information on where
#' one where registered the 1st of november 1946.
#' 
#' Personal identity numbers for people born after 1989 do not contain any information
#' on birthplace.
#' 
#' During the period 1946 - 1989 the pin also contains information on whether one has 
#' immigrated to Sweden during the period.
#' 
#' @param pin Character vector with personal number with format 'YYYYMMDDNNNC'. See \link{pin_format}.
#' 
#' @references
#' 1946 års folkbokföringsförordning (1946:469)]
#' 1967 års folkbokföringslag (1967:198)
#' 1991 års folkbokföringslag (1991:481)
#' 
#' @return
#' Age as an integer vector.
#'
#' @export
pin_birthplace <- function(pin){
  
  birth_vector <- 
    c(rep("Stockholm stad",10),
      rep("Stockholms län", 4),
      rep("Uppsala län", 2),
      rep("Södermanlands län", 3),
      rep("Östergötlands län", 5),
      rep("Jönköpings län", 3),
      rep("Kronobergs län", 2),
      rep("Kalmar län", 3),
      rep("Gotlands län", 1),
      rep("Blekinge län", 2),
      rep("Kristianstads län", 4),
      rep("Malmöhus län", 7),
      rep("Hallands län", 2),
      rep("Göteborgs och Bohus län", 7),
      rep("Älvsborgs län", 4),
      rep("Skaraborgs län", 3),
      rep("Värmlands län", 3),
      rep("Extra number", 1),
      rep("Örebro län", 3),
      rep("Västmanlands län", 2),
      rep("Kopparbergs län", 3),
      rep("Extra number", 1),
      rep("Gävleborgs län", 3),
      rep("Västernorrlands län", 4),
      rep("Jämtlands län", 3),
      rep("Västerbottens län", 4),
      rep("Norrbottens län", 4),
      rep("Extra number and immigrants (immigrated after 1946)", 7))
    
  res <- as.factor(vapply(X = pin, 
                          FUN = pin_birthplace_internal, 
                          FUN.VALUE = character(1), 
                          birth_vector = birth_vector, 
                          USE.NAMES = FALSE))
  
  return(res)

}


#' @title
#' Parse personal identity numbers to ABS format
#' 
#' @description
#' Converts personal identity numbers of different formats to standard (ABS) pin format \code{YYYYMMDDNNNC} 
#' where \code{YYYYMMDD} is the date of birth, \code{NNN} is the birth number and \code{C} is the
#' control number.
#' 
#' @details
#' The function converts different formats of swedish personal identity numbers to
#' the standard ABS format. The formats that can be converted are:
#' \itemize{
#'   \item numeric: \code{YYYYMMDDNNNC}
#'   \item numeric: \code{YYMMDDNNNC} (assuming < 100 years of age)
#'   \item character: \code{"YYYYMMDDNNNC"}
#'   \item character: \code{"YYMMDD-NNNC"}
#'   \item character: \code{"YYYYMMDD-NNNC"}
#'   \item character: \code{"YYMMDDNNNC"} (assuming < 100 years of age)
#' }
#' 
#' @param pin Vector with swedish personal identity numbers in character or numeric format. See details.
#' 
#' @references 
#' \href{https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf}{Population registration in Sweden}
#' \href{https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf}{SKV 704}
#' \href{http://www.riksdagen.se/sv/Dokument-Lagar/Utredningar/Statens-offentliga-utredningar/Personnummer-och-samordningsnu_GWB360/}{SOU 2008:60 : Personnummer och samordningsnummer}
#' 
#' @return
#' Character vector with swedish personal identity numbers with standard ABS format \code{"YYYYMMDDNNNC"}.
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
#' Test if a character vector contains correct  \code{pin}
#' 
#' @description
#' Test which elements of a character vector that contains correct personal 
#' identity numbers (regarding format).
#' To test the pin regarding the control number use \link{pin_ctrl}.
#' 
#' @param pin Character vector with swedish personal identity numbers with standard ABS format \code{"YYYYMMDDNNNC"}. See \link{pin_format}.
#' 
#' @return
#' Logical vector indicating if the elements can are of format personal identity number.
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
#' Check control number from \code{pin}
#' 
#' @description
#' Calculates the control number using the luhn algorithm and compare it with the 
#' control number in the personal identity number.
#' 
#' @inheritParams is.pin
#' 
#' @references 
#' \href{https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf}{Population registration in Sweden}
#' \href{https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf}{SKV 704}
#' \href{http://www.riksdagen.se/sv/Dokument-Lagar/Utredningar/Statens-offentliga-utredningar/Personnummer-och-samordningsnu_GWB360/}{SOU 2008:60 : Personnummer och samordningsnummer}
#' 
#' @return
#' Logical vector indicating if a pin is correct (\code{TRUE}) or not (\code{FALSE})
#'
#' @export
pin_ctrl <- function(pin){

  res <- vapply(pin, luhn_algo, integer(1), USE.NAMES = FALSE, 
                multiplier = c(0, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0))
  as.integer(substr(pin, 12, 12)) == res
}

#' @title
#' Calculate sex from \code{pin}
#' 
#' @description
#' Calculates the sex of from the personal identification number.
#' 
#' @inheritParams is.pin
#' 
#' @references 
#' \href{https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf}{Population registration in Sweden}
#' \href{https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf}{SKV 704}
#' \href{http://www.riksdagen.se/sv/Dokument-Lagar/Utredningar/Statens-offentliga-utredningar/Personnummer-och-samordningsnu_GWB360/}{SOU 2008:60 : Personnummer och samordningsnummer}
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
#' Check if \code{pin} is a coordination number.
#' 
#' @description
#' Calculate if the personal identity number is a coordination number.
#' 
#' @inheritParams is.pin
#' 
#' @references 
#' \href{https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf}{Population registration in Sweden}
#' \href{https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf}{SKV 704}
#' \href{http://www.riksdagen.se/sv/Dokument-Lagar/Utredningar/Statens-offentliga-utredningar/Personnummer-och-samordningsnu_GWB360/}{SOU 2008:60 : Personnummer och samordningsnummer}
#' 
#' @return
#' Logical vector indicating if the pin is a coordination number (\code{TRUE}) or pin (\code{FALSE}).
#'
#' @export
pin_coordn <- function(pin) {
  as.numeric(substr(pin,7,8)) > 60
}


#' @title
#' Calculate age of \code{pin} for a given date.
#' 
#' @description
#' Calculate the age in full years for a given date.
#' 
#' @inheritParams is.pin
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
                             substr(pin,5,6), 
                             substr(pin,7,8), sep="-")),
                   ymd(date))
  message(paste("The age has been calculated at ", as.character(date), ".", sep=""))
  return(as.integer(diff %/% years(1)))
}


#' @title
#' Calculate the birthplace of \code{pin}.
#' 
#' @description
#' Calculate the birthplace for a given personal identity number born before 1990. See details.
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
#' @inheritParams is.pin
#' 
#' @references
#' \href{http://www.riksdagen.se/sv/Dokument-Lagar/Utredningar/Statens-offentliga-utredningar/Personnummer-och-samordningsnu_GWB360/}{SOU 2008:60 : Personnummer och samordningsnummer}
#' 
#' @return
#' Birthplace as factor.
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


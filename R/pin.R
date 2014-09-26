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
#' @examples
#' # Examples taken from SKV 704 (see references)
#' ex_pin1 <- c("196408233234", "640823-3234", "19640823-3234")
#' as.pin(pin = ex_pin1)
#' ex_pin2 <- c("6408233234")
#' as.pin(ex_pin2)
#' ex_pin3 <- c(6408233234, 196408233234)
#' as.pin(ex_pin3)
#' 
#' @export
as.pin <- function(pin){
  pin_is_char <- is.character(pin)
  pin <- as.character(pin)
  if(!pin_is_char){
    pin <- vapply(pin, pin_add_zero, character(1), USE.NAMES = FALSE)
  }
  
  if(any(nchar(pin) == 10)) message("Assumption: All are less than 100 years old.")

  # Convert
  pin <- vapply(X = pin, FUN = pin_convert, FUN.VALUE = character(1), USE.NAMES = FALSE)
  
  isna <- is.na(pin)
  if(any(isna)) {
    warning("The following personal identity numbers are incorrect: ", 
            paste(which(isna), collapse = ", "), 
            call. = FALSE)
  }
  class(pin) <- c("pin", "character")
  return(pin)
}

#' @title
#' Test if a vector is of class \code{pin}
#' 
#' 
#' @param pin Character vector with swedish personal identity numbers with standard ABS format \code{"YYYYMMDDNNNC"}. See \link{pin_format}.
#' 
#' @return
#' Logical vector indicating if the elements can are of format personal identity number.
#'
#' @examples
#' ex_pin <- c("196408233234", "AA6408323234")
#' is.pin(ex_pin)
#'
#' @export
is.pin <- function(pin){
  "pin" %in% class(pin)
}

#' @title
#' Check control number from \code{pin}
#' 
#' @description
#' Calculates the control number using the Luhn algorithm and compare it with the 
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
#' @examples
#' # Examples taken from SKV 704 (see references)
#' ex_pin <- c("196408233234", "196408233235")
#' pin_ctrl(ex_pin)
#' 
#' @export
pin_ctrl <- function(pin){
  if(!is.pin(pin)) pin <- as.pin(pin)
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
#' 
#' @return
#' Factor with label 'Male' and 'Female'.
#' 
#' @examples
#' # Examples taken from SKV 704 (see references)
#' ex_pin <- c("196408233234", "186408233224")
#' pin_sex(ex_pin)
#'
#' @export
pin_sex <- function(pin){
  if(!is.pin(pin)) pin <- as.pin(pin)
  female <- as.numeric(substr(pin,11,11)) %% 2 == 0
  output <- factor(ifelse(female, "Female", "Male"))
  return(output)
}


#' @title
#' Check if \code{pin} is a coordination number
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
#' @examples
#' # Examples taken from SKV 704 (see references)
#' ex_pin <- c("196408233234", "196408833224")
#' pin_coordn(ex_pin)
#'
#' @export
pin_coordn <- function(pin) {
  if(!is.pin(pin)) pin <- as.pin(pin)
  as.numeric(substr(pin,7,8)) > 60
}


#' @title
#' Calculate age of \code{pin} for a given date
#' 
#' @description
#' Calculate the age in full years for a given date.
#' 
#' @inheritParams is.pin
#' @param date Date at which age is calculated.
#'
#' @references 
#' \href{https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf}{SKV 704}
#'   
#' @return
#' Age as an integer vector.
#'
#' @examples
#' # Example with someone born today
#' today_pin <- 
#'   paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),
#'         "0000",sep="")
#' pin_age(today_pin)
#' 
#' # Examples taken from SKV 704 (see references)
#' ex_pin <- c("196408233234", "186408833224")
#' pin_age(ex_pin, date = "2012-01-01")
#'
#' @export
pin_age <- function(pin, date=Sys.Date()) {
  if(!is.pin(pin)) pin <- as.pin(pin)
  date <- as.Date(date)
  pin <- pin_coordn_correct(pin)
  diff <- lubridate::interval(lubridate::ymd(paste(substr(pin,1,4), 
                             substr(pin,5,6), 
                             substr(pin,7,8), sep="-")),
                   lubridate::ymd(date))
  message(paste("The age has been calculated at ", as.character(date), ".", sep=""))
  return(as.integer(diff %/% lubridate::years(1)))
}


#' @title
#' Calculate the birthplace of \code{pin}
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
#' @examples
#' # Example with someone born today and from SKV 704 (see references)
#' today_pin <- 
#'   paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),
#'         "0000",sep="")
#' ex_pin <- c("196408233234", today_pin)
#' pin_birthplace(ex_pin)
#'
#' @export
pin_birthplace <- function(pin){
  if(!is.pin(pin)) pin <- as.pin(pin)
  birth_vector <- 
    c(rep("Stockholm stad",10),
      rep("Stockholms l\u00E4n", 4),
      rep("Uppsala l\u00E4n", 2),
      rep("S\u00F6dermanlands l\u00E4n", 3),
      rep("\u00F6sterg\u00F6tlands l\u00E4n", 5),
      rep("J\u00F6nk\u00F6pings l\u00E4n", 3),
      rep("Kronobergs l\u00E4n", 2),
      rep("Kalmar l\u00E4n", 3),
      rep("Gotlands l\u00E4n", 1),
      rep("Blekinge l\u00E4n", 2),
      rep("Kristianstads l\u00E4n", 4),
      rep("Malm\u00F6hus l\u00E4n", 7),
      rep("Hallands l\u00E4n", 2),
      rep("G\u00F6teborgs och Bohus l\u00E4n", 7),
      rep("\u00E4lvsborgs l\u00E4n", 4),
      rep("Skaraborgs l\u00E4n", 3),
      rep("V\u00E4rmlands l\u00E4n", 3),
      rep("Extra number", 1),
      rep("\u00F6rebro l\u00E4n", 3),
      rep("V\u00E4stmanlands l\u00E4n", 2),
      rep("Kopparbergs l\u00E4n", 3),
      rep("Extra number", 1),
      rep("G\u00E4vleborgs l\u00E4n", 3),
      rep("V\u00E4sternorrlands l\u00E4n", 4),
      rep("J\u00E4mtlands l\u00E4n", 3),
      rep("V\u00E4sterbottens l\u00E4n", 4),
      rep("Norrbottens l\u00E4n", 4),
      rep("Extra number and immigrants (immigrated after 1946)", 7))
    
  res <- as.factor(vapply(X = pin, 
                          FUN = pin_birthplace_internal, 
                          FUN.VALUE = character(1), 
                          birth_vector = birth_vector, 
                          USE.NAMES = FALSE))
  
  return(res)
}


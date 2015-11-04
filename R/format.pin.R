#' Formatting pin
#' 
#' Format \code{pin} for pretty printing
#'
#' @param x pin vector or am vector that can be coerced to such
#' @param format. character string specifying the output format. 
#' \code{\%N} is used as a reference for the last four digits of the pin.
#' Format of the date is handled via \code{\link{strptime}}.
#' (\code{"\%Y\%m\%d\%N"} by default). \code{\%P} is an available  
#' shorthand for \code{"(\%C) \%y-\%m-\%d - \%N"}, a format aimed for 
#' maximal readability when used in long lists
#' @param ... further arguments passed to \code{\link{format.Date}}
#'
#' @return character vector of same length as \code{x}
#' @export
#'
#' @examples
#' x <- as.pin(fake_pins$pin[1:10])
#' 
#' # Separate elements with hyphens:
#' format(x, "%Y-%m-%d-%N")
#' 
#' 
#' # Separate even further
#' format(x, "%C-%y-%m-%d-%N")
#'
#' # The special P-format for maximal readability
#' format(x, "%P") 
#' 
#' # A custom representation
#' format(x, "Borned %d of %B in %Y (a %A in week %U) with suffix no: %N")
#' 
#' # Extract only the year
#' format(x, "%Y")
format.pin <- function(x, format. = "%Y%m%d%N", ...) {
  if (format. == "%P") format. <- "(%C) %y-%m-%d - %N"
  gsub_v <- Vectorize(gsub, "replacement")
  f <- gsub_v("%N", substr(x, 9, 12), format.)
  mapply(format, pin_to_date(x), f)
}


#' Encode in a Common Format
#' 
#' This is essentialy just a wrapper to \code{\link[base]{format}}.
#' 
#' Objects of class \code{pin} inherits from class \code{AsIs}, which has its own 
#' \code{format} method. 
#' This function is just a wrapper to avoid calling \code{\link{format.AsIs}}.
#' 
#' @param x any R object (conceptually); typically numeric. If \code{x} is of class 
#'   \code{pin} method \code{\link{format.pin}} is used, otherwise
#'   \code{\link[base]{format}}
#' @param ... further arguments passed to or from other methods. 
#'   
#' @export
format <- function(x, ...) {
  if (is.pin(x)) format.pin(x, ...)
  else UseMethod("format")
}

#' @export
as.data.frame.pin <- function (x, ...) 
{
  nm <- deparse(substitute(x), width.cutoff = 500L)
  if (!"nm" %in% names(list(...))) 
    as.data.frame.vector(x, ..., nm = nm)
  else as.data.frame.vector(x, ...)
}

#' @export
print.pin <- function (x,...) 
{
  print(as.character(x),...)
 
  if (!is.null(attr(x, "non_personal")) && !attr(x, "non_personal")) {
    cat("Personal identity number(s)")
  } else{
    cat("Fake personal identity number(s) for tests or educational purposes.")
  }
  
}
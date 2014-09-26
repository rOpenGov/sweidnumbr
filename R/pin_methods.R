#' @export
as.data.frame.pin <- function (x, ...) 
{
  nm <- deparse(substitute(x), width.cutoff = 500L)
  if (!"nm" %in% names(list(...))) 
    as.data.frame.vector(x, ..., nm = nm)
  else as.data.frame.vector(x, ...)
}

#' @export
print.pin <- function (x) 
{
  print(as.character(x))
  cat("Personal identity number(s)")
}
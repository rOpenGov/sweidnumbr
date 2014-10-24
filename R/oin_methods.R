#' @export
as.data.frame.oin <- function (x, ...) 
{
  nm <- deparse(substitute(x), width.cutoff = 500L)
  if (!"nm" %in% names(list(...))) 
    as.data.frame.vector(x, ..., nm = nm)
  else as.data.frame.vector(x, ...)
}

#' @export
print.oin <- function (x,...) 
{
  print(as.character(x),...)
  cat("Organizational identity number(s)")
}
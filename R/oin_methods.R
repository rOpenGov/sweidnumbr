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

#' @export
`[.oin` <- create_s3_method("[")
#' @export
rep.oin <- create_s3_method("rep")

#' @export
`[<-.oin` <- function(x, ..., value){
  value <- as.oin(value)
  NextMethod()
}

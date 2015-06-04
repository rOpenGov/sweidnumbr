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
  cat("Personal identity number(s)")
}

# Helper function to create S3 method that preserves (most) 
# attributes (including class)
create_s3_method <- function(generic = NULL, object = NULL){
  function(x, i, ...) {
    r <- NextMethod(generic = generic, object = object)
    mostattributes(r) <- attributes(x)
    r
  }
}

#' @export
`[.pin` <- create_s3_method("[")
#' @export
rep.pin <- create_s3_method("rep")

#' @export
`[<-.pin` <- function(x, ..., value){
  value <- as.pin(value)
  NextMethod()
}


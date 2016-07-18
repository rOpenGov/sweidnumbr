#' @export
print.pin <- function (x,...) 
{
  print(as.character(x),...)
  cat("Personal identity number(s)")
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

#' @export
c.pin <- function(..., recursive = FALSE){
  args <- list(...)
  if (!length(args)) return(as.pin(character()))
  as.pin(unlist(lapply(args, as.character)))
} 

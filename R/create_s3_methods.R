# Helper function to create S3 method that preserves (most) 
# attributes (including class)
create_s3_method <- function(generic = NULL, object = NULL){
  function(x, i, ...) {
    r <- NextMethod(generic = generic, object = object)
    mostattributes(r) <- attributes(x)
    r
  }
}
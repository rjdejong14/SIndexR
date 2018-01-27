#' @title
#' Curve source
#' @description
#'  Returns string containing publication source.
#' @param cu_index Integer/Numeric, Curve index.
#' @return
#' String containing publication citation.
#'    If input parameter is not a valid curve index, the return is the
#'    null pointer.
#' @rdname SIndexR_CurveSource
SIndexR_CurveSource <- function(cu_index){
  if(!(class(cu_index) %in% c("numeric", "integer"))){
    stop("cu_index must be either numeric or integer.")
  } else if (class(cu_index) == "numeric") {
    if(!identical(round(cu_index), cu_index)){
      stop("cu_index must be either numeric or integer.")
    } else {
      cu_index <- as.integer(cu_index)
    }
  }
  return(unlist(lapply(cu_index, function(s) Sindex_CurveSource(s))))
}

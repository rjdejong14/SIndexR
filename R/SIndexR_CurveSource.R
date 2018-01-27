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
  cu_index <- wholeToInteger(cu_index, "cu_index")
  return(unlist(lapply(cu_index, function(s) Sindex_CurveSource(s))))
}

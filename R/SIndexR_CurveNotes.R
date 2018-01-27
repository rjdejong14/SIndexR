#' @title
#' Curve notes
#' @description
#'    Returns string containing notes on use.
#' @param cu_index Integer/Numeric, Curve index.
#' @return A string containing notes on use of curve.
#'    If input parameter is not a valid curve index, the return is the
#'    null pointer.
#' @rdname SIndexR_CurveNotes
SIndexR_CurveNotes <- function(cu_index){
  cu_index <- wholeToInteger(cu_index, "cu_index")
  return(unlist(lapply(cu_index, function(s) Sindex_CurveNotes(s))))
}

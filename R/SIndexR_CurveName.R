#' @title
#'    Curve name
#' @description
#'    Returns string containing author and date of curve.
#' @param cu_index Integer/Numeric, Curve index.
#' @return
#'    A string containing curve author and date.
#'    If input parameter is not a valid curve index, the return is the
#'    \code{null}.
#'
#' @note
#'    Curve name string examples: "Bruce (1981)", "Nigh (1998)".
#' @rdname SIndexR_CurveName
SIndexR_CurveName <- function(cu_index){
  cu_index <- wholeToInteger(cu_index, "cu_index")
  return(unlist(lapply(cu_index, function(s) Sindex_CurveName(s))))
}

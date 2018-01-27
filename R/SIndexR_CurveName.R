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
  if(!(class(cu_index) %in% c("numeric", "integer"))){
    stop("cu_index must be either numeric or integer.")
  } else if (class(cu_index) == "numeric") {
    if(!identical(round(cu_index), cu_index)){
      stop("cu_index must be either numeric or integer.")
    } else {
      cu_index <- as.integer(cu_index)
    }
  }
  return(unlist(lapply(cu_index, function(s) Sindex_CurveName(s))))
}

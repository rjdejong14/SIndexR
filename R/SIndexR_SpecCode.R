#' @title
#'    Returns string containing species code.
#' @description
#'    Returns string containing species code.
#' @param sp_index Integer, Specifies species index.
#' @return
#'    Pointer to string containing species code.
#'    If input parameter is not a valid species index, the return is the
#'    null pointer.
#' @note
#'    Species code string takes the form "Xx" or "Xxx", such as "Sw" or "Fdc".
#' @rdname SIndexR_SpecCode
SIndexR_SpecCode <- function(sp_index){
  sp_index <- wholeToInteger(sp_index, "sp_index")
  return(unlist(lapply(sp_index, function(s) Sindex_SpecCode(s))))
}

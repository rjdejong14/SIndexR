#' @title
#' Returns next defined curve index for a species.
#' @description
#' Returns next defined curve index for a species.
#' @param sp_index Integer/Numeric, Specifies species index.
#' @param cu_index Integer/Numeric, Specifies curve index.
#' @return
#'   Integer curve index, for use in other Sindex functions.
#'    May return an error code under the following conditions:
#'
#'    return value    condition
#'
#'    ------------    ---------
#'
#'    SI_ERR_SPEC     input species is not a valid species index.
#'
#'    SI_ERR_CURVE    input curve is not a valid curve index for this species.
#'
#'    SI_ERR_NO_ANS   input parameter is last defined index for this species.
#' @note
#'    No assumption should be made about the ordering of the curves.
#' @rdname SIndexR_NextCurve

SIndexR_NextCurve <- function(sp_index, cu_index){
  sp_index <- wholeToInteger(sp_index, "sp_index")
  cu_index <- wholeToInteger(cu_index, "cu_index")
  if(length(sp_index) == 1 & length(cu_index) != 1){
    sp_index <- rep(sp_index, length(cu_index))
  }
  if(length(sp_index) != 1 & length(cu_index) == 1){
    cu_index <- rep(cu_index, length(sp_index))
  }
  if(length(sp_index) != length(cu_index)){
    stop("sp_index and cu_index do not have same length.")
  }
  sp_index_list <- lapply(sp_index, function(s) s)
  cu_index_list <- lapply(cu_index, function(s) s)
  allinputs <- Map(list, sp_index_list, cu_index_list)
  return(unlist(lapply(allinputs, function(s) Sindex_NextCurve(sp_index = s[[1]],
                                                               cu_index = s[[2]]))))

}

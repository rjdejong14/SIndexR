#' @title
#'    Calculates the number of years a stand takes to grow from seed to
#'     breast height.
#' @description
#'    Calculates the number of years a stand takes to grow from seed to
#'     breast height.
#' @param curve, Integer/Numeric, The particular site index curve to project the height and age along.
#' @param siteIndex, Numeric, The site index value of the stand.
#' @return \code{output}: computed years to breast height.
#'         \code{error}: error information, i.e.,
#'             0, or an error code under the following conditions:
#'            return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index.
#'    SI_ERR_GI_TOT   if GI curve
#'    SI_ERR_LT13     if site index <= 1.3
#' @importFrom data.table data.table
#' @rdname SIndexR_Y2BH
SIndexR_Y2BH <- function(curve,
                         siteIndex){
  curve <- wholeToInteger(curve, "curve")
  inputdata <- data.table::data.table(curve, siteIndex)
  rm(curve, siteIndex)
  curve_list <- as.list(inputdata$curve)
  siteIndex_list <- as.list(inputdata$siteIndex)
  inputdata_list <- Map(list, curve_list, siteIndex_list)

  y2bh <- unlist(lapply(inputdata_list,
                        function(s) si_y2bh(cu_index = s[[1]],
                                            site_index = s[[2]])))

  error <- y2bh
  error[error > 0] <- 0
  return(list(output = y2bh,
              error = error))
}

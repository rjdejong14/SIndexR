#' @title 
#' Calculate a height based on an age, site index and site index curve.
#' @description  
#'    Converts an Age and Site Index to a Height for a particular Site Index
#'    Curve.
#' @param curve Integer, The particular site index curve to project the height and age along.
#' @param age Numeric, The age of the trees indicated by the curve selection.  The
#'                     interpretation of this age is modified by the 'ageType' parameter.
#' @param ageType Integer, Age type. Must be one of:
#'                \code{SI_AT_TOTAL}, the age is the total age of the stand in years since
#'                planting, or \code{SI_AT_BREAST}, the age indicates the number of years since the stand
#'                reached breast height.
#' @param siteIndex Numeric, The site index value of the stand.
#' @param y2bh Numeric, Years to breast height.
#'                      The number of years it takes the stand to reach breast height.
#' @return \code{output} the computed height
#'         \code{error} 0, or an error code under the following conditions:
#'    
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index.
#'    SI_ERR_GI_MIN   if bhage < 0.5 (for GI)
#'    SI_ERR_GI_MAX   if bhage > GI range
#'    SI_ERR_NO_ANS   if computed age > 999
#'    SI_ERR_GI_TOT   if total age and GI curve
#'    SI_ERR_LT13     if site index <= 1.3
#' @export
#' @docType methods
#' @rdname Sindex_AgeSIToHt
#' 
setGeneric("Sindex_AgeSIToHt",
           function(curve,
                    age,
                    ageType,
                    siteIndex,
                    y2bh){standardGeneric("Sindex_AgeSIToHt")})
#' @rdname Sindex_AgeSIToHt
setMethod("Sindex_AgeSIToHt",
          signature = c(curve = "integer",
                        age = "numeric",
                        ageType = "integer",
                        siteIndex = "numeric",
                        y2bh = "numeric"),
          definition = function(curve,
                                age,
                                ageType,
                                siteIndex,
                                y2bh){
            
            height <- index_to_height (curve, age, ageType, siteIndex, y2bh, 0.5)
            error <- height
            error[error > 0] <- 0
            return(list(output = height,
                        error = error))
          })

#' @export
#' @rdname Sindex_AgeSIToHt
setMethod("Sindex_AgeSIToHt",
          signature = c(curve = "numeric",
                        age = "numeric",
                        ageType = "numeric",
                        siteIndex = "numeric",
                        y2bh = "numeric"),
          definition = function(curve,
                                age,
                                ageType,
                                siteIndex,
                                y2bh){
            
            return(Sindex_AgeSIToHt (curve = as.integer(curve),
                                     age, ageType = as.integer(ageType),
                                     siteIndex, y2bh))
          })

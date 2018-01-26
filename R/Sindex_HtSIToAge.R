#' @title
#' Calcuate age based on a height, site index for a site index curve 
#' @description 
#' Converts a Height and Site Index to an Age for a particular Site Index
#' Curve.
#' @param curve Integer, Defines curve index. 
#'                       The particular site index curve to project the height and age along.
#' @param height Numeric, Defines a tree height of the species in meters.
#' @param ageType Integer, Defines age type. Must be one of:
#'                        \code{SI_AT_TOTAL}, the age is the total age of the stand in years since
#'                        planting; or \code{SI_AT_BREAST}, the age indicates the number of years since the stand
#'                        reached breast height.
#' @param siteIndex Numeric, Defines site index of the stand.
#' @param y2bh Numeric, The number of years it takes the stand to reach breast height.
#' @return 
#'      \code{output} contains computed age; \code{error} contains error information.
#'      If an error condition occurs, the age is set to the
#'        same as the return value.
#'
#'  ------------
#'    0, or an error code under the following conditions:
#'    
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index.
#'    SI_ERR_GI_MIN   if bhage < 0.5 (for GI)
#'    SI_ERR_GI_MAX   if bhage > GI range
#'    SI_ERR_NO_ANS   if computed height > 999
#'    SI_ERR_GI_TOT   if total age and GI curve
#'    SI_ERR_LT13     if site index <= 1.3
#'  
#' @docType methods
#' @rdname Sindex_HtSIToAge
setGeneric("Sindex_HtSIToAge",
           function(curve,
                    height,
                    ageType,
                    siteIndex,
                    y2bh){standardGeneric("Sindex_HtSIToAge")})
#' @rdname Sindex_HtSIToAge
setMethod("Sindex_HtSIToAge",
          signature = c(curve = "integer",
                        height = "numeric",
                        ageType = "integer",
                        siteIndex = "numeric",
                        y2bh = "numeric"),
          definition = function(curve,
                                height,
                                ageType,
                                siteIndex,
                                y2bh){
            age <- index_to_age (curve, height, ageType, siteIndex, y2bh)
            error <- age
            error[age > 0] <- 0
            return(list(output = age,
                        error = error))          
          })

#' @export
#' @rdname Sindex_HtSIToAge
setMethod("Sindex_HtSIToAge",
          signature = c(curve = "numeric",
                        height = "numeric",
                        ageType = "numeric",
                        siteIndex = "numeric",
                        y2bh = "numeric"),
          definition = function(curve,
                                height,
                                ageType,
                                siteIndex,
                                y2bh){
            return(Sindex_HtSIToAge(curve = as.integer(curve),
                                    height = height,
                                    ageType = as.integer(ageType),
                                    siteIndex, y2bh))
          })

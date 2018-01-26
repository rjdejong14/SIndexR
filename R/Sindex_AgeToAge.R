#' @title 
#' Convert age to other type
#' @description 
#'    Age conversion between age types (total vs breast height)
#' @param cu_index Integer, Curve index.
#' @param age1 Numeric, Source age.
#' @param age_type1 Integer, Type of source age (SI_AT_BREAST or SI_AT_TOTAL).
#' @param y2bh Numeric, Years to breast height.
#' @param age_type2 Integer, Type of target age (SI_AT_BREAST or SI_AT_TOTAL).
#' 
#' @return 
#'  \code{output} contains computed targe age.
#'  \code{error} contains error information, i.e.,
#'      0, or an error code under the following conditions:
#'    
#'    return value    condition
#'    ------------    ---------
#'    SI_ERR_CURVE    input curve is not a valid curve index for this species.
#'    SI_ERR_AGE_TYPE unknown age type.
#'
#' @rdname Sindex_AgeToAge
Sindex_AgeToAge <- function(cu_index,
                            age1,
                            age_type1,
                            y2bh,
                            age_type2){
  age2 <- age_to_age (cu_index, age1, age_type1, age_type2, y2bh)
  error <- age2
  error[error > 0] <- 0
  return(list(output = age2,
              error = error))
}


wholeToInteger <- function(numbers, name) {
  if(class(numbers) == "numeric"){
    if(!identical(round(numbers), numbers)){
      stop(paste(name, " must be integer or whole number.", sep = ""))
    } else {
      return(as.integer(numbers))
    }
  } else if (class(numbers) == "integer"){
    return(numbers)
  } else {
      stop(paste(name, " must be integer or whole number.", sep = ""))
  }
}

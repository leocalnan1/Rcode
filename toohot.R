toohot <- function(temperature, isSummer) {
  if(isSummer == FALSE){
    if(temperature >= 60 & temperature <= 90){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  if(isSummer == TRUE){
    if(temperature >= 60 & temperature <= 100){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
}


toohot(50, TRUE)
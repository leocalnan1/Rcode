blackjack <- function(a,b) {
  if(a > 21 & b > 21) {
    print("0")
  }
  else if(a > b & a <= 21 | b > 21) {
    print(a)
  }
  else if(a < b & b <= 21 | a > 21) {
    print(b)
  }
}

blackjack(21, 22)
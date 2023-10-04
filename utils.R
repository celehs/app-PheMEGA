
sprint <- function(x) {
  ifelse(x > 10, sprintf("%.1f", x), 
         ifelse(x > 1, sprintf("%.2f", x),
                ifelse(x > .1, sprintf("%.2f", x), sprintf("%.2e", x))
         ))
}

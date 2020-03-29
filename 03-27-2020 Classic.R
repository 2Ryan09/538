total <- data.frame()

# Define the number of trials and max number of sided die
trials <- 10
maxNumSides <- 1000

# If the range of a vector = 0, all elements are the same
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}

# iterate up to the max sided die
for (diceSides in 1:maxNumSides) {
  history <- vector()
  
  # iterate for the number of trials specified
  for (i in 1:trials) {
    
    # inital roll is random numbers between 1 and number of sides, repeats allowed
    dice <- sample(1:diceSides, diceSides, replace = TRUE)
    rounds <- 0
    
    # whiel all the values rolled are not the same, roll again
    while (!zero_range(dice)) {
      
      # Use the previous roll to determine the next
      dice <- sample(dice, diceSides, replace = TRUE)
      
      # count the number of rolls
      rounds <- rounds + 1
    }
    
    # keep track the number of rounds required
    history <- append(history, rounds)
  }
  
  # collect average number rolls as a function of number of sides
  total <- rbind(total, data.frame(sides=diceSides, trials=mean(history)))
}

# Perform a linear fit, forcing the y-intercept through the origin
summary(lm(trials ~ 0 + sides, total))
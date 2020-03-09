# This R-file provides the solution to the challenge as provided by Optiver
# The challenge goes as follows:
#   Calvin has to cross several signals when he walks from his home to school. 
#   Each of these signals operate independently. They alternate every 80 seconds 
#   between green light and red light. At each signal, there is a counter display 
#   that tells him how long it will be before the current signal light changes. 
#   Calvin has a magic wand which lets him turn a signal from red to green 
#   instantaneously. However, this wand comes with limited battery life, so he can 
#   use it only for a specified number of times.

#   Can you write a script that takes as inputs the number of signals and the number 
#   of times Calvin can use his magic wand, and outputs the expected waiting time?

ExpectedWaitingTime <- function(signals_num = 1, wand_num = 0) {
  
  #### Function Description ####
  
  # Function that calculates the expected time Calvin has to wait 
  # at the traffic lights, given that he uses an optimal strategy.
  # Input parameters:
  #   signals_num = The number of signals Calvin encounters on his way home.
  #   wand_num    = The number of times Calvin can use his magic wand.
  # Output:
  #   The expected time Calvin has to wait.
  
  #### Erroneous input ####
  
  # Check for the number of signals
  if (signals_num < 0) {
    stop("The number of signals must be greater than or equal to 0!")
  }
  
  # Check for the number of times the wand can be used
  if (wand_num < 0) {
    stop("The number of times the wand can be used must be greater than or equal to 0!")
  }
  
  #### Some general parameters of the problem at hand ####
  
  # The number of seconds before a signal switches from red to green
  red_to_green <- 80
  
  # The number of seconds before a signal switches from green to red
  green_to_red <- 80

  # The probability of a signal displaying a green light at any given time
  green_prob <- green_to_red / (green_to_red + red_to_green)
  
  # The probability of a signal displaying a red light at any given time
  red_prob <- 1 - green_prob
  
  # Expected waiting time at any signal, without knowing the value of 
  # the counter, and without using the wand
  #   There is only waiting time when the light is red
  #   The waiting time given that the light is red, follows an unif(0, 80) distribution
  expected_waiting_time <- red_prob * 0.5 * red_to_green
  
  #### Calculate expected waiting time given the optimal strategy ####
  
  # Create internal function that will be used for the recursion
  # to avoid redundant computations of the code above
  CalculateEWT <- function(signals_num, wand_num) {
    
    # If the number of times the magic wand can be used, is at least the number of signals
    # then the expected waiting time equals 0
    if (wand_num >= signals_num) {
      return(0)
    }
    
    # If the magic wand can not be used anymore, the expected waiting time equals 
    # expected_waiting_time * signals_num
    if(wand_num == 0) {
      return(expected_waiting_time * signals_num)
    }
    
    # Calculate the expected waiting time at the remaining signals in two scenarios:
    #   1. When using the wand at the current signal. 
    #        Equal to CalculateEWT(signals_num = signals_num - 1, wand_num = wand_num - 1)
    #   2. When not using the wand at the current signal
    #        Equal to CalculateEWT(signals_num = signals_num - 1, wand_num = wand_num)
    ewt1 <- CalculateEWT(signals_num = signals_num - 1, wand_num = wand_num - 1)
    ewt2 <- CalculateEWT(signals_num = signals_num - 1, wand_num = wand_num)
    
    # The difference in expected waiting time between both scenarios
    difference <- ewt1 - ewt2
    
    # If the value at the counter is greater than (or equal to) the difference, then
    # it is beneficial to use the magic wand. The probability of using the wand, given this strategy
    # equals: red_prob * (1 - difference / red_to_green)
    use_wand_prob <- red_prob * (1 - difference / red_to_green)
    
    # The expected waiting time then consists of three components:
    #   1. The signal is green, the wand is not used.
    #   2. The signal is red:
    #        a. The counter is less than the difference, the wand is not used.
    #        b. The counter is greater than or equal to the difference, the wand is used.
    # The components of the expected waiting time are as follows:
    component_1 <- green_prob * ewt2
    component_2a <- (1 - green_prob - use_wand_prob) * (0.5 * difference + ewt2)
    component_2b <- use_wand_prob * ewt1
  
    # Return the expected waiting time as the sum of the components
    result <- component_1 + component_2a + component_2b
    return(result)
  }
  
  # Return the expected waiting time
  result <- CalculateEWT(signals_num = signals_num, wand_num = wand_num)
  return(result)
}
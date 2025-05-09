# this is a first attempt to use likelihood for fitting a step-based model of
# explicit adaptation

explicit_step_likelihood <- function(par, data) {
  
  # for fitting,  should have a few trials before a rotation starts
  # and a few more after the rotation starts
  # this is to ensure that the model can be fit to the data
  # trial 0 is the first trial with the rotation,
  # trial 1 the first trials where people conceivably have strategy
  
  # par should be a named vector with the following elements:
  # 'shape' a shape parameter for a gamma distribution
  # 'rate' a rate parameter for a gamma distribution
  # 'mean2' the mean of the step sizes away from zero
  # 'sd2' the sd of the step sizes away from zero
  # 'sd1' the sd of the strategies around zero
  # 'prop2' the proportion of participants who do not remain at 0 strategy
  
  # if we get tri-modal distributions at higher rotations,
  # we'll need to add a third step (mean3, sd3, shape2, rate2, prop3)
  
  # maybe the parameters can be a matrix?
  # columns: sd, mean, shape, rate, prop
  # rows: step 0 and 1
  # mean 0 has to 0, other means get progressively higher
  # sd's get higher,
  # products of shape and rate should get higher
  # sum of props should be 1
  
  # we can set fixed parameters using an optional parameter:
  # https://stackoverflow.com/questions/24209922/passing-fixed-and-variable-parameters-to-optimx#24210704
  
  # data should be a data frame with the following columns:
  # 'ID' for participants
  # 'trial' for the trial number (0 is the first trial with the rotation)
  # 'rotation' for the rotation angle
  # 'strategy' for the strategy angle
  
  
  
    
  
}
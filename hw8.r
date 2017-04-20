xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
    rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  return(as.vector(tapply(y, x, function(z) sample(z, length(z), replace=T))))
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  y <- NULL
  y <- fit + sample(err, length(fit), replace = F)
  return(y)
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if(degree == 1) {
    coeff <- as.vector(coef(lm(y ~ x)))
  }
  else {
    coeff <- as.vector(coef(lm(y ~ x + I(x^2))))
  }
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  if(is.null(fit)) {
    Y <- unlist(genBootY(data[,1], data[,2]))
  }
  else {
    Y <- unlist(genBootR(fit[,1], fit[,2]))
  }
  ### Use fitModel to fit a model to this bootstrap Y 
  fitModel(data[,1], Y, degree)
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic
  line <- lm(y ~ x, data = data)
  line.data <- cbind(line$fit, data$y - line$fit)
  quad <- lm(y ~ x + I(x^2), data = data)
  quad.data <- cbind(quad$fit, data$y - quad$fit)
  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  for(i in 1:B) {
    {
    line.nofit <- oneBoot(data, fit = NULL, degree = 1)
    line.fit <- oneBoot(data, fit = line.data, degree = 1)
    quad.nofit <- oneBoot(data, fit = NULL, degree = 2)
    quad.fit <- oneBoot(data, fit = quad.data, degree = 2)
    }
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  m1 <- matrix(line.nofit, nrow=2, ncol=B)
  m2 <- matrix(line.fit, nrow=2, ncol=B)
  m3 <- matrix(quad.nofit, nrow=3, ncol=B)
  m4 <- matrix(quad.fit, nrow=3, ncol=B)
  coeff <- list(m1, m2, m3, m4)
  }
  return(coeff)
}

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data
  plot(y ~ x)
  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  for(i in 1:4) {
    if (nrow(coeff[[i]]) == 2) {
      mapply(function(a, b) abline(a, b, col = rgb(1, 0, 0, alpha = 0.5)), coeff[[i]][1,], coeff[[i]][2,])
    } else {
      mapply(function(a, b, c) curve(a + b*x + c*x^2, add=TRUE,  col = rgb(1, 0, 0, alpha = 0.5)), coeff[[i]][1,], coeff[[i]][2,], coeff[[i]][3,])
    }
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  }
  curve(trueCoeff[1] + trueCoeff[2]*x + trueCoeff[3]*x^2, add=TRUE, col='green', lwd=3)
}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
    bootPlot(myData$x, myData$y, 
             coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}

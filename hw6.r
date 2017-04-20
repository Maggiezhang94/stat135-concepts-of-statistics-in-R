# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
  has_adopted <- matrix(rep(initial.doctors,n.days),nrow=n.doctors,ncol=n.days)
  for (i in 1:n.days){
    x <- sample(1:n.doctors, size=2)
    if (has_adopted[x[1], i] != has_adopted[x[2],i]) {
    y <- x[has_adopted[x, i] == 0]
    has_adopted[y, i:n.days] <- sample(c(0,1),1,prob=c(1-p, p))
    }
  }
return(has_adopted) 
}

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)

  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output

# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

set.seed(42)
n.doctors <- 100
initial.doctors <- sample(0:1, size=n.doctors, prob=c(0.9,0.1), replace=T)
n.days <- 100

p=0.9
has_adopted1 <- sim.doctors(initial.doctors, n.doctors, n.days, p)
plot(1:n.days, colSums(has_adopted1), xlab="Days", ylab="Number of Doctors Adopted the Drug", type='l', col="red")

p=0.7
has_adopted2 <- sim.doctors(initial.doctors, n.doctors, n.days, p)
lines(1:n.days, y=colSums(has_adopted2), col="orange")

p=0.5
has_adopted3 <- sim.doctors(initial.doctors, n.doctors, n.days, p)
lines(1:n.days, y=colSums(has_adopted3), col="yellow")

p=0.3
has_adopted4 <- sim.doctors(initial.doctors, n.doctors, n.days, p)
lines(1:n.days, y=colSums(has_adopted4), col="green")

p=0.1
has_adopted5 <- sim.doctors(initial.doctors, n.doctors, n.days, p)
lines(1:n.days, y=colSums(has_adopted5), col="blue")

legend("topright", legend=c("p=0.1","p=0.3","p=0.5","p=0.7","p=0.9"), 
       fill=c("red","orange","yellow","green","blue"))

# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)


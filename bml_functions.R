#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  # initialize a lattice with uniform distributed numbers
  m=runif(r*c)
  # determine which site is 0(empty), 1(red) and 2(blue)
  # Initially, cars are distributed at random: each intersection is independently 
  # assigned a car with probability p, or an empty space, i.e., 0 with probability 1 - p. 
  # Each car is independently equally likely to be blue or red.
  empty=which(m<=(1-p))
  red=which(m>(1-p) & m<(1-p/2))
  m=rep(2,r*c)
  m[empty]=0
  m[red]=1
  m=array(m,c(r,c))
  return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.step <- function(m){
  r=dim(m)[1]
  c=dim(m)[2]
  blue=which(m==2,arr.ind=T)
  red=which(m==1,arr.ind=T)
  
  # blue cars move to the north
  # the sites above the blue
  blue.north=blue
  blue.north[,1]=blue[,1]-1
  blue.north[blue.north[,1]==0,1]=r
  
  blue.move=(m[blue.north]==0)
  m[blue.north[blue.move,,drop=FALSE]]=2 # move up?
  m[blue[blue.move,,drop=FALSE]]=0 # make the empty?
  
  # red cars move to the east
  #browser()
  red.east=red
  red.east[,2]=red[,2]+1
  red.east[red.east[,2]==(c+1),2]=1
  
  red.move=(m[red.east]==0)
  m[red.east[red.move,,drop=FALSE]]=1 # move to right
  m[red[red.move,,drop=FALSE]]=0 # make the empty
  
  
  grid.new=(sum(blue.move,red.move)>0)
  return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  # initilize
  m=bml.init(r,c,p)
  flag=1
  step=0
  quartz()
  image(rotate(m),col=c('white','red','blue'))
  while(flag & step <= 2000){
    step = step+1
    m.sim=bml.step(m)
    m=m.sim[[1]]
    flag=m.sim[[2]]
    image(rotate(m),col=c('white','red','blue'))
  }
  return (step)
}

# rotate the image for display
rotate <- function(x) t(apply(x, 2, rev))

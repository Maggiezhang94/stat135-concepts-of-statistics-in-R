#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.
source('bml_functions.R')

step_1 = matrix(1:16, nrow=4, byrow=T, 
                dimnames=list(c("20*20","40*40","60*60","80*80"),
                              c("0.2","0.4","0.6","0.8")))
for (i in 1:4) {
  for (j in 1:4){
    step_1[i,j]<-bml.sim(20*i,20*i,0.2*j)
  }
}
print(step_1)

step_2 = matrix(1:16, nrow=4, byrow=T, 
                dimnames=list(c("20*20","40*40","60*60","80*80"),
                              c("0.2","0.4","0.6","0.8")))
for (i in 1:4) {
  for (j in 1:4){
    step_2[i,j]<-bml.sim(20*i,20*i,0.2*j)
  }
}
print(step_2)

step_3 = matrix(1:16, nrow=4, byrow=T, 
                dimnames=list(c("20*20","40*40","60*60","80*80"),
                              c("0.2","0.4","0.6","0.8")))
for (i in 1:4) {
  for (j in 1:4){
    step_3[i,j]<-bml.sim(20*i,20*i,0.2*j)
  }
}
print(step_3)

step_4 = matrix(1:16, nrow=4, byrow=T, 
                dimnames=list(c("20*20","40*40","60*60","80*80"),
                              c("0.2","0.4","0.6","0.8")))
for (i in 1:4) {
  for (j in 1:4){
    step_4[i,j]<-bml.sim(20*i,20*i,0.2*j)
  }
}
print(step_4)

save(step_1,step_2,step_3,step_4,file="steps")
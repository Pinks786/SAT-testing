#The SAT is a standardized college admissions test used in the United States. 
#determine some probabilities of what happens when a student guessed for all of their answers on the SAT.
#An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.

# probability of guessing correctly for one question.
1/5

#expected value of points for guessing on one question
expected_value_one<-1*0.2+(-0.25*0.8)
expected_value_one

#expected score of guessing on all 44 questions
n<- 44
expected_value<- n*expected_value_one
expected_value


#standard error of guessing on all 44 questions
se<-sqrt(n)*abs(1-(-0.25))*sqrt(0.2*0.8)
se

#(Central Limit Theorem) determine the probability that a guessing student scores 8 points or higher on the test.
n<- 44
prob_scores_8_or_higher<- 1-pnorm(8,expected_value,se)
prob_scores_8_or_higher

set.seed(21,sample.kind="Rounding")

# probability that a guessing student scores 8 points or higher
#Monte Carlo Simulation
B<- 10000
S<- replicate(B,{
  X<- sample(c(1,-0.25),n,replace=TRUE,prob=c(1/5,4/5))
  sum(X)
})
mean(S)
sd(S)
mean(S>=8)

#The SAT was recently changed to reduce the number of multiple choice options from 5 to 4 and also to eliminate the penalty for guessing.
#Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.
#What is the expected value of the score when guessing on this new test?
X<- sample(c(1,0),n,replace=TRUE,prob=c(0.25,0.75))
 exp_value<- n*(0.25)
 exp_value
 std_error<- sqrt(n)*abs(0--0.25)*sqrt(0.25*0.75)
 std_error
 
 #Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills. 
#What is the lowest p such that the probability of scoring over 35 exceeds 80%?

 p<- seq(0.25,0.95,0.05)
 n<-44
  prob_scores_35_or_higher<-function(s){
   exp_val<- n*(1*s+0*(1-s))
   std_err<- sqrt(n)*abs(1-0)*sqrt(s*(1-s))
   1-pnorm(35, exp_val,std_err)
 }
 scores_35_or_higher<-sapply(p,prob_scores_35_or_higher)
min(p[which(scores_35_or_higher>0.8)])








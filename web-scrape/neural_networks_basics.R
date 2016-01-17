# libraries
library("dplyr")
library("stringr")
library("ggplot2")

log_data = read.csv("../Data/Interesting/python/logistic_regression_scratch.csv")
log_data_cleaned = log_data %>% setNames(c("grade1", "grade2", "label")) %>% 
  mutate(label = label %>% str_replace_all(";", "") %>% as.integer)

str(log_data_cleaned)

sigmoid <- . %>%  1 / (1.0 + exp(-1.0 * .))

hypothesis <- function(theta, x) sum(x * theta) %>% sigmoid
  
cost_function <- function(X, Y, theta, m){
  
  sumOfErrors = 0
  
  for (i in length(m)){
    xi = X[i]
    hi = Hypothesis(theta,xi)
    if (Y[i] == 1) error = Y[i] * math.log(hi)
    else if (Y[i] == 0) error = (1-Y[i]) * math.log(1-hi)
    sumOfErrors = sumOfErrors + error
  }
  
    
  const = -1/m
  J = const * sumOfErrors
  message('cost is ', J)
  return(J)
}
  
  
def Cost_Function_Derivative(X,Y,theta,j,m,alpha):
  sumErrors = 0
for i in xrange(m):
  xi = X[i]
xij = xi[j]
hi = Hypothesis(theta,X[i])
error = (hi - Y[i])*xij
sumErrors += error
m = len(Y)
constant = float(alpha)/float(m)
J = constant * sumErrors
return J

def Gradient_Descent(X,Y,theta,m,alpha):
  new_theta = []
constant = alpha/m
for j in xrange(len(theta)):
  CFDerivative = Cost_Function_Derivative(X,Y,theta,j,m,alpha)
new_theta_value = theta[j] - CFDerivative
new_theta.append(new_theta_value)
return new_theta

def Logistic_Regression(X,Y,alpha,theta,num_iters):
  m = len(Y)
for x in xrange(num_iters):
  new_theta = Gradient_Descent(X,Y,theta,m,alpha)
theta = new_theta
if x % 100 == 0:
  Cost_Function(X,Y,theta,m)
print 'theta ', theta
print 'cost is ', Cost_Function(X,Y,theta,m)
Declare_Winner(theta)

def Declare_Winner(theta):
  score = 0
winner = ""
scikit_score = clf.score(X_test,Y_test)
length = len(X_test)
for i in xrange(length):
  prediction = round(Hypothesis(X_test[i],theta))
answer = Y_test[i]
if prediction == answer:
  score += 1
my_score = float(score) / float(length)
if my_score > scikit_score:
		print 'You won!'
	elif my_score == scikit_score:
		print 'Its a tie!'
	else:
		print 'Scikit won.. :('
	print 'Your score: ', my_score
	print 'Scikits score: ', scikit_score

# setting variables
initial_theta = [0,0]
alpha = 0.1
iterations = 1000
Logistic_Regression(X,Y,alpha,initial_theta,iterations)

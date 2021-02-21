#install.packages("knitr")  #probably already installed
#install.packages("rmarkdown")  #probably already installed
#install.packages("ggplot2")  #plotting with ggplot
#install.packages("ggfortify")
#install.packages("MASS")
#install.packages("class")
#install.packages("pROC")
#install.packages("plotROC")
#install.packages("caret")

library(ggplot2)
library(caret)
library(MASS)


# Problem 1.

id <- "1X_8OKcoYbng1XvYFDirxjEWr7LtpNr1m"  # google file ID
values <- dget(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
X = values$X
dim(X)
x0 = values$x0
dim(x0)
beta = values$beta
dim(beta)
sigma = values$sigma
sigma

# D)
# TODO: Fill in value (squared bias) and comment on what we see.
bias = function(lambda, X, x0, beta) {
  p = ncol(X)
  value = (t(x0) %*% solve((t(X) %*% X + lambda * diag(p))) %*% t(X) %*% X %*% beta - t(x0) %*% beta ) ^ 2
  return(value)
}
lambdas = seq(0, 2, length.out = 500)
BIAS = rep(NA, length(lambdas))
for (i in 1:length(lambdas)) BIAS[i] = bias(lambdas[i], X, x0, beta)
dfBias = data.frame(lambdas = lambdas, bias = BIAS)
ggplot(dfBias, aes(x = lambdas, y = bias)) + geom_line(color = "red") + xlab(expression(lambda)) +
  ylab(expression(bias^2))

# E)
# TODO: Fill in value and comment on what we see.
variance = function(lambda, X, x0, sigma) {
  p = ncol(X)
  inv = solve(t(X) %*% X + lambda * diag(p))
  value = sigma * sigma * t(x0) %*% solve(t(X) %*% X + lambda * diag(p)) %*% t(X) %*% X %*% solve(t(X) %*% X + lambda * diag(p)) %*% x0
  return(value)
}
lambdas = seq(0, 2, length.out = 500)
VAR = rep(NA, length(lambdas))
for (i in 1:length(lambdas)) VAR[i] = variance(lambdas[i], X, x0, sigma)
dfVar = data.frame(lambdas = lambdas, var = VAR)
ggplot(dfVar, aes(x = lambdas, y = var)) + geom_line(color = "green4") + xlab(expression(lambda)) +
  ylab("variance")

# F)
# TODO: Fill in exp_mse and find value of lambda that minimizes mse.
# TODO: Comment on what we see.
# TODO: When generating the pdf, change to eval=True

exp_mse = BIAS + VAR + sigma * sigma
lambdas[which.min(exp_mse)]

dfAll = data.frame(lambda = lambdas, bias = BIAS, var = VAR, exp_mse = exp_mse)
ggplot(dfAll) + geom_line(aes(x = lambda, y = exp_mse), color = "blue") +
  geom_line(aes(x = lambda, y = bias), color = "red") +
  geom_line(aes(x = lambda, y = var), color = "green4") +
  xlab(expression(lambda)) +
  ylab(expression(E(MSE))
)

# Problem 3.

# read file
id <- "1i1cQPeoLLC_FyAH0nnqCnnrSBpn05_hO"  # google file ID
diab <- dget(sprintf("https://docs.google.com/uc?id=%s&export=download", id))

t = MASS::Pima.tr2
train = diab$ctrain
test = diab$ctest

# A)

logReg = glm(diabetes ~ ., data = train, family = "binomial")
summary(logReg)

# i)
# TODO: Show that...

# ii)

create_confusion_matrix <- function(pred, target) {
  confMat <- table(pred, test$diabetes)
  colnames(confMat) <- c("PRED FALSE", "PRED TRUE")
  row.names(confMat) <- c("TARGET FALSE", "TARGET TRUE")
  return(confMat)
}

cutOff = 0.5
pred <- predict(logReg, newdata = test[-1], type="response")
#confMat <- table(pred>cutOff, test$diabetes)
#colnames(confMat) <- c("PRED FALSE", "PRED TRUE")
#row.names(confMat) <- c("TARGET FALSE", "TARGET TRUE")
conf_mat = create_confusion_matrix(pred>cutOff, test$diabetes)
conf_mat

print("The sensitivity is:")
conf_mat[2,2]/(conf_mat[2,1]+conf_mat[2,2])
print("The specificity is:")
conf_mat[1,1]/(conf_mat[1,2]+conf_mat[1,1])



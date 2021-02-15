#install.packages("knitr")  #probably already installed
#install.packages("rmarkdown")  #probably already installed
#install.packages("ggplot2")  #plotting with ggplot
#install.packages("ggfortify")
#install.packages("MASS")
#install.packages("class")
#install.packages("pROC")
#install.packages("plotROC")
#install.packages("caret")
#install.packages("dplyr")

library(ggplot2)
library(caret)
library(MASS)
library(ElemStatLearn)
library(knitr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(GGally)
library(MASS)
library(boot) 


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
  confMat <- table(pred, target)
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


# B)

# i)
# TODO: Explain what the coefficients are.

# ii)
# TODO: Fit LDA
lda_model <- lda(diabetes ~ ., data=train)
pred <- predict(lda_model, newdata=test[-1])
conf_mat = create_confusion_matrix(unlist(pred[1]), test$diabetes)
conf_mat

# With cutOff = 0.5: pred[1]
# Posterior probabilities: pred[2]

# TODO: Fit QDA
qda_model <- qda(diabetes ~ ., data=train)
pred <- predict(qda_model, newdata=test[-1])
conf_mat = create_confusion_matrix(unlist(pred[1]), test$diabetes)
conf_mat

# TODO: Explain difference between the models.
# TODO: Create confusion tables.

# Problem 2

# read file
id <- "1yYlEl5gYY3BEtJ4d7KWaFGIOEweJIn__" # google file ID
d.corona <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),header=T)
summary(d.corona)

# Number of deceased and non-deceased
deceased <- nrow(filter(d.corona, d.corona$deceased == "1"))
non_deceased <- nrow(filter(d.corona, d.corona$deceased == "0"))

# Number of males and females for each country
number_of_males_and_females <- d.corona %>%
  group_by(country, sex) %>%
  summarise(amount = n()) %>%
  arrange(desc(amount))


# The number of deceased and non-deceased for each sex
number_of_deceased_and_non_deceased <- d.corona %>%
  group_by(deceased, sex) %>%
  summarise(amount = n()) %>%
  arrange(desc(amount)) 

# The number of deceased and non-deceased in France, separate for each sex.
number_of_deceased_and_non_deceased_in_France_for_each_sex <- filter(d.corona, d.corona$country == "France") %>%
  group_by(deceased, sex) %>%
  summarise(amount = n()) %>%
  arrange(desc(amount)) 


#Linear regression

train_ID = sample(1:nrow(d.corona), nrow(d.corona)/(3/2))
train_data = d.corona[train_ID, ]
test_data = d.corona[-train_ID, ]

age_vs_deceased <- d.corona %>%
  group_by(age, deceased) %>%
  summarise(amount = n()) %>%
  arrange(desc(amount))

age_country_deceased <- d.corona %>%
  group_by(age, deceased, country) %>%
  summarise(amount = n()) %>%
  arrange(desc(amount)) 

scatter_plot <- scatter.smooth(x=age_vs_deceased$age, y=age_vs_deceased$amount, main="Age ~ Deceased")
cor(d.corona$age, d.corona$deceased)

model <- glm(deceased ~ age + country + sex, family = binomial, data=train_data)
anova(model)
summary(model)
coefficients(model)

test <- filter( d.corona, d.corona$country == "Korea" & d.corona$age == "75" & d.corona$sex == "male")

create_confusion_matrix <- function(pred, target) {
  confMat <- table(pred, target)
  colnames(confMat) <- c("PRED FALSE", "PRED TRUE")
  row.names(confMat) <- c("TARGET FALSE", "TARGET TRUE")
  return(confMat)
}


# 1. What is the probability to die of covid for a male age 75 living in Korea?
data_point <- data.frame("age" = 75, "sex" = "male","country" = "Korea")
predict(model, newdata = data_point, type = "response")
# There are a 10.01 % chance of a 75 year-old korean male dying of covid. 

# 2. Is there evidence that males have higher probability to die than females?
# Yes, the regression model shows that there are a higher probability for males to die than females.
# There is statistically significant evidence that males have a higher probability of dying of covid. 
  
# 3. Is there evidence that the country of residence has an influence on the probability to decease?
# Yes, there is evidence that the country of residence has an impact on the probability to decease. There is a big difference between in the probability of decease in Korea, Japan, and Indonesia. Indonesia has a much higher probability of decease than the rest. 
# There is evidence that for France and Indonesia, the country of residence does not impace the probability as much. While for Korea and Japan it has a bigger influence. This is due to the estimate of the dummy variables for France and Indonesia has a larger Pr(>|z|). 

# 4. Quantify how the odds to die changes when someone with otherwise identical covariates is 10 years older than another person.
summary(model)
odds <- exp(10*coefficients(model)[2])

# C.

# Is age a greater risk factor for males than for females?

# Is age a greater risk factor for the French population than for the Indonesian population?

# D.

lda_model <- lda(deceased ~ age + country + sex, data=d.corona)
lda_pred <- predict(lda_model, newdata=test_data[-1])
conf_matrix <- create_confusion_matrix(unlist(lda_pred[1]), test_data$deceased)
sum(conf_matrix)
null_rate <- (conf_matrix[2,1] + conf_matrix[2,2]) / sum(conf_matrix) 
# 1. The "null rate" for misclassification is 5.22%, because this is the proportion of deaths among all cases in the dataset. 
# False

# 2. LDA is not a very useful method for this dataset.
# False

# 3. LDA has a specificity of 1.
specificity <- conf_matrix[1,1] / (conf_matrix[1,1] + conf_matrix[1,2])
# False

# 4. QDA has a lower sensitivity to classify deceased compared to LDA.
qda_model <- qda(deceased ~ ., data=d.corona)
qda_pred <- predict(qda_model, newdata=test_data[-1])
qda_conf <- create_confusion_matrix(unlist(qda_pred[1]), test_data$deceased)
sens_lda <- conf_matrix[2,1] / (sum(conf_matrix[2,]))
sens_qda <- qda_conf[2,1] / (sum(qda_conf[2,]))
# False, they are equal



# Problem 4



# Problem 5
id <- "19auu8YlUJJJUsZY8JZfsCTWzDm6doE7C" # google file ID
d.bodyfat <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),header=T)

rsquared <- function(data, indices) {
  d <- data[indices,] # select samples
  fit <- lm(bodyfat ~ age + weight + bmi, data = d)
  return(summary(fit)$r.square)
}

rsquared(d.bodyfat, nrow(d.bodyfat))
# The R^2 for a linear regression model is 0.5803

set.seed(4268)

# bootstrapping with 1000 replications
results <- boot(data=d.bodyfat, statistic=rsquared,
                R=1000, formula=bmi_formula)

# view results
results
plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")

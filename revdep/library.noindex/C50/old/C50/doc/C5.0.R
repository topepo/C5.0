## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(C50)
library(recipes)

## ----credit-data---------------------------------------------------------
library(recipes)
data(credit_data)

## ----credit-vars---------------------------------------------------------
vars <- c("Home", "Seniority")
str(credit_data[, c(vars, "Status")])

# a simple split
set.seed(2411)
in_train <- sample(1:nrow(credit_data), size = 3000)
train_data <- credit_data[ in_train,]
test_data  <- credit_data[-in_train,]

## ----tree-mod------------------------------------------------------------
library(C50)
tree_mod <- C5.0(x = train_data[, vars], y = train_data$Status)
tree_mod

## ----tree-summ-----------------------------------------------------------
summary(tree_mod)

## ----tree-plot, fig.width = 10-------------------------------------------
plot(tree_mod)

## ----tree-boost----------------------------------------------------------
tree_boost <- C5.0(x = train_data[, vars], y = train_data$Status, trials = 3)
summary(tree_boost)

## ----rule-mod------------------------------------------------------------
rule_mod <- C5.0(x = train_data[, vars], y = train_data$Status, rules = TRUE)
rule_mod
summary(rule_mod)

## ----pred----------------------------------------------------------------
predict(rule_mod, newdata = test_data[1:3, vars])
predict(tree_boost, newdata = test_data[1:3, vars], type = "prob")

## ----cost----------------------------------------------------------------
cost_mat <- matrix(c(0, 2, 1, 0), nrow = 2)
rownames(cost_mat) <- colnames(cost_mat) <- c("bad", "good")
cost_mat

cost_mod <- C5.0(x = train_data[, vars], y = train_data$Status, 
                 costs = cost_mat)
summary(cost_mod)

# more samples predicted as "bad"
table(predict(cost_mod, test_data[, vars]))

# that previously
table(predict(tree_mod, test_data[, vars]))


install.packages(c("neuralnet","GGally", "tidyverse","car"))
library(GGally)
library(tidyverse)
library(neuralnet)
sensitivi_met_data<-read.table("data/sensitivity_met_data.txt",header=T)%>%na.omit()
str(sensitivi_met_data)
summary(sensitivi_met_data)
library(car)
scatterplotMatrix(~Rl_downwell+AT_mbar+RS_downwell+rH+T_b_1477+D_g_1477+F_1_s_g_1477+Rnet,data=sensitivi_met_data,main="Scatterplot with car")
jpeg("ggpairs.jpg")
ggpairs(sensitivi_met_data[c(3,4,5,6,7,8,9,10)],title="ANNMaster")
dev.off()
images=list.files("../images/",full.names=T)
images
names=gsub("[a-zA-Z]|[[:punct:]]"," ",images)
names
scale01 <- function(x){
  (x - min(x)) / (max(x) - min(x))
}
sensiti_met_d <- sensiti_met_data[c(3,4,5,6,7,8,9,10)] %>%  mutate_all(scale01)
# Split into test and train sets
set.seed(12345)
#size is the proportion to training
sensiti_Data_Train <- sample_frac(tbl = sensiti_met_d, replace = FALSE, size = 0.80)  
sensiti_Data_Test <- anti_join(sensiti_met_d, sensiti_Data_Train)

?neuralnet
# act.fct = 'logistic' is the sigmoid, 'tanh', default is 'logistic'
#  linear.output = TRUE is to do regression, default
# algorithm = "rprop+" is default (resilient backpropagation with weight backtracking)

# Calculate activation function
sigmoid <- function(z){1.0/(1.0+exp(-z))}
# Partial derivative of activation function
sigmoid_prime <- function(z){sigmoid(z)*(1-sigmoid(z))}
plot(sigmoid, -100.,100.)

#1-hidden layer ANN with 1 neuron, the simplest of all neural networks:
sensiti_NN1 <- neuralnet(Rnet~Rl_downwell+AT_mbar+Rs_downwell+rH+T_b_1477+D_g_1477+F_1_s_g_1477,
                         data=sensiti_Data_Train)

plot(sensiti_NN1, rep = 'best')
# weights learned by the sensiti_NN1 neural network, 
# and displays the number of iterations before convergence, 
# as well as the SSE (sum of squared errors) of the training data set.

par(mfrow=c(2,2))  # 2 x 2 pictures on one plot
#gwplot plots the generalized weights for one specific covariate and one response variable
# visualize the generalized weights:
gwplot(sensiti_NN1,selected.covariate="Rl_downwell")
gwplot(sensiti_NN1,selected.covariate="AT_mbar")
gwplot(sensiti_NN1,selected.covariate="Rs_downwell")
gwplot(sensiti_NN1,selected.covariate="rH")
gwplot(sensiti_NN1,selected.covariate="T_b_1477")
gwplot(sensiti_NN1,selected.covariate="D_g_1477")
gwplot(sensiti_NN1,selected.covariate="F_1_s_g_1477")

Test_NN1_Output <- compute(sensiti_NN1, sensiti_Data_Test[, 1:7])$net.result
NN1_Test_SSE <- sum((Test_NN1_Output - sensiti_Data_Test[, 8])^2)/2
NN1_Test_SSE

# cor to calculate the correlation between the two numeric vectors:
cor(Test_NN1_Output, sensiti_Data_Test[, 8])

# 2-Hidden Layers, Layer-1 4-neurons, Layer-2, 1-neuron

sensiti_NN2 <- neuralnet(Rnet~Rl_downwell+AT_mbar+Rs_downwell+rH+T_b_1477+D_g_1477+F_1_s_g_1477,
                         data=sensiti_Data_Train,
                         hidden = c(4, 1))

## Test Error
Test_NN2_Output <- compute(sensiti_NN2, sensiti_Data_Test[, 1:7])$net.result
NN2_Test_SSE <- sum((Test_NN2_Output - sensiti_Data_Test[, 8])^2)/2
NN2_Test_SSE
plot(sensiti_NN2, rep = 'best')

jpeg('NN2.jpg')
plot(sensiti_NN2, rep = 'best')
dev.off()

# cor to calculate the correlation between the two numeric vectors:
cor(Test_NN2_Output,sensiti_Data_Test[, 8])

par(mfrow=c(2,2))
gwplot(sensiti_NN2,selected.covariate="Rl_downwell")
gwplot(sensiti_NN2,selected.covariate="AT_mbar")
gwplot(sensiti_NN2,selected.covariate="Rs_downwell")
gwplot(sensiti_NN2,selected.covariate="rH")
gwplot(sensiti_NN2,selected.covariate="T_b_1477")
gwplot(sensiti_NN2,selected.covariate="D_g_1477")
gwplot(sensiti_NN2,selected.covariate="F_1_s_g_1477")

# 2-Hidden Layers, Layer-1 4-neurons, Layer-2 2-neurons, 1-neuron
# function

sensiti_NN3 <- neuralnet(Rnet~Rl_downwell+AT_mbar+Rs_downwell+rH+T_b_1477+D_g_1477+F_1_s_g_1477,data=sensiti_Data_Train,
                         hidden = c(4, 2), linear.output = TRUE)

## Test Error
Test_NN3_Output <- compute(sensiti_NN3, sensiti_Data_Test[, 1:7])$net.result
NN3_Test_SSE <- sum((Test_NN3_Output - sensiti_Data_Test[, 8])^2)/2

plot(sensiti_NN3, rep = 'best')


# cor to calculate the correlation between the two numeric vectors:
cor(Test_NN3_Output,sensiti_Data_Test[, 8])


par(mfrow=c(2,2))
gwplot(sensiti_NN3,selected.covariate="Rl_downwell")
gwplot(sensiti_NN3,selected.covariate="AT_mbar")
gwplot(sensiti_NN3,selected.covariate="Rs_downwell")
gwplot(sensiti_NN3,selected.covariate="rH")
gwplot(sensiti_NN3,selected.covariate="T_b_1477")
gwplot(sensiti_NN3,selected.covariate="D_g_1477")
gwplot(sensiti_NN3,selected.covariate="F_1_s_g_1477")


# training error
NN1_Train_SSE <- sensiti_NN1$result.matrix[1]
NN2_Train_SSE <- sensiti_NN2$result.matrix[1]
NN3_Train_SSE <- sensiti_NN3$result.matrix[1]
# Bar plot of results
Regression_NN_Errors <- tibble(Network = rep(c("NN1", "NN2", "NN3"), each = 2), 
                               DataSet = rep(c("Train", "Test"), time = 3), 
                               SSE = c(NN1_Train_SSE, NN1_Test_SSE, 
                                       NN2_Train_SSE, NN2_Test_SSE, 
                                       NN3_Train_SSE, NN3_Test_SSE))
Regression_NN_Errors %>% ggplot(aes(Network, SSE, fill = DataSet)) + 
  geom_col(position = "dodge") + 
  ggtitle("Regression ANN's SSE")



# to do regression linear.output=TRUE, it is default
# rep is the repeating the neural network reusing the weights as initial weights
sensiti_NN2 <- neuralnet(Rnet~Rl_downwell+AT_mbar+Rs_downwell+rH+T_b_1477+D_g_1477+F_1_s_g_1477,
                         data=sensiti_Data_Train,
                         hidden = c(4, 1), rep = 10, linear.output=TRUE)
## Test Error
Test_NN2_Output <- compute(sensiti_NN2, sensiti_Data_Test[, 1:7])$net.result
NN2_Test_SSE <- sum((Test_NN2_Output - sensiti_Data_Test[, 8])^2)/2

plot(sensiti_NN2, rep = "best")


# Generalised linear model regression

#Now in order to compare with the results of the neural network, we can try a generalised linear model to perform regression.

#generalised linear model
sensiti_met_d <- sensiti_met_data[c(3,4,5,6,7,8,9,10)] # %>%  mutate_all(scale01)

# Split into test and train sets

#size is the proportion to training
sensiti_Data_Train_uns <- sample_frac(tbl = sensiti_met_d, replace = FALSE, size = 0.80)  
sensiti_Data_Test_uns <- anti_join(sensiti_met_d, sensiti_Data_Train_uns)


lm.fit <- glm(Rnet~., data=sensiti_Data_Train_uns)
summary(lm.fit)
pr.lm <- predict(lm.fit,sensiti_Data_Test_uns)
MSE.lm <- sum((pr.lm - sensiti_Data_Test_uns$Rnet)^2)/nrow(sensiti_Data_Test_uns)
# mean squared error (MSE) as a measure of how much our predictions are far away from the real data:
paste("MSE.lm: ", round(MSE.lm, 6))

pr.nn <- compute(sensiti_NN2, sensiti_Data_Test[, 1:7])
## Test Error
#Test_NN2_Output <- compute(sensiti_NN2, sensiti_Data_Test[, 1:7])$net.result
#NN2_Test_SSE <- sum((Test_NN2_Output - sensiti_Data_Test[, 8])^2)/2

pr.nn_ <- pr.nn$net.result*(max(sensiti_met_data$Rnet)-min(sensiti_met_data$Rnet))+min(sensiti_met_data$Rnet)
test.r <- (sensiti_Data_Test$Rnet)*(max(sensiti_met_data$Rnet)-min(sensiti_met_data$Rnet))+min(sensiti_met_data$Rnet)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(sensiti_Data_Test)


#compare both MSEs (from generalised linear model and neural network)
print(paste(MSE.lm,MSE.nn))
MSE.nn < MSE.lm

# Apparently, the net is doing a better work than the linear model at predicting Rnet.

par(mfrow=c(1,2))
plot(sensiti_Data_Test_uns$Rnet,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(sensiti_Data_Test_uns$Rnet,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)


plot(sensiti_Data_Test_uns$Rnet,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(sensiti_Data_Test_uns$Rnet,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))








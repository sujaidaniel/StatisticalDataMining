library(ISLR2)
head(College)
dim(College)

#Dividing test and training set
set.seed(131)
train_indis <- sample ( c ( 1 : length ( College [ , 1 ] ) ) , size = round ( 2 / 3 * length ( College [ , 1 ] ) ) , replace = FALSE )
train <- College[train_indis,]
test <- College[-train_indis,]

#Fitting a linear model using the least squares
lm.fit = lm(Apps~., data=train)
lm.pred = predict(lm.fit, new_data=test)
mean((test$Apps - lm.pred )^2)

install.packages("glmnet")
library(glmnet)
install.packages("caret")
library(caret)

set.seed(1)
#Set up matrices needed for the glmnet functions
train.matrix <- model.matrix(Apps~., data = train)[,-1]
test.matrix <- model.matrix(Apps~., data = test)[,-1]
#Choosing lambda using cross-validation
cv.out = cv.glmnet(train.matrix,train$Apps,alpha=0)
bestlamda = cv.out$lambda.min
bestlamda

#Fit a ridge regression
ridge.model <- glmnet(train.matrix, train$Apps , alpha = 0, lambda = bestlamda)
#Make predictions
ridge.prediction <- predict(ridge.model, s=bestlamda, newx = test.matrix)
#Calculate test error
mean((ridge.prediction - test$Apps)^2)

#Choosing lambda using cross-validation
set.seed(121)
cv.out2 = cv.glmnet(train.matrix, train$Apps,alpha=1)
bestlamda2 = cv.out2$lambda.min
paste("Lamda value :" , bestlamda2)
#Fit a lasso model 
lasso.model = glmnet(train.matrix, train$Apps, alpha = 1, lambda = bestlamda2)
#make predictions
lasso.prediction = predict(lasso.model , s = bestlamda2 , newx = test.matrix)
#calculating test error
paste("Test Error :", mean((lasso.prediction - test$Apps)^2))

#Fit a lasso model 
lasso.model = glmnet(train.matrix, train$Apps, alpha = 1, lambda = bestlamda2)
#make predictions
lasso.prediction = predict(lasso.model , s = bestlamda2 , newx = test.matrix)
#calculating test error
mean((lasso.prediction - test$Apps)^2)

#Retrieving the lasso coefficients
lasscoefficients = predict(lasso.model, type="coefficients", s = bestlamda2 )[1:length(lasso.model$beta),]
#Printing non zero coefficients
lasscoefficients[lasscoefficients!=0]

#Accuracy of prediction of the number of applications received, through ridge regression R-square value.
totalSumOFSquares = sum((mean(test$Apps) - test$Apps)^2)
totalSumOfResidualSquares = sum((ridge.prediction - test$Apps)^2)
1 - (totalSumOfResidualSquares)/(totalSumOFSquares)

df = read.table('ticdata2000.txt',sep="\t")
test_df = read.table('ticeval2000.txt', sep = '\t')
test_y = read.table('tictgts2000.txt', sep = '\t')
test_df = cbind(test_df,test_y)
dim(test_df)
colnames(df) = colnames(Caravan)
colnames(test_df) = colnames(Caravan)
head(test_df)

linearRegression = lm(Purchase~. , data = df)
predict_df = predict(linearRegression , test_df )
test_error <- sum((test_df$Purchase - predict_df)^2) 
mean_squared_error <- test_error/nrow(test_df)

paste("Residual sum of squares error :", test_error)
paste("Mean Squared error :", mean_squared_error)

install.packages("leaps")
library(leaps)

regfit.fwd <- regsubsets(Purchase~., data=df, nbest=1, nvmax=85 , method="forward")
my_sum_forward <-summary(regfit.fwd)
predict.regsubsets= function(object, newdata, id){
    form = as.formula(object$call[[2]])
    mat= model.matrix(form,newdata)
    coefi= coef(object,id=id)
    xvars=names(coefi)
    mat[,xvars]%*%coefi
}

train_err_store_fwd <- matrix(rep(NA,85))
test_err_store_fwd <- matrix(rep(NA,85))

for (i in 1:85){
    y_hat_train= predict(regfit.fwd, newdata = df, id = i )
    y_hat_test= predict.regsubsets(regfit.fwd, newdata = test_df, id=i )
    train_err_store_fwd[i]=(1/length(test_df))*sum((df$Purchase - y_hat_train)^2)
    test_err_store_fwd[i]=sum((test_df$Purchase - y_hat_test)^2)
}


par(mfrow=c(2,2))
plot(my_sum_forward$cp, xlab="No of variables", ylab="Cp")
plot(my_sum_forward$bic, xlab="No of variables", ylab="BIC")
plot(my_sum_forward$rss, xlab="No of variables", ylab="RSS")
plot(my_sum_forward$adjr2, xlab="No of variables", ylab="Adjusted Rsq")

which(my_sum_forward$cp==min(my_sum_forward$cp))
which(my_sum_forward$bic==min(my_sum_forward$bic))
which(my_sum_forward$rss==min(my_sum_forward$rss))
which(my_sum_forward$adjr2==max(my_sum_forward$sdjr2))

regfit.bwd <- regsubsets(Purchase~., data=df, nbest=1, nvmax=85 , method="backward")
my_sum_backward <-summary(regfit.bwd)
predict.regsubsets= function(object, newdata, id){
    form = as.formula(object$call[[2]])
    mat = model.matrix(form,newdata)
    coefi = coef(object,id=id)
    xvars =names(coefi)
    mat[,xvars] %*% coefi
}

train_err_store_bwd <- matrix(rep(NA,85))
test_err_store_bwd <- matrix(rep(NA,85))
for (i in 1:85){
    y_hat_train = predict(regfit.bwd, newdata = df, id = i )
    y_hat_test = predict.regsubsets(regfit.bwd, newdata = test_df, id = i )
    train_err_store_bwd[i] = (1/length(test_df))*sum((df$Purchase - y_hat_train)^2)
    test_err_store_bwd[i] = (1/length(test_df))*sum((test_df$Purchase - y_hat_test)^2)
}

par(mfrow=c(2,2))
plot(my_sum_backward$cp, xlab="No of variables", ylab="Cp")
plot(my_sum_backward$bic, xlab="No of variables", ylab="BIC")
plot(my_sum_backward$rss, xlab="No of variables", ylab="RSS")
plot(my_sum_backward$adjr2, xlab="No of variables", ylab="Adjusted Rsq")

which(my_sum_backward$cp==min(my_sum_backward$cp))
which(my_sum_backward$bic==min(my_sum_backward$bic))
which(my_sum_backward$rss==min(my_sum_backward$rss))
which(my_sum_backward$adjr2==max(my_sum_backward$sdjr2))

set.seed(100)
X_train <- df[,c(1:85)]
y_train <- df[,c(86)]
lasso.mod <- glmnet(X_train,y_train, alpha = 1)
cv.out6 = cv.glmnet(as.matrix(X_train), y_train, alpha = 1)
bestlamda6 = cv.out$lambda.min
paste("Lamda Value :",bestlamda6)
lasso.pred <- predict(lasso.mod, s = bestlamda6, newx = as.matrix(test_df[,c(1:85)]), type = "response")
test_error_lasso <- sum((lasso.pred - test_df$Purchase)^2)  #sum of residual squares error
mean_squared_lasso <- test_error_lasso/nrow(test_df) #mean squared error
paste( "Mean Squared Error for Lasso Regression :" , mean_squared_lasso)

ridge.mod <- glmnet(X_train,y_train, alpha = 0 )
cv.out7 = cv.glmnet(as.matrix(X_train), y_train, alpha = 0 )
bestlamda7 = cv.out$lambda.min
paste("Lamda Value :",bestlamda7)
ridge.pred <- predict(ridge.mod, s = bestlamda7, newx = as.matrix(test_df[,c(1:85)]), type = "response")
test_error_ridge <- sum((ridge.pred - test_df$Purchase)^2)  #sum of residual squares error
mean_squared_ridge <- test_error_ridge/nrow(test_df) #mean squared error
paste( "Mean Squared Error for Ridge Regression :" , mean_squared_ridge)





# install.packages(glmnet);
library(glmnet);

cbind(iris$Petal.Width,iris$Sepal.Length,iris$Sepal.Width,rnorm(nrow(iris))) -> dsnmatrix.X;
rsp.Y <- iris$Petal.Length;

cv.glmnet(dsnmatrix.X,rsp.Y,alpha=1) -> glmnet.iris.cv.lasso; 
cv.glmnet(dsnmatrix.X,rsp.Y,alpha=0) -> glmnet.iris.cv.ridge;
cv.glmnet(dsnmatrix.X,rsp.Y,alpha=0.5) -> glmnet.iris.cv.enet; 

glmnet(dsnmatrix.X,rsp.Y,lambda=glmnet.iris.cv.lasso$lambda.1se) -> glmnet.iris.cv.lasso.mod;
glmnet(dsnmatrix.X,rsp.Y,lambda=glmnet.iris.cv.ridge$lambda.1se) -> glmnet.iris.cv.ridge.mod;
glmnet(dsnmatrix.X,rsp.Y,lambda=glmnet.iris.cv.enet$lambda.1se) -> glmnet.iris.cv.enet.mod;

cbind(coef(glmnet.iris.cv.lasso.mod),coef(glmnet.iris.cv.ridge.mod),
      coef(glmnet.iris.cv.enet.mod)) -> coefficients.shrinkage; 

library(htmlTable); 

colnames(coefficients.shrinkage) <- c("Lasso", "Ridge", "Elastic Net")
rownames(coefficients.shrinkage) <- c("Intercept","Petal Width", "Sepal Length", "Sepal Width", "Random")

format(coefficients.shrinkage,digits=4) -> coefficients.shrinkage;
htmlTable(coefficients.shrinkage)

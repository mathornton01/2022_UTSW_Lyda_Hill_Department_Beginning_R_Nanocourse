formula(breaks~wool+tension) -> breakdata.form; 

cbind(warpbreaks$wool,warpbreaks$tension) -> X.dat;
warpbreaks$breaks -> Y.dat;
cv.glmnet(X.dat,Y.dat,alpha=1,family=poisson(link="identity")) -> breakdata.mod.lasso;
cv.glmnet(X.dat,Y.dat,alpha=0,family=poisson(link="identity")) -> breakdata.mod.ridge;
cv.glmnet(X.dat,Y.dat,alpha=0.5,family=poisson(link="identity")) -> breakdata.mod.enet;

cbind(coef(breakdata.mod.lasso),coef(breakdata.mod.ridge),coef(breakdata.mod.enet)) -> tabdata;

colnames(tabdata) <- c("LASSO", "ridge", "enet");
rownames(tabdata) <- c("Intercept","Wool","Tension");

library(htmlTable)

format(tabdata,digits=3) -> tabdata; 

htmlTable(tabdata);

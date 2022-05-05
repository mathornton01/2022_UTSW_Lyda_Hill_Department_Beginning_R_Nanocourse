# Introduction to R for Beginners
#  May 10, 2022 - Introduction to Linear Models 
#    iris dataset 



# It is never a bad idea to plot the data first to examine
# whether a linear relationship makes sense. 

# The pairs function will make simultaneous scatterplots of 
#  our data, so we can see where there are potential linear
#  trends. 
pairs(iris)
summary(iris)

formula("Fertility~Catholic") -> form1;
formula(Fertility~Catholic) -> form2;

glm(form1,data=swiss)
glm(form2,data=swiss)

iris[,1:4] -> irisn;

formula(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width) -> irisn.form1;

glm(irisn.form1,data=irisn) -> irisn.mod1;
lm(irisn.form1,data=irisn) -> irisn.mod1.lm; 

summary(irisn.mod1);
summary(irisn.mod1.lm);

formula(Sepal.Length~0+Sepal.Width+Petal.Length+Petal.Width) -> irisn.form2;

glm(irisn.form2,data=irisn) -> irisn.mod2; 

summary(irisn.mod2)

formula(Sepal.Length~0+Petal.Length*Petal.Width*Sepal.Width) -> irisn.form3;

glm(irisn.form3,data=irisn) -> irisn.mod3;

summary(irisn.mod3);


# Forward stepwise selection for single/joint fixed effects. 
formula(Sepal.Length~0) -> irisn.ioform;
glm(irisn.ioform,data=irisn) -> irisn.iomod; 

forward <- step(irisn.iomod, direction='forward',scope=formula(irisn.mod3),trace=0)
forward$anova
forward$coefficients

# Backwards stepwise selection for single/joint fixed effects. 
backward <- step(irisn.mod3, direction='backward',scope=formula(irisn.mod3),trace=0);
backward$anova
backward$coefficients

# Bidirectional Stepwise Selection for single/joint fixed effects. 
both <- step(irisn.iomod,direction='both',scope=formula(irisn.mod3),trace=0);
both$anova
both$coefficients

library(glmnet);

X <- irisn[,2:4];
X <- cbind(1,X);
X[,5] <- X[,2]*X[,3];
X[,6] <- X[,2]*X[,4];
X[,7] <- X[,3]*X[,4];
X[,8] <- X[,2]*X[,3]*X[,4]; 

Y <- irisn[,1]; 

enet.mod1 <- cv.glmnet(X,Y,family="gaussian");

enet.mod1

plot(enet.mod1)

# Joint Effects Modeling. 

par(mfrow=c(1,2))
med.pl <- median(irisn$Petal.Length);
ind.pl.high <- irisn$Petal.Length > med.pl;

plot(irisn[ind.pl.high,'Sepal.Width'],irisn[ind.pl.high,'Sepal.Length'],
     xlab="Sepal Width",
     ylab="Sepal Length",
     ylim=c(4,10),
     main="Dependence of (Sepal Length on Sepal Width) Depends on Petal Length \n 
     Fisher's Iris Data Set (Sepal Width only Models) (N=150)")
points(irisn[!ind.pl.high,'Sepal.Width'],irisn[!ind.pl.high,'Sepal.Length'],col='red',pch=2)

abline(lm(Sepal.Length~1+Sepal.Width,data=irisn[ind.pl.high,]))
abline(lm(Sepal.Length~1+Sepal.Width,data=irisn[!ind.pl.high,]),col='red')
med.pl
legend('topleft',legend=c(paste("Petal Length >",med.pl),
                          paste("Petal Length <",med.pl)),col=c('black','red'),
       pch=1:2)

text(2.75,5,"Estimated Slope = -0.097 cm \n p-value: 0.351",col='red')
text(3.55,6.55,"Estimated Slope = 0.761 cm \n p-value: 0.000404")

formula(Sepal.Length~0+Sepal.Width*Petal.Length*Petal.Width) -> irisn.form3.a;
formula(Sepal.Length~0+Sepal.Width+Petal.Length+Petal.Width+
          Sepal.Width:Petal.Length + 
          Sepal.Width:Petal.Width + 									Petal.Length:Petal.Width + 			
          Sepal.Width:Petal.Length:Petal.Width) -> irisn.form3.b;
irisn.form3.a==irisn.form3.b
irisn.form3.a
irisn.form3.b

summary(glm(irisn.form3.a,data=irisn))
summary(glm(irisn.form3.b,data=irisn))


glm(irisn.form3.a,data=iris[iris$Species=='setosa',])-> irisn.setosa.full;
glm(irisn.form3.a,data=iris[iris$Species=='versicolor',])-> irisn.versicolor.full;
glm(irisn.form3.a,data=iris[iris$Species=='virginica',])-> irisn.virginica.full;

cbind(coefficients(irisn.setosa.full),
      coef(summary(irisn.setosa.full))[,4],
      coefficients(irisn.versicolor.full),
      coef(summary(irisn.versicolor.full))[,4],
      coefficients(irisn.virginica.full),
      coef(summary(irisn.virginica.full))[,4]) -> coefficients.df;

library(htmlTable)
colnames(coefficients.df) <- rep(c("Coefficient","P-Value"),3)
format(coefficients.df,digits=1,scientific=FALSE) -> coefficients.df;
htmlTable(coefficients.df,n.rgroup=c(3,3,1),
          rgroup=c("Singleton Effects", "Bivariate Effects", "Trivariate Effects"),
          n.cgroup=rep(2,3),cgroup=c("Setosa Data", "Versicolor Data", "Virginica Data"),
          scientific=F)

simple.formula <- formula(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width); 

data.split <- sample(size=nrow(iris),c("test","train"),replace=TRUE,prob=c(0.1,0.9))

simple.model <- glm(simple.formula, data=iris[data.split=="train",]);

test.preds <- predict(simple.model,iris[data.split=="test",]);

plot(test.preds,iris[data.split=="test","Sepal.Length"]);
abline(0,1)
summary(simple.model)


# Ordinal Predictors Example 

inf.mort.quants <- quantile(swiss$Infant.Mortality,c(0.25,0.5,0.27));

inf.mort.ord <- ifelse(swiss$Infant.Mortality <= inf.mort.quants[1], "Q1", 
                       ifelse(swiss$Infant.Mortality <= inf.mort.quants[2], "Q2", 
                              ifelse(swiss$Infant.Mortality <= inf.mort.quants[3], "Q3","Q4")));

swiss$imo <- inf.mort.ord; 

swiss.form.ord.1 <- formula(Fertility ~ ordered(imo,levels=paste("Q",1:4,sep="")));
swiss.mod.ord.1 <- glm(swiss.form.ord.1, data=swiss); 

inf.mort.cont.lin <- ifelse(inf.mort.ord == "Q1", 1, 
                        ifelse(inf.mort.ord == "Q2",2,
                               ifelse(inf.mort.ord == "Q3", 3, 4)));

inf.mort.cont.qua <- ifelse(inf.mort.ord == "Q1", 1, 
                            ifelse(inf.mort.ord == "Q2",4,
                                   ifelse(inf.mort.ord == "Q3", 9, 16)));

swiss$imo.l <- inf.mort.cont.lin;
swiss$imo.q <- inf.mort.cont.qua; 

swiss.form.ord.2 <- formula(Fertility ~ imo.l +imo.q);
swiss.mod.ord.2 <- glm(swiss.form.ord.2,data=swiss);

inf.mort.quants <- quantile(swiss$Infant.Mortality,c(0.33,0.66));
inf.mort.ord <- ifelse(swiss$Infant.Mortality <= inf.mort.quants[1], "Low", 
                       ifelse(swiss$Infant.Mortality <= inf.mort.quants[2], "Medium", "High"));
swiss$imo <- inf.mort.ord;

swiss.ex1.f1 <- formula(Fertility ~ relevel(as.factor(imo),ref="High"));
swiss.ex1.m1 <- glm(swiss.ex1.f1,data=swiss);
summary(swiss.ex1.m1);

summary(swiss.mod.ord.1)
summary(swiss.mod.ord.2)

iris.form.1 <- formula(Sepal.Length~Sepal.Width*Petal.Length*Petal.Width+Species);
iris.mod.1 <- glm(iris.form.1,data=iris); 

plot(iris.mod.1)







# Suppose you would like to build a linear model for a subset of the data (train)
#  Then use the other part (test) to determine how well the model fit.  This 
#  can be done by first creating a selector with the desired proportions 


iris.form.1 <- formula(Sepal.Length~Sepal.Width*Petal.Length*Petal.Width+as.factor(Species)); 
M <- 100000; 

percentage.training <- 0.7; 
coefs.vec <- list(); 
for (i in 1:M){
ind.train <- rbinom(n=nrow(iris),size=1,prob=percentage.training);
glm(iris.form.1,data=iris[which(as.logical(ind.train)),]) -> iris.mod.1;
coefs.vec <- append(coefs.vec, list(coef(iris.mod.1)));
}

unlist(lapply(1:M,function(x){coefs.vec[[x]][1]})) -> intercept.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][2]})) -> SepalWidth.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][3]})) -> PetalLength.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][4]})) -> PetalWidth.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][5]})) -> Versicolor.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][6]})) -> Virginica.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][7]})) -> SW.PL.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][8]})) -> SW.PW.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][9]})) -> PL.PW.estimates;
unlist(lapply(1:M,function(x){coefs.vec[[x]][10]})) -> SW.PL.PW.estimates;


pdf("Simulation-Plot-Results.PDF",width=18,height=8);

par(mfrow=c(1,2));

plot(density(intercept.estimates),
     main=paste("Intercept in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(intercept.estimates),lty=2,col="blue");
arrows(x0=mean(intercept.estimates),
       y0=0.2,
       x1=mean(intercept.estimates)+sd(intercept.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(intercept.estimates),digits=2)
simmu <- format(mean(intercept.estimates),digits=2)
text(x=(2*mean(intercept.estimates)+sd(intercept.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(intercept.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(SepalWidth.estimates),
     main=paste("Sepal Width Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(SepalWidth.estimates),lty=2,col="blue");
arrows(x0=mean(SepalWidth.estimates),
       y0=0.2,
       x1=mean(SepalWidth.estimates)+sd(SepalWidth.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(SepalWidth.estimates),digits=2)
simmu <- format(mean(SepalWidth.estimates),digits=2)
text(x=(2*mean(SepalWidth.estimates)+sd(SepalWidth.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(SepalWidth.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(PetalLength.estimates),
     main=paste("Petal Length Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(PetalLength.estimates),lty=2,col="blue");
arrows(x0=mean(PetalLength.estimates),
       y0=0.2,
       x1=mean(PetalLength.estimates)+sd(PetalLength.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(PetalLength.estimates),digits=2)
simmu <- format(mean(PetalLength.estimates),digits=2)
text(x=(2*mean(PetalLength.estimates)+sd(PetalLength.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(PetalLength.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(PetalWidth.estimates),
     main=paste("Petal Width Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(PetalWidth.estimates),lty=2,col="blue");
arrows(x0=mean(PetalWidth.estimates),
       y0=0.2,
       x1=mean(PetalWidth.estimates)+sd(PetalWidth.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(PetalWidth.estimates),digits=2)
simmu <- format(mean(PetalWidth.estimates),digits=2)
text(x=(2*mean(PetalWidth.estimates)+sd(PetalWidth.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(PetalWidth.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(Versicolor.estimates),
     main=paste("Versicolor Species Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(Versicolor.estimates),lty=2,col="blue");
arrows(x0=mean(Versicolor.estimates),
       y0=0.2,
       x1=mean(Versicolor.estimates)+sd(Versicolor.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(Versicolor.estimates),digits=2)
simmu <- format(mean(Versicolor.estimates),digits=2)
text(x=(2*mean(Versicolor.estimates)+sd(Versicolor.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(Versicolor.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(Virginica.estimates),
     main=paste("Virginica Species Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(Virginica.estimates),lty=2,col="blue");
arrows(x0=mean(Virginica.estimates),
       y0=0.2,
       x1=mean(Virginica.estimates)+sd(Virginica.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(Virginica.estimates),digits=2)
simmu <- format(mean(Virginica.estimates),digits=2)
text(x=(2*mean(Virginica.estimates)+sd(Virginica.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(Virginica.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(SW.PL.estimates),
     main=paste("Sepal Width & Petal Length Joint Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(SW.PL.estimates),lty=2,col="blue");
arrows(x0=mean(SW.PL.estimates),
       y0=0.2,
       x1=mean(SW.PL.estimates)+sd(SW.PL.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(SW.PL.estimates),digits=2)
simmu <- format(mean(SW.PL.estimates),digits=2)
text(x=(2*mean(SW.PL.estimates)+sd(SW.PL.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(SW.PL.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(SW.PW.estimates),
     main=paste("Sepal Width & Petal Width Joint Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(SW.PW.estimates),lty=2,col="blue");
arrows(x0=mean(SW.PW.estimates),
       y0=0.2,
       x1=mean(SW.PW.estimates)+sd(SW.PW.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(SW.PW.estimates),digits=2)
simmu <- format(mean(SW.PW.estimates),digits=2)
text(x=(2*mean(SW.PW.estimates)+sd(SW.PW.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(SW.PW.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(PL.PW.estimates),
     main=paste("Petal Length & Petal Width Joint Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(PL.PW.estimates),lty=2,col="blue");
arrows(x0=mean(PL.PW.estimates),
       y0=0.2,
       x1=mean(PL.PW.estimates)+sd(PL.PW.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(PL.PW.estimates),digits=2)
simmu <- format(mean(PL.PW.estimates),digits=2)
text(x=(2*mean(PL.PW.estimates)+sd(PL.PW.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(PL.PW.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

plot(density(SW.PL.PW.estimates),
     main=paste("Sepal Width & Petal Length & Petal Width Joint Effect Estimates  in MV LR model for iris data (M = ", M, " Simulations)\n",
                as.character(iris.form.1)[[2]], " = ", as.character(iris.form.1)[[3]]));
abline(v=mean(SW.PL.PW.estimates),lty=2,col="blue");
arrows(x0=mean(SW.PL.PW.estimates),
       y0=0.2,
       x1=mean(SW.PL.PW.estimates)+sd(SW.PL.PW.estimates),
       y1=0.2,
       code=3,
       length=0.1)
simsd <- format(sd(SW.PL.PW.estimates),digits=2)
simmu <- format(mean(SW.PL.PW.estimates),digits=2)
text(x=(2*mean(SW.PL.PW.estimates)+sd(SW.PL.PW.estimates))/2,y=0.18,
     labels=bquote(sigma == .(simsd)))
text(x=mean(SW.PL.PW.estimates)+.3,y=0.05,labels=bquote(mu == .(simmu)))

dev.off()


trainprop <- 0.8; 
trainnum <- floor(trainprop*nrow(iris));
testnum <- nrow(iris)-trainnum; 
trainind <- sample(c(rep(T,trainnum),rep(F,testnum)));


iris.lm.pred.form.1 <- formula(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width);
iris.lm.pred.mod.1 <- glm(iris.lm.pred.form.1,data=iris[trainind,]);

par(mfrow=c(1,1))
iris.lm.pred.mod.1.predictions <- predict(iris.lm.pred.mod.1,newdata=iris[!trainind,]);
plot(iris.lm.pred.mod.1.predictions,iris[!trainind,"Sepal.Length"],
     xlab="Predicted Sepal Length", ylab="Actual Sepal Length",
     main=paste("True vs. Predicted Sepal Length from Linear Model\n",
                as.character(iris.lm.pred.form.1)[2], " = ", as.character(iris.lm.pred.form.1)[3]));

Petal.Width.Quantiles <- quantile(iris$Petal.Width,c(0.33,0.66));
iris$Petal.Width.Class <- ifelse(iris$Petal.Width <= Petal.Width.Quantiles[1], "Low", 
                                 ifelse(iris$Petal.Width <= Petal.Width.Quantiles[2], "Medium", "High"));

iris$Petal.Width.Class <- ordered(iris$Petal.Width.Class,levels=c("Low","Medium","High"));
iris$Petal.Width.Class;

iris.form.2 <- formula(Petal.Length~Petal.Width.Class);
iris.mod.2 <- glm(iris.form.2,data=iris);
summary(iris.mod.2);

iris$Linear.PWC <- ifelse(iris$Petal.Width <= Petal.Width.Quantiles[1], 0, 
                                 ifelse(iris$Petal.Width <= Petal.Width.Quantiles[2], 1, 2));
iris$Quadratic.PWC <- iris$Linear.PWC^2;
iris.form.3 <- formula(Petal.Length~Linear.PWC+Quadratic.PWC);
iris.mod.3 <- glm(iris.form.3,data=iris);

summary(iris.mod.3);


formula(Sepal.Length~0+offset(1.3*Sepal.Width)+Petal.Length+Petal.Width+
           Sepal.Width:Petal.Length + 
           Sepal.Width:Petal.Width + Petal.Length:Petal.Width + 			
           Sepal.Width:Petal.Length:Petal.Width) -> irisn.form5;

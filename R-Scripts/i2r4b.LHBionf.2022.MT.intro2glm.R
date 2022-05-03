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

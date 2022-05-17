iris.null.formula <- formula(Petal.Length ~ 1); 
iris.full.formula <- formula(Petal.Length ~ Petal.Width*Sepal.Length*Sepal.Width);

iris.null.model <- glm(iris.null.formula,data=iris); 
iris.full.model <- glm(iris.full.formula,data=iris); 

step(iris.null.model,direction='forward',scope=iris.full.formula) -> iris.forward.step;
step(iris.full.model,direction='backward',scope=iris.full.formula) -> iris.backward.step;
step(iris.null.model,direction='both',scope=iris.full.formula) -> iris.both.step;

iris.forward.step$anova
iris.backward.step$anova
iris.both.step$anova

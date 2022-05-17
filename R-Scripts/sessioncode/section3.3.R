# install.packages("htmlTable");
library(htmlTable);

iris.table.formula <- formula(Petal.Length~Petal.Width+Sepal.Length+Sepal.Width);
iris.table.model <- glm(iris.table.formula,data=iris);

cbind(coef(iris.table.model),confint(iris.table.model)) -> table.data; 

format(table.data,digits=3) -> table.data;

colnames(table.data) <- c("est.","L CI", "U CI");
htmlTable(table.data);

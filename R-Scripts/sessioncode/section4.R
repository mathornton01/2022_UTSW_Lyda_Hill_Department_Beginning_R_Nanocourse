library(survival)
formula(Surv(lung$time,lung$status)~age+as.factor(sex)+meal.cal+wt.loss) -> cox.formula;
coxph(cox.formula,data=lung) -> cox.model;
summary(cox.model)

library(survminer)
plot(cox.zph(cox.model))
ggcoxzph(cox.zph(cox.model))


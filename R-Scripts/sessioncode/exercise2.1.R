# Introduction to R for Beginners LH Department of Bioinformatics 2022
#  Day 2
#   Micah Thornton
# Exercise 2 

warpbreaks$HB <- (warpbreaks$breaks > median(as.numeric(warpbreaks$breaks)));
wb.log.form <- formula(HB~wool+tension);
wb.log.mod <- glm(wb.log.form,data=warpbreaks,family=binomial(link="logit"));
summary(wb.log.mod);



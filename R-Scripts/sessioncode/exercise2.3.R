qqplot(rpois(100000,mean(warpbreaks$breaks)),warpbreaks$breaks); abline(0,1);
qqplot(rnorm(100000,mean(warpbreaks$breaks),sd(warpbreaks$breaks)),warpbreaks$breaks); abline(0,1);

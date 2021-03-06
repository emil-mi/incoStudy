install.packages("xtable")
install.packages('inline')
install.packages('Rcpp')
Sys.setenv(R_MAKEVARS_USER='')
options(repos = c(getOption("repos"), rstan = "http://wiki.stan.googlecode.com/git/R"))
install.packages('rstan', type = 'source')
install.packages("ggplot2")
install.packages("latticeExtra")
install.packages("coin")
install.packages("pwr")
install.packages("gsDesign")
install.packages("MCMCpack")
install.packages("hypergeo")
install.packages("norm")
install.packages("HH")
install.packages("lme4")
install.packages("languageR")
install.packages("pbkrtest")
install.packages("plm")
install.packages("Hmisc")
install.packages("nortest")

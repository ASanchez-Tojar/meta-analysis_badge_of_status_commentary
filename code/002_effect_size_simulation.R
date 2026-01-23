################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code
#
# Pietro D'Amelio (pie.damelio@googlemail.com): revised & adjusted the code

# Script first created in Jan 2025 and sequentially revised until Jan 2026

#######################################################################

# This script is to assess the consequences of choosing different type of 
# effect size equations and transformations.

# Assessing the Association Between Animal Color and Behavior: A Meta-Analysis 
# of Experimental Studies (https://doi.org/10.1002/ece3.70655)

# Data and Code from the original article obtained in Jan 2025 from:
# https://github.com/sruckman/meta-analysis/tree/master


################################################################################
# Packages needed
################################################################################
library(metafor)

# cleaning up
rm(list = ls())

################################################################################
# Functions needed
################################################################################

# none

################################################################################
# simulations
################################################################################
# creating empty vectors
treatment.mean <- c()
treatment.sd <- c()
control.mean <- c()
control.sd <- c()

mean.RBIS <- c()
mean.RBIS.Ruckman <- c()
mean.RBIS.Ruckman.fixed <- c()

tvalues <- c()
t.test.dfs <- c()
t.test.RBIS <- c()
t.test.RBIS.esa <- c()

lm.tvalues.dfs <- c()
lm.tvalues <- c()
lm.t.test.RBIS <- c()
lm.t.test.RBIS.esa <- c()
lm.t.test.RBIS.Ruckman <- c()

lm.Fvalues.df1 <- c()
lm.Fvalues.df2 <- c()
lm.Fvalues <- c()
lm.Fvalues.r <- c()

aov.Fvalues.df1 <- c()
aov.Fvalues.df2 <- c()
aov.Fvalues <- c()
aov.Fvalues.SumSq.group <- c()
aov.Fvalues.SumSq.Residuals <- c()
aov.Fvalues.r.Ruckman <- c()
aov.Fvalues.r.Ruckman.group <- c()
aov.Fvalues.r.Ruckman.Residuals <- c()
aov.Fvalues.r.Ruckman.full <- c()


nsim <- 100 # number of experiments

set.seed(77)

for (i in 1:nsim){
  
  # comment/uncomment to change whether 10 or 70 "individuals" are the sample
  # size of each group
  sample.size.t <- 10
  sample.size.c <- 10
  # sample.size.t <- 70
  # sample.size.c <- 70
  
  treatment <- rnorm(n = sample.size.t,
                     mean = 2.40,
                     sd = 0.80)
  
  control <- rnorm(n = sample.size.c,
                   mean = 2.25,
                   sd = 0.75)
  
  treatment.mean <- c(treatment.mean,mean(treatment))
  treatment.sd <- c(treatment.sd,sd(treatment))
  
  control.mean <- c(control.mean,mean(control))
  control.sd <- c(control.sd,sd(control))
  
  # calculating effect sizes and extract them for later on
  
  mean.RBIS <- c(mean.RBIS,
                 escalc(measure="RBIS",
                        n1i = sample.size.t,
                        n2i = sample.size.c,
                        m1i = mean(treatment),
                        m2i = mean(control),
                        sd1i = sd(treatment),
                        sd2i = sd(control))$yi[1])
  
  # Ruckman calculation # 2 = treatment
  m <- sample.size.c+sample.size.t-2
  d <- (mean(treatment) - mean(control))/(sqrt(((sample.size.c-1)*sd(control)^2)+((sample.size.t-1)*sd(treatment)^2)/(m)))
  #d.fixed <- (mean(treatment) - mean(control))/(sqrt({((sample.size.c-1)*sd(control)^2)+((sample.size.t-1)*sd(treatment)^2)}/(m)))
  d.fixed <- (mean(treatment) - mean(control))/(sqrt((((sample.size.c-1)*sd(control)^2)+((sample.size.t-1)*sd(treatment)^2))/(m)))
  h <- (m/sample.size.c) + (m/sample.size.t)
  rpb <- d/sqrt((d^2)+h)
  rpb.fixed <- d.fixed/sqrt((d^2)+h)
  p <- sample.size.c/(sample.size.c+sample.size.t)
  #p <- sample.size.t/(sample.size.t+sample.size.c)
  mean.RBIS.Ruckman.i <- ((sqrt(p*(1-p)))/dnorm(qnorm(p)))*rpb
  mean.RBIS.Ruckman <- c(mean.RBIS.Ruckman,mean.RBIS.Ruckman.i)
  mean.RBIS.Ruckman.fixed.i <- ((sqrt(p*(1-p)))/dnorm(qnorm(p)))*rpb.fixed
  mean.RBIS.Ruckman.fixed <- c(mean.RBIS.Ruckman.fixed,mean.RBIS.Ruckman.fixed.i)
  
  # t-test
  tvalues.i <- t.test(treatment,control,var.equal=F)[[1]]
  tvalues <- c(tvalues,tvalues.i)
  t.test.dfs <- c(t.test.dfs,t.test(treatment,control,var.equal=F)[[2]])
  
  t.test.RBIS <- c(t.test.RBIS,escalc(measure="RBIS",
                                      n1i = sample.size.t,
                                      n2i = sample.size.c,
                                      ti = tvalues.i)$yi[1])
  
  # assuming equal n between groups
  t.test.RBIS.esa <- c(t.test.RBIS.esa,escalc(measure="RBIS",
                                              n1i = (sample.size.t+sample.size.c)/2,
                                              n2i = (sample.size.t+sample.size.c)/2,
                                              ti = tvalues.i)$yi[1])
  
  # simulating lm's
  data.i <- data.frame(group = c(rep("control",sample.size.c),rep("treatment",sample.size.t)),
                       y = as.numeric(c(control,treatment)))
  
  lm.model <- lm(y~group,data.i)
  lm.tvalues.dfs <- c(lm.tvalues.dfs,lm.model$df.residual)
  lm.tvalues.i <- summary(lm.model)[["coefficients"]][,"t value"][[2]]
  lm.tvalues <- c(lm.tvalues,lm.tvalues.i)
  lm.t.test.RBIS <- c(lm.t.test.RBIS,escalc(measure="RBIS",
                                            n1i = sample.size.t,
                                            n2i = sample.size.c,
                                            ti = lm.tvalues.i)$yi[1])
  # assuming equal n between groups
  lm.t.test.RBIS.esa <- c(lm.t.test.RBIS.esa,escalc(measure="RBIS",
                                                    n1i = (sample.size.t+sample.size.c)/2,
                                                    n2i = (sample.size.t+sample.size.c)/2,
                                                    ti = lm.tvalues.i)$yi[1])
  
  # Ruckman calculation
  rpb <- sqrt((lm.tvalues.i^2)/(lm.tvalues.i^2 + lm.model$df.residual))
  p <- (sample.size.t+sample.size.c)/((sample.size.t+sample.size.c)+(sample.size.t+sample.size.c))
  lm.t.test.RBIS.Ruckman.i <- ((sqrt(p*(1-p)))/dnorm(qnorm(p)))*rpb
  lm.t.test.RBIS.Ruckman <- c(lm.t.test.RBIS.Ruckman,lm.t.test.RBIS.Ruckman.i)
  
  # F-values
  lm.Fvalues.df1 <- c(lm.Fvalues.df1,summary(lm.model)$fstatistic[[2]])
  lm.Fvalues.df2 <- c(lm.Fvalues.df2,summary(lm.model)$fstatistic[[3]])
  lm.Fvalues <- c(lm.Fvalues,summary(lm.model)$fstatistic[[1]])
  
  #d.2 <- 2*sqrt(summary(lm.model)$fstatistic[[1]]/(sample.size.t+sample.size.c))
  d.2 <- sqrt((summary(lm.model)$fstatistic[[1]]*(sample.size.t+sample.size.c))/(sample.size.t*sample.size.c))
  #lm.Fvalues.RBIS <- d.2/sqrt((d.2^2)+(sample.size.t+sample.size.c)) #from Cooper et al. 2019 (there is also a correction for unequal sample sizes)
  a <- (((sample.size.t+sample.size.c)^2)/(sample.size.t*sample.size.c))
  lm.Fvalues.r.i <- d.2/sqrt((d.2^2)+a) #from Cooper et al. 2019 (there is also a correction for unequal sample sizes)
  lm.Fvalues.r <- c(lm.Fvalues.r,lm.Fvalues.r.i)
  
  
  ANOVA.Fvalues <- aov(y~group,data.i)
  aov.Fvalues.df1 <- c(aov.Fvalues.df1,summary(ANOVA.Fvalues)[[1]][["Df"]][1])
  aov.Fvalues.df2 <- c(aov.Fvalues.df2,summary(ANOVA.Fvalues)[[1]][["Df"]][2])
  aov.Fvalues <- c(aov.Fvalues,summary(ANOVA.Fvalues)[[1]][["F value"]][1])
  aov.Fvalues.SumSq.group <- c(aov.Fvalues.SumSq.group,summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][1])
  aov.Fvalues.SumSq.Residuals <- c(aov.Fvalues.SumSq.Residuals,summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][2])
  
  # Ruckman calculation
  SumSq <- summary(ANOVA.Fvalues)[[1]][["Df"]][1]*summary(ANOVA.Fvalues)[[1]][["F value"]][1]/summary(ANOVA.Fvalues)[[1]][["Df"]][2]
  aov.Fvalues.r.Ruckman.i <- SumSq/(1+SumSq)
  aov.Fvalues.r.Ruckman <- c(aov.Fvalues.r.Ruckman,aov.Fvalues.r.Ruckman.i)
  
  aov.Fvalues.r.SumSq.group.i <- summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][1]/(1+summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][1])
  aov.Fvalues.r.Ruckman.group <- c(aov.Fvalues.r.Ruckman.group,aov.Fvalues.r.SumSq.group.i)
  
  aov.Fvalues.r.SumSq.Residuals.i <- summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][2]/(1+summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][2])
  aov.Fvalues.r.Ruckman.Residuals <- c(aov.Fvalues.r.Ruckman.Residuals,aov.Fvalues.r.SumSq.Residuals.i)
  
  aov.Fvalues.r.SumSq.full.i <- (summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][1]+summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][2])/(1+(summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][1]+summary(ANOVA.Fvalues)[[1]][["Sum Sq"]][2]))
  aov.Fvalues.r.Ruckman.full <- c(aov.Fvalues.r.Ruckman.full,aov.Fvalues.r.SumSq.full.i)
}

# final full dataset for exploring different things, though we focused below in
# the disagreement between rbis calculations
final.data <- data.frame(treatment.mean=treatment.mean,
                         treatment.sd=treatment.sd,
                         treatment.n=sample.size.t,
                         control.mean=control.mean,
                         control.sd=control.sd,
                         control.n=sample.size.c,
                         mean.RBIS=mean.RBIS,
                         mean.RBIS.Ruckman=mean.RBIS.Ruckman,
                         mean.RBIS.Ruckman.fixed=mean.RBIS.Ruckman.fixed,
                         tvalues=tvalues,
                         t.test.dfs=t.test.dfs,
                         t.test.RBIS=t.test.RBIS,
                         t.test.RBIS.esa=t.test.RBIS.esa,
                         lm.tvalues=lm.tvalues,
                         lm.tvalues.dfs=lm.tvalues.dfs,
                         lm.t.test.RBIS=lm.t.test.RBIS,
                         lm.t.test.RBIS.esa=lm.t.test.RBIS.esa,
                         lm.t.test.RBIS.Ruckman=lm.t.test.RBIS.Ruckman,
                         lm.Fvalues=lm.Fvalues,
                         lm.Fvalues.df1=lm.Fvalues.df1,
                         lm.Fvalues.df2=lm.Fvalues.df2,
                         lm.Fvalues.r=lm.Fvalues.r,
                         aov.Fvalues.df1=aov.Fvalues.df1,
                         aov.Fvalues.df2=aov.Fvalues.df2,
                         aov.Fvalues=aov.Fvalues,
                         aov.Fvalues.SumSq.group=aov.Fvalues.SumSq.group,
                         aov.Fvalues.SumSq.Residuals=aov.Fvalues.SumSq.Residuals,
                         aov.Fvalues.r.Ruckman=aov.Fvalues.r.Ruckman,
                         aov.Fvalues.r.Ruckman.group=aov.Fvalues.r.Ruckman.group,
                         aov.Fvalues.r.Ruckman.Residuals=aov.Fvalues.r.Ruckman.Residuals,
                         aov.Fvalues.r.Ruckman.full=aov.Fvalues.r.Ruckman.full)



#final.data

pairs(labels = c("escalc(RBIS)","Ruckman RBIS","Ruckman RBIS (fixed)"),
      final.data[,c("mean.RBIS",
                    "mean.RBIS.Ruckman",
                    "mean.RBIS.Ruckman.fixed")])


summary(final.data)
summary(final.data[,c("mean.RBIS", 
                      "mean.RBIS.Ruckman", 
                      "mean.RBIS.Ruckman.fixed")])

# some correlations to explore agreement
# r
round(cor(final.data$mean.RBIS.Ruckman,final.data$mean.RBIS.Ruckman.fixed),3) # 10: 0.989, 70: 0.996
round(cor(final.data$mean.RBIS.Ruckman,final.data$mean.RBIS),3) # 10: 0.987, 70: 0.995
round(cor(final.data$mean.RBIS,final.data$mean.RBIS.Ruckman.fixed),3) # 10: 0.999, 70: 1.000

# R2
round(cor(final.data$mean.RBIS.Ruckman,final.data$mean.RBIS.Ruckman.fixed)^2,3) # 10: 0.978, 70: 0.992
round(cor(final.data$mean.RBIS.Ruckman,final.data$mean.RBIS)^2,3) # 10: 0.974, 70: 0.990
round(cor(final.data$mean.RBIS,final.data$mean.RBIS.Ruckman.fixed)^2,3) # 10: 0.998, 70: 1.000

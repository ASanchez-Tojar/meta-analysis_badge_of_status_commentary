################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code
#
# Pietro D'Amelio (pie.damelio@googlemail.com): revised & adjusted the code

# Script first created in Jan 2025 and sequentially revised until Jan 2026

################################################################################
# Description of script and Instructions
################################################################################

# This script is to re-calculate the effect sizes from the meta-analytic raw
# dataset from:

# Assessing the Association Between Animal Color and Behavior: A Meta-Analysis 
# of Experimental Studies (https://doi.org/10.1002/ece3.70655)

# Data and Code from the original article obtained in Jan 2025 from:
# https://github.com/sruckman/meta-analysis/tree/master


################################################################################
# Packages needed
################################################################################
# install.packages("pacman")
pacman::p_load(metafor,
               ggstatsplot,
               dplyr,
               ggplot2)

# cleaning up
rm(list = ls())


################################################################################
# Functions needed
################################################################################

# none

################################################################################
# Data
################################################################################

#Effect Sizes will be in rho and Fisher Z values
metadat <- read.csv("data/original/meta_data.csv", header=T)
summary(metadat)

# generating the subset for data exploration: 103 observations
subset.inferential.statistics <- metadat[metadat$Stat.Test%in%c("t","F","X2"),
                                         c("Authors","Study","Stat.Test",
                                           "df1","df2","Sample.Size")]

# # exporting the subset for visually exploring the data
# write.csv(subset.inferential.statistics, 'data_exploration/subset_inferential_statistics.csv',
#           row.names = F)


################################################################################
# BISERIAL CORRELATIONS
################################################################################

# Calculating the biserial correlation coefficients and their sampling variance
# for the corresponding subset containing means, SDs, and sample sizes
db.biserial <- metadat[metadat$Stat.Test=="mean",]
#db.biserial
nrow(db.biserial) # 42 effect sizes
summary(db.biserial)

# caculating biserial correlations (rbis) using escalc() from the metafor package
# see ?escalc() and Jacobs & Viechtbauer 2017: https://doi.org/10.1002/jrsm.1218
db.biserial <- as.data.frame(escalc(measure = "RBIS",
                                    n2i = n1,
                                    n1i = n2,
                                    m2i = mean1,
                                    m1i = mean2,
                                    sd2i = sd1,
                                    sd1i = sd2,
                                    data = db.biserial))
# exploring data
summary(db.biserial)

# # visually exploring the effect size. rbis > 0 when mean2 > mean1
# db.biserial[,c("mean1","mean2","n1","n2","Sample.Size","yi","vi")]

# exploring cases where n1 + n2 is different to Sample.Size
db.biserial[(db.biserial$n1 + db.biserial$n2 != db.biserial$Sample.Size),c("n1","n2","Sample.Size")]

# # some additional checks
# plot(db.biserial$Sample.Size,db.biserial$vi)
# cor(db.biserial$Sample.Size,db.biserial$vi)
# plot(db.biserial$yi,db.biserial$vi)
# cor(db.biserial$yi,db.biserial$vi)

# what is the percentage of negative rbis? 40.5%
round((table(db.biserial$yi<0)/nrow(db.biserial))*100,1)


################################################################################
# Converting t-values TO r
################################################################################

# converting t-values into Pearson's r for the corresponding subset
db.t.test <- metadat[metadat$Stat.Test=="t",]
#db.t.test
nrow(db.t.test) # 35 effect sizes
summary(db.t.test)

# quick exploration
# 2/3 of p-values are statistically significant: 23/(12+23) = 66%
table(db.t.test$p.value<0.05)

# few t-values are negative
table(db.t.test$Test.Statistic<0)

# no dfs is larger than its corresponding sample size: all good
table(db.t.test$df1>db.t.test$Sample.Size)

# For converting t-values to r, we are assuming, following the authors, that 
# the t-values come from independent t tests (even though this is unlikely the 
# case)
db.t.test <- as.data.frame(escalc(measure = "COR",
                                  ti = Test.Statistic,
                                  ni = Sample.Size,
                                  data = db.t.test))
# exploring data
summary(db.t.test)

# visually exploring the effect size. Some df1 are much smaller than Sample.size
db.t.test[,c("Test.Statistic","Stat.Test","Sample.Size","df1","yi","vi")]

# # some additional checks
# plot(db.t.test$df1,db.t.test$vi)
# cor(db.t.test$df1,db.t.test$vi)
# plot(db.t.test$Sample.Size,db.t.test$vi)
# cor(db.t.test$Sample.Size,db.t.test$vi)
# plot(db.t.test$yi,db.t.test$vi)
# cor(db.t.test$yi,db.t.test$vi)

# what is the percentage of negative t-to-r? 14.3%
round((table(db.t.test$yi<0)/nrow(db.t.test))*100,1)


################################################################################
# Converting X2-values TO r
################################################################################

# converting X2-values into Pearson's r for the corresponding subset
db.chi <- metadat[metadat$Stat.Test=="X2",]
# db.chi
nrow(db.chi) # 35 effect sizes
summary(db.chi)

# Around half of p-values are statistically significant (two reported simply as NS)
table(db.chi$p.value<0.05)
db.chi$p.value

# there are no negative chi-squares, which is what should be expected (but...)
table(db.chi$Test.Statistic<0)

# no dfs is larger than its corresponding sample size: all good
table(db.chi$df1>db.chi$Sample.Size)

# If we use a basic transformation from X2-values to r, which assumes that all 
# X2-values come from a 2x2 table (which is not the case), we obtain
db.chi$yi.wrong <- sqrt((db.chi$Test.Statistic)/
                          (db.chi$Sample.Size))


# From df1 it seems that the X2 not only come from 2x2 contingency tables but
# also from larger tables, including 2 x 3 (df = 2), 3 x 3 (df = 4; equivalent 
# to a 2 x 5 too) and even 3 x 4 (df = 6; equivalent to a 2 x 7 too). These
# different designs should be accounted for if we wanted to use Cramer’s V
# conversion formula, where k = number of levels of the variable with the least 
# number of levels (e.g., a contingency table, would be equal to 3).
# see: https://matthewbjane.quarto.pub/Categorical-Proportional-Data.html
# if df = 1 then k = 2, if df = 2 then k = 2, if df = 6 then k = 3
# Here as an example of how to go about using Cramer’s V
db.chi$k <- ifelse(db.chi$df1==6,3,2)

# accounting for the differently sized contingency tables
db.chi$yi <- sqrt((db.chi$Test.Statistic)/
                    (db.chi$Sample.Size*(db.chi$k-1)))

db.chi$vi <- ((1 - (db.chi$yi ^ 2)) ^ 2)/(db.chi$Sample.Size - 1)

# exploring data
summary(db.chi)

# visually exploring the effect size.
db.chi[,c("Test.Statistic","Stat.Test","Sample.Size","df1","yi","vi")]
db.chi[order(db.chi$yi),
       c("Study","Test.Statistic","Stat.Test","Sample.Size","df1","yi","vi")]
db.chi[db.chi$df1==6,]
db.chi[order(db.chi$Study),
       c("Study","Test.Statistic","Stat.Test","Sample.Size","df1","yi","vi")]

# # some additional checks
# plot(db.chi$Sample.Size,db.chi$vi)
# cor(db.chi$Sample.Size,db.chi$vi)
# plot(db.chi$Test.Statistic,db.chi$df1)
# cor(db.chi$Test.Statistic,db.chi$df1)

# what is the percentage of negative X2-to-r? 0%, which makes sense since X2
# values cannot be negative but it is a bit suspicious that out of 35 values
# they all corresponded to studies supporting the original hypothesis. This is 
# unlikely given the corresponding percentages found for rbis and t-to-r, and
# it suggests that effect size direction adjustments may not have been performed
# potentially leading to an overestimating of the overall effect
round((table(db.chi$yi<0)/nrow(db.chi))*100,1)


################################################################################
# Converting F-values TO r
################################################################################

# Subsetting by selection metadat$Stat.Test=="F" ignores that there is an 
# F-value in this dataset that has an r-value. Although we should priorities r,
# we are following the subsetting strategy used by the authors
db.F.test <- metadat[metadat$Stat.Test=="F",]
#db.F.test <- metadat[metadat$Stat.Test=="F" & (is.na(metadat$r)),]
#db.F.test
nrow(db.F.test) # 33 effect sizes
summary(db.F.test)


# Around half of p-values are statistically significant (three as ">0.05")
table(db.F.test$p.value<0.05)
db.F.test$p.value

# Though F-values can't be negative, there are two negative F-values. By checking
# the original studies, it seems that these 2 signs were seemingly added to
# reflect that those two effect sizes should indeed reflect a negative association
# This indicates that the authors were aware of the importance of accounting for
# the direction of the effects sizes, although we suspect that this may not have 
# been done consistently throughout the database
table(db.F.test$Test.Statistic<0)

# there is 1 df larger than the sample size, which is suspicious
table(db.F.test$df2>db.F.test$Sample.Size)

# The following exploration was performed in the .qmd file we generated (004_):
# db.F.test[db.F.test$df2>db.F.test$Sample.Size,]
# difference is massive: 53 vs 18, the reason is that the F-value comes from
# a model comparing 6 groups of 9 females each, but the only comparison of
# interest here is subordinate+male present vs dominant+male present. Since
# 18 rather than 54 is used as the sample size when calculating *Vr*, there is 
# no action required for this study.

# the authors used the following equation, which does not 'complain' about 
# negative F values. We will use this for the time being because our equation of
# choice would, of course, not accept negative F-values, as they aren't possible

# first, extracting information about what F-values are negative, for later use
db.F.test$sign <- ifelse(db.F.test$Test.Statistic<0,-1,1)

# running equation used originall by the authors
db.F.test$yi.wrong <- (db.F.test$df1*db.F.test$Test.Statistic/(db.F.test$df2))/(1+db.F.test$df1*db.F.test$Test.Statistic/(db.F.test$df2))

# we chose to use the approach suggested by Ben-Shachar et al:
# what about using F_to_eta2() function from the 'effectsize' R package?
# (Ben-Shachar, Lüdecke, and Makowski 2020): https://matthewbjane.quarto.pub/Effect-Sizes-for-ANOVAs.html
db.F.test$yi <- (db.F.test$df1*db.F.test$Test.Statistic)/(db.F.test$df1*db.F.test$Test.Statistic+db.F.test$df2)
db.F.test$yi <- sqrt(abs(db.F.test$yi))
db.F.test$yi <- db.F.test$yi * db.F.test$sign #adjusting the sing accordingly

# alternatively one can use the function from effectsize R package, which is 
# the same
# library(effectsize)
# F_to_eta2(f = db.F.test$Test.Statistic,
#           df = db.F.test$df1,
#           df_error = db.F.test$df2,
#           alternative = 'two.sided')

db.F.test$vi <- ((1 - (db.F.test$yi ^ 2)) ^ 2)/(db.F.test$Sample.Size - 1)
summary(db.F.test)

# checking the F-value with an associated r-value to see if they agree, which
# they do not: r = 0.33 vs r-from-F = 0.63
db.F.test[db.F.test$r!=0 & !(is.na(db.F.test$r)),]

# what is the percentage of negative F-to-r? 6.1%
round((table(db.F.test$yi<0)/nrow(db.F.test))*100,1)


################################################################################
# Pearson's r
################################################################################

# Pearson's r can be used directly
db.r <- metadat[metadat$Stat.Test=="r",]
# db.r
nrow(db.r) # 25 effect sizes

# over half the values are negative, which seems slightly surprising given 
# that for the other effect sizes, most results were positive. This further
# suggests that signs were not properly taken care of for the other effect sizes
# what is the percentage of negative r? 56%
round((table(db.r$r<0)/nrow(db.r))*100,1)

# Pearson's r can be used directly 
db.r$yi <- db.r$r

# typical vi for r
db.r$vi <- ((1 - (db.r$yi ^ 2)) ^ 2)/(db.r$Sample.Size - 1)


################################################################################
# Building full dataset
################################################################################

# full dataset, excluding variables not needed for the subsequent analyses
metadata.full <- rbind(db.biserial,
                       db.t.test,
                       subset(db.chi, select=-c(yi.wrong,k)),
                       subset(db.F.test, select=-c(yi.wrong,sign)),
                       db.r)

nrow(metadata.full)
summary(metadata.full)


################################################################################
# Exploring effect size type disagreements
################################################################################

# first calculate percentage of positive values for each type of effect size
effect.size.positive.perc <- metadata.full %>% 
  filter(Classification!="pteridine") %>% #excluding it here already because the authors decided to exclude from the final analyses, see script 003
  group_by(Stat.Test) %>% 
  mutate(Stat.Test = factor(Stat.Test, 
                            levels = c("r","mean","t","F","X2"))) %>%
  mutate(Stat.Test = recode(Stat.Test, r = "Pearson's r",
                            mean = "mean, SD, N",
                            t = "t value",
                            F = "F value",
                            X2 = "X2 value")) %>% 
  summarise(Percentage = 100*table(yi<0)[1]/n())# %>% 

effect.size.positive.perc

########################################
# plotting effect size magnitude

# generating the data subset
metadata.full.yi <- metadata.full %>%
  filter(Classification!="pteridine") %>% #excluding it here already because the authors decided to exclude from the final analyses, see script 003
  select(c(yi,vi,Stat.Test)) %>%
  mutate(Stat.Test = factor(Stat.Test, 
                            levels = c("r","mean","t","F","X2"))) %>%
  mutate(Stat.Test = recode(Stat.Test, r = "Pearson's r",
                            mean = "mean, SD, N",
                            t = "t value",
                            F = "F value",
                            X2 = "X2 value"))

# generating label for annotation
effect.size.positive.perc$label.perc <- paste0(round(effect.size.positive.perc$Percentage,0),
                                               "%\npositive")


# effect size magnitude # more at: https://indrajeetpatil.github.io/ggstatsplot/reference/ggbetweenstats.html
set.seed(77)
yi.plot <- ggbetweenstats(
  data  = metadata.full.yi,
  x     = Stat.Test,
  y     = yi,
  point.args = list(position = ggplot2::position_jitterdodge(dodge.width = 0.6),
                    alpha = 0.4, 
                    size = 1/sqrt(metadata.full.yi$vi)-min(1/sqrt(metadata.full.yi$vi))+0.1,
                    #size = asinh(1/sqrt(metadata.full.yi$vi)-min(1/sqrt(metadata.full.yi$vi))+0.1)+1, 
                    stroke = 0, na.rm = TRUE),
  #point.args = list(size = 1),
  type = "parametric",
  pairwise.display = "none",
  #p.adjust.method = "none", # if no multiple correction used, differences are everywhere
  #ggsignif.args = list(textsize = 3, tip_length = 0.02, na.rm = TRUE), # if pairwise.display on, change size
  bf.message = F,
  effsize.type = "eta", # which corresponds to the partial eta squared we are using to transform F-to-r
  #results.subtitle = F, # to remove statistical results from the top of the plot
  centrality.label.args = list(size = 4, nudge_x = 0.4, 
                               segment.linetype = 3,
                               min.segment.length = 0),
  xlab = "\nEffect size origin\n",
  ylab = "\nEffect size\n(r and rbis)",
  title = "\nDoes effect size magnitude differ considerably depending on their origin?"
) 

yi.plot <- yi.plot + 
  # modifying text size
  theme(axis.text=element_text(size=11), 
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=15)) +
  # adding the percentage of positive effect sizes for each type
  annotate("text", 
           x = seq(0.7,4.7,1), 
           y = 1.15, 
           label = effect.size.positive.perc$label.perc) +
  # adding grey area to better signal postive vs negative values
  annotate("rect", xmin = 0, xmax = 6, ymin = -1, ymax = 0,
           alpha = .1)

yi.plot # this suggest potential issues further explored down the line


################################################################################
# Saving dataset
################################################################################

# processed dataset for re-analysis
write.csv(metadata.full, "data/new/meta_complete_data2_new.csv")

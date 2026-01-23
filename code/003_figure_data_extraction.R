################################################################################
# Authors: 
#
# Alfredo Sanchez-Tojar (alfredo.tojar@gmail.com): original code
#
# Pietro D'Amelio (pie.damelio@googlemail.com): revised & adjusted the code

# Script first created in Jan 2025 and sequentially revised until Jan 2026

#######################################################################

# This script is to extract data from an article included in:

# Assessing the Association Between Animal Color and Behavior: A Meta-Analysis 
# of Experimental Studies (https://doi.org/10.1002/ece3.70655)

# Data and Code from the original article obtained from:
# https://github.com/sruckman/meta-analysis/tree/master

# The extraction is performed from one figure and the text, 3 effect sizes in
# total, which are then included section Carola et al. 2024 in script:
# 004_data_extraction_validation.qmd


################################################################################
# Packages needed
################################################################################

library(metaDigitise)
library(metafor)

################################################################################
# Extracting data from figure
################################################################################

data <- metaDigitise(dir = "data_extraction_figures/")
data

# and calculating rbis

escalc(measure="RBIS",
       n1i = 10,
       n2i = 10,
       m1i = 35.7444205,
       m2i = 22.9487664,
       sd1i = 9.7852693,
       sd2i = 10.5786696)

################################################################################
# Additional effect sizes
################################################################################

escalc(measure="RBIS",
       n1i = 9,
       n2i = 9,
       m1i = 2.874692,
       m2i = 1.825617,
       sd1i = 1.1140136,
       sd2i = 0.4564103)

escalc(measure="RBIS",
       n1i = 10,
       n2i = 10,
       m1i = 0.6027107,
       m2i = 3.1707548,
       sd1i = 2.5326098,
       sd2i = 2.4224963)

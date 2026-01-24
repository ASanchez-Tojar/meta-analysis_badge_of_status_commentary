# Short title: The jury is still out about the badge of status hypothesis
## Title: Fifty years later, and we still don’t know about badges of status 
**Contributors:** Alfredo Sánchez-Tójar, Pietro B. D'Amelio

**Date created:** January 2026

**Identifer:** DOI (TBA)

**Category:** Project

**Code License:** [MIT](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/tree/main?tab=MIT-1-ov-file)

**Data License:** See the original licence assigned by the generators of the meta-analytic dataset used at Ruckman et al. (2024: https://doi.org/10.5061/dryad.9kd51c5tk).

**Description:** This repository contains the data, code and other materials used in the following study:

---

Alfredo Sánchez-Tójar, Pietro B. D'Amelio. 2026. **Fifty years later, and we still don’t know about badges of status**.

---

This study is a commentary on Ruckman et al. (2024, Ecology & Evolution: https://doi.org/10.1002/ece3.70655). The repository consists of an R project with 6 scripts and 5 folders containing the data either needed to run these scripts or created by these scripts, including the figures. To run this repository, first download and unzip the entire repository and open the Rproject file: [meta-analysis_badge_of_status_commentary.Rproj](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/blob/main/meta-analysis_badge_of_status_commentary.Rproj). The scripts are named in the order that they should be run, from 001 to 006. For any further information about this repository, please contact: [Alfredo Sánchez-Tójar](https://scholar.google.co.uk/citations?hl=en&user=Sh-Rjq8AAAAJ&view_op=list_works&sortby=pubdate), email: alfredo.tojar@gmail.com. 


## Information about folders and files within:

Folders:
-	[code](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/tree/main/code): contains all six scripts used to process, simulate,  data, extract and re-extract data, and reproduce the re-analyses.

- [data](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/tree/main/data): contains the original data files publicly shared by Ruckman et al. (2014) [original](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/tree/main/data/original) and the two [new](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/tree/main/data/new) data files generated as part of our re-analysis.

- [data_extraction_figures](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/tree/main/data_extraction_figures): contains the figures that were used to extrac data using metaDigitise.

- [new_figures](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/tree/main/new_figures): contains all figures reported in the main text and the supplementary figures

- [new_taxonomy](https://github.com/ASanchez-Tojar/meta-analysis_badge_of_status_commentary/tree/main/new_taxonomy): contains the taxonomic and phylogenetic information generated and necessary for running the phylogenetic multilevel models.

## Data dictionary
Please, see the original data dictionary in the repository by Ruckman et al. (2024): https://doi.org/10.5061/dryad.9kd51c5tk

## Software and {Packages} used

R Session Information

R version: 4.3.1 (2023-06-16 ucrt)

Platform: x86_64-w64-mingw32/x64 (64-bit)

Operating system: Windows 10 x64 (build 19044)

Attached base packages
- grid
- stats
- graphics
- grDevices
- utils
- datasets
- methods
- base

Other attached packages
- bayestestR (0.15.0)
- emmeans (1.10.6)
- TreeTools (1.13.0)
- phytools (1.9-16)
- maps (3.4.1.1)
- MCMCglmm (2.35)
- coda (0.19-4)
- ape (5.7-1)
- gridExtra (2.3)
- orchaRd (2.0)
- knitr (1.49)
- metaDigitise (1.0.1)
- ggplot2 (3.5.1)
- dplyr (1.1.4)
- ggstatsplot (0.12.5)
- metafor (4.6-0)
- numDeriv (2016.8-1.1)
- metadat (1.2-0)
- Matrix (1.6-1)

Loaded via a namespace (not attached)
- tidyselect (1.2.1)
- farver (2.1.2)
- bitops (1.0-7)
- R.utils (2.12.2)
- statsExpressions (1.6.1)
- RCurl (1.98-1.12)
- optimParallel (1.0-2)
- combinat (0.0-8)
- TH.data (1.1-3)
- tensorA (0.36.2)
- pacman (0.5.1)
- mathjaxr (1.6-0)
- PlotTools (0.3.1)
- digest (0.6.35)
- estimability (1.5.1)
- lifecycle (1.0.4)
- survival (3.5-5)
- magrittr (2.0.3)
- compiler (4.3.1)
- rlang (1.1.4)
- tools (4.3.1)
- igraph (2.1.4)
- plotrix (3.8-2)
- phangorn (2.12.1)
- clusterGeneration (1.3.8)
- bit (4.5.0.1)
- mnormt (2.1.1)
- scatterplot3d (0.3-44)
- multcomp (1.4-28)
- R.cache (0.16.0)
- expm (0.999-7)
- withr (3.0.2)
- purrr (1.2.1)
- R.oo (1.25.0)
- datawizard (1.2.0)
- xtable (1.8-4)
- colorspace (2.1-0)
- paletteer (1.6.0)
- scales (1.3.0)
- iterators (1.0.14)
- MASS (7.3-60)
- zeallot (0.1.0)
- insight (1.4.2)
- cli (3.6.1)
- mvtnorm (1.3-2)
- generics (0.1.3)
- rstudioapi (0.17.1)
- parameters (0.23.0)
- splines (4.3.1)
- parallel (4.3.1)
- effectsize (0.8.9)
- vctrs (0.6.5)
- sandwich (3.1-0)
- patchwork (1.3.0)
- bit64 (4.6.0-1)
- correlation (0.8.6)
- foreach (1.5.2)
- glue (1.8.0)
- rematch2 (2.1.2)
- codetools (0.2-19)
- cubature (2.1.0)
- gtable (0.3.6)
- quadprog (1.5-8)
- munsell (0.5.1)
- tibble (3.2.1)
- pillar (1.10.1)
- R6 (2.6.1)
- Rdpack (2.6)
- doParallel (1.0.17)
- evaluate (1.0.3)
- lattice (0.21-8)
- rbibutils (2.2.16)
- R.methodsS3 (1.8.2)
- corpcor (1.6.10)
- Rcpp (1.1.0)
- fastmatch (1.1-4)
- nlme (3.1-162)
- xfun (0.49)
- zoo (1.8-14)
- pkgconfig (2.0.3)

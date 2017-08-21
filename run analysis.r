##
## Code to reproduce the analysis in Thomas et al. "The narrow gap between norms and cooperative behaviour in a reindeer herding community"
##
## Author: Matthew Gwynfryn Thomas (@matthewgthomas)
##
## Requires the following packages:
## - tidyverse
## - readxl
## - data.table
## - igraph
## - proxy
## - ggplot2
## - amen
## - bayesplot
## - rstanarm
## - loo (currently the development version, so can run model_weights())
## - gridExtra
## - broom
## - reshape2
## - rmarkdown
## - futile.logger
##
# set up log file
library(futile.logger)
flog.appender(appender.file(paste0("analysis - ", format(Sys.time(), "%Y-%m-%d %H_%M_%S"), ".log")))  # create timestamped log file

# this data preparation step was run on the raw data to create processed data loaded by 'init.r' and analysed below
# (raw data are not available as part of this project for reasons of confidentiality)
#source("data prep.r")  # see the data coding and transformations doc for an explanation of this step

source("init.r")
source("init plots.r")

source("plot gift distribution.r")

source("social network analysis - reciprocity.r")
source("social network analysis - degree and gifts.r")
source("social network analysis - similarity.r")

source("social relations model - fit.r")      # (this takes a bit of time to fit all the models)
source("social relations model - compare.r")
source("social relations model - analyse.r")

rmarkdown::render("results text.r", "word_document", output_dir=results.dir)  # this produces the in-text statistics

flog.info("Finished analysis!")

##
## A kind of meta-loader to import data if not already in the workspace
## This lets you run each part of the analysis separate rather than all in a batch, should you prefer
##
if (!exists("herders"))
  source("init.r")

if (!exists("common_theme"))
  source("init plots.r")

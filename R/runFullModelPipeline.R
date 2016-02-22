# TODO: Add comment
# 
# Author: benedikt
###############################################################################


# input
source("createInputFile.R")
source("createInputFile_NVAO.R")

# feature generation
# IR
source("createQueryData.R")
source("createQueryFeatures.R")
# PMI
source("createPMIFeatures.R")
source("createPMIFeaturesNVAO.R")
# FeatureHashing
source("createHashFeatures.R")

# model prediction
source("runModel.R")


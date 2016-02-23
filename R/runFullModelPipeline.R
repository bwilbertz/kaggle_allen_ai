# 
# Author: Benedikt Wilbertz
###############################################################################


# input
source("createInputFile.R")
source("createInputFile_NVAO.R")

# feature generation
source("createBasicStatFeatures.R")
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


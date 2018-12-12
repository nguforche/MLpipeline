classifier.name <- function(method = "glm"){
mod <- c("glm", "rf", "gbm", "svmRadial", "glmnet", "pda", "xgbDART", "nnet", "C5.0")
fun <- c("glm", "randomForest", "gbm", "ksvm", "glmnet", "fda", "xgb.train", "nnet", "C5.0")
names(fun) <- mod
fun[method] 
}


Model <- function(x){
switch(x,
glm = {extra.para <- GLM()},
rf = {extra.para <- RF()},
gbm = {extra.para <- GBM()},
svmRadial = {extra.para <- SVM()},
glmnet = {extra.para <- GLMNET()},
nnet = {extra.para <- NNET()}, 
pda = {extra.para <- PDA()},
C5.0 = {extra.para <- C50()},
xgbDART = {xtra.para <- XGBoost()}, 
stop(paste0(x, " is not yet implemeted")) 
) 
}


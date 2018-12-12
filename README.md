# MLpipeline: Machine Learning pipeline
 A framework for constructing several machine learning supervised learning models from raw data, evaluate

# Exampple  

suppressMessages(require(MLpipeline))
suppressMessages(require(plyr))
suppressMessages(require(caret))
suppressMessages(require(PresenceAbsence))

document(Rcode)

dat <- SynData(sim="simple")
dat$class <- factor(ifelse(dat$class == 1, "Yes", "No"))

form <- as.formula(paste0("class ~ ", paste0(setdiff(names(dat), "class"), collapse = "+"))) 

mod <- trainMLpipeline(form=form, data=dat, method = c("glm", "rf", "gbm", "svmRadial", "nnet"), importance=TRUE)

tab <- do.call(rbind, lapply(c("glm", "rf", "gbm", "svmRadial", "nnet"), function(xx) {
perf <- Performance(object=mod, method= xx, positive.class = "Yes")
perf$Resample <- NULL 
cbind(mdel = xx, getSummary(perf,  groups = NULL,  alpha = 0.05))
}))
tab 



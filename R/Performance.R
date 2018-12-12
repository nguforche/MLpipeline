#' @title Performance metrics  
#
#' @description This function computes several performance metrics from the trained ML pipeline object 
#' and optionally represent the results in a publication ready tabular format.  
#' @param oject oject of class trainPipeline 
#' @param method  classifier name
#' @param cost   costs/benefits matrix  see \code{evThreshold}
#' @importFrom plyr ddply 
#' @export 
Performance <- function(object, method="glm", positive.class = "Yes", cost = NULL){
#pred obs    No   Yes rowIndex mtry   Resample

pred <- object[[method]]$pred
pred$Resample <- factor(pred$Resample)

pred <- na.omit(pred[, c("obs",positive.class, "Resample")])
  
perf <- ddply(pred, .variables = "Resample", .fun = function(xx){
pp <- abs(as.numeric(xx[, positive.class]))
obs <- ifelse(xx[, "obs"] == positive.class, 1, 0)

if(is.null(cost))
thresh <- as.numeric(opt.thresh(prob=pp, obs=obs, opt.methods = 9))
else 
thresh <- cost.thresh(prob= pp, obs=obs, cost = cost)$best.threshold 

Performance.measures(obs = obs, pred = as.numeric(pp), threshold = thresh)
})

}


#' @export
Performance.risk.score <- function(object, data, method="glm", positive.class = "Yes", risk.score, cost = NULL){
#pred obs    No   Yes rowIndex mtry   Resample

pred <- object[[method]]$pred
pred$Resample <- factor(pred$Resample)


pred <- na.omit(pred[, c("obs",positive.class, "rowIndex", "Resample")])
  
perf <- ddply(pred, .variables = "Resample", .fun = function(xx){
pp <- abs(as.numeric(xx[, positive.class]))
obs <- ifelse(xx[, "obs"] == positive.class, 1, 0)

ix <- xx$rowIndex
#rs <- normalize(as.numeric(data[ix, risk.score]))
rs <- as.numeric(data[ix, risk.score])
obs.rs <- factor(xx[ix, "obs"])
dl <- data.frame(y = obs.rs, x = rs)
mod <- glm(y ~ x, data = dl, family = binomial)
rs <- predict(mod, newdata = dl, type = "response")
if(is.null(cost)){
thresh <- as.numeric(opt.thresh(prob=pp, obs=obs, opt.methods = 9))
thresh.rs <- as.numeric(opt.thresh(prob=rs, obs=obs.rs, opt.methods = 9))

} else {
thresh <- cost.thresh(prob= pp, obs=obs, cost = cost)$best.threshold 
thresh.rs <- cost.thresh(prob= rs, obs=obs.rs, cost = cost)$best.threshold 
}
m1 <- cbind(model = method, Performance.measures(obs = obs, pred = pp, threshold = thresh))
m2 <- cbind(model = risk.score, Performance.measures(obs = obs, pred = rs, threshold = thresh.rs))
rbind(m1, m2) 
})

}




















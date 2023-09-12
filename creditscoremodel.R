load("~/Desktop/CreditRiskModelling/LCdata_1.RData")
ix <- sample(157085,102350,replace=FALSE)
cctrain<-D1[ix,]
cctest<-D1[-ix,]
glm1.out <- with(cctrain, glm(def_flag ~ loan_amnt + grade + emp_length_p + term + addr_state, family = binomial("logit")))
summary(glm1.out)
yp <- predict(glm1.out, cctest, type="link")
roc <- function(y, s){
yav <- rep(tapply(y, s, mean), table(s))
rocx <- cumsum(yav)
rocy <- cumsum(1 - yav)
area <- sum(yav * (rocy - 0.5 * (1 - yav)))
x1 <- c(0, rocx)/sum(y)
y1 <- c(0, rocy)/sum(1 - y)
auc <- area/(sum(y) * sum(1 - y))
print(auc)
plot(x1,y1,"l")
}
roc(cctest$def_flag, yp)

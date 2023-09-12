load("~/Desktop/CreditRiskModelling/LCdata_1.RData")
library(ggplot2)

set.seed(69)

#ggplot(D1, aes(x=loan_amnt, color=non_default)) +
#  geom_histogram(fill="white", bins=20) +
#  labs(title="Loan amount for defaulters and non-defaulters",x="Loan amount ($)", y = "Count")


woe.tab <- function(x,y) {
  n1 <- sum(y)
  n0 <- sum(1-y)
  nx0n1 <- tapply(1-y,x,sum)*n1
  nx1n0 <- tapply(y,x,sum) *n0
  nx0n1[which(nx0n1==0)]<-n1
  nx1n0[which(nx1n0==0)]<-n0
  return(log(nx0n1)-log(nx1n0))
}

woe.assign <- function(wtab, x) {
  w<-rep(0,length(x))
  ni<-names(wtab)
  for (i in 1:length(ni)) {
    w[which(x==ni[i])]<-wtab[i]
  }
  return(w)
}

D1$non_default = !D1$def_flag
D1$addr_state_woe <- woe.assign(woe.tab(D1$addr_state,D1$non_default), D1$addr_state)
D1$grade_woe <- woe.assign(woe.tab(D1$grade,D1$non_default), D1$grade)
D1$term_woe <- woe.assign(woe.tab(D1$term,D1$non_default), D1$term)
D1$log_loan_amnt <-log(D1$loan_amnt)
D1$log_annual_inc <- log(D1$annual_inc)
D1$emp_length_p[is.na(D1$emp_length_p)] <- 0
#REPLACING
D1$int_rate_log <- log(D1$int_rate)
D1$log_avg_cur_bal <- log(D1$avg_cur_bal+0.1)
D1$home_ownership_woe <- woe.assign(woe.tab(D1$home_ownership,D1$non_default), D1$home_ownership)
D1$purpose_p_woe <- woe.assign(woe.tab(D1$purpose_p,D1$non_default), D1$purpose_p)
D1$issue_d_woe <- woe.assign(woe.tab(D1$issue_d,D1$non_default), D1$issue_d)
D1$verification_status_woe <- woe.assign(woe.tab(D1$verification_status,D1$non_default), D1$verification_status)
D1$log_total_rev_hi_lim <- log(D1$total_rev_hi_lim+0.1)
  
D1 = subset(D1, select = -c(revol_bal,acc_now_delinq,chargeoff_within_12_mths,
                              delinq_amnt,initial_list_status,mo_sin_rcnt_rev_tl_op,num_actv_bc_tl,
                              num_actv_rev_tl,num_bc_sats,open_acc,total_acc,pub_rec,pub_rec_bankruptcies,
                            avg_cur_bal,home_ownership,purpose_p,issue_d,verification_status,total_rev_hi_lim))
D1 = subset(D1, select = -c(addr_state, grade, term, loan_amnt, annual_inc, def_flag, int_rate) )

D1 = subset(D1, select = -c(num_accts_ever_120_pd,mort_acc,grade_woe))

ix <- sample(157085,102350,replace=FALSE)
cctrain<-D1[ix,]
cctest<-D1[-ix,]

roc <- function(y, s){
  yav <- rep(tapply(y, s, mean), table(s))
  rocx <- cumsum(yav)
  rocy <- cumsum(1 - yav)
  area <- sum(yav * (rocy - 0.5 * (1 - yav)))
  x1 <- c(0, rocx)/sum(y)
  y1 <- c(0, rocy)/sum(1 - y)
  auc <- area/(sum(y) * sum(1 - y))
  print(auc)
  plot(x1,y1, xlab="False positive rate", ylab="True positive rate","l",main="ROC for training data")
}

glm2.out <- glm(non_default ~., data=cctrain, family = binomial("logit"))
yp1<-predict(glm2.out,cctest,type="link")
ytrainp1 <- predict(glm2.out,cctrain,type="link")
roc(cctest$non_default, yp1)
roc(cctrain$non_default, ytrainp1)

glm3.out <- glm(non_default ~. + log_annual_inc*verification_status_woe,data=cctrain, family = binomial("logit"))
yp2<-predict(glm3.out,cctest,type="link")
ytrainp2 <- predict(glm3.out,cctrain,type="link")
roc(cctest$non_default, yp2)
roc(cctrain$non_default, ytrainp2)



###SEGMENTED MODEL
load("~/Desktop/CreditRiskModelling/LCdata_1.RData")
D1$non_default = !D1$def_flag
D1$addr_state_woe <- woe.assign(woe.tab(D1$addr_state,D1$non_default), D1$addr_state)
D1$grade_woe <- woe.assign(woe.tab(D1$grade,D1$non_default), D1$grade)
D1$log_loan_amnt <-log(D1$loan_amnt)
D1$log_annual_inc <- log(D1$annual_inc)
D1$emp_length_p[is.na(D1$emp_length_p)] <- 0
#REPLACING
D1$int_rate_log <- log(D1$int_rate)
D1$log_avg_cur_bal <- log(D1$avg_cur_bal+0.1)
D1$home_ownership_woe <- woe.assign(woe.tab(D1$home_ownership,D1$non_default), D1$home_ownership)
D1$purpose_p_woe <- woe.assign(woe.tab(D1$purpose_p,D1$non_default), D1$purpose_p)
D1$issue_d_woe <- woe.assign(woe.tab(D1$issue_d,D1$non_default), D1$issue_d)
D1$verification_status_woe <- woe.assign(woe.tab(D1$verification_status,D1$non_default), D1$verification_status)
D1$log_total_rev_hi_lim <- log(D1$total_rev_hi_lim+0.1)

D1 = subset(D1, select = -c(
                            avg_cur_bal,home_ownership,purpose_p,issue_d,verification_status,total_rev_hi_lim))
D1 = subset(D1, select = -c(addr_state, grade, loan_amnt, annual_inc, def_flag, int_rate) )

cctrain<-D1[ix,]
cctest<-D1[-ix,]
longtermtrain <- subset(cctrain[cctrain$term == 60,], select=-term)
longtermtest <- subset(cctest[cctest$term == 60,], select=-term)
shorttermtrain <- subset(cctrain[cctrain$term == 36,], select=-term)
shorttermtest <- subset(cctest[cctest$term == 36,], select=-term)


glmlong.out <- glm(non_default ~., data=longtermtrain, family = binomial("logit"))

ylongtest<-predict(glmlong.out,longtermtest,type="link")
ylongtrain <- predict(glmlong.out,longtermtrain,type="link")
roc(longtermtest$non_default, ylongtest)
roc(longtermtrain$non_default, ylongtrain)

glmshort.out <- glm(non_default ~., data=shorttermtrain, family = binomial("logit"))
yshorttest<-predict(glmshort.out,shorttermtest,type="link")
yshorttrain <- predict(glmshort.out,shorttermtrain,type="link")
roc(shorttermtest$non_default, yshorttest)
roc(shorttermtrain$non_default, yshorttrain)


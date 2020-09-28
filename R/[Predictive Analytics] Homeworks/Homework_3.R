data<-read.csv("loan_data.csv")

data$AGE<-as.numeric(data$AGE)
data$TOT_LOAN<-as.numeric(data$TOT_LOAN)
data$TOT_LOAN_CRD<-as.numeric(data$TOT_LOAN_CRD)
data$LOAN_BNK<-as.numeric(data$LOAN_BNK)
data$LOAN_CPT<-as.numeric(data$LOAN_CPT)
data$LOAN_CRD_CNT<-as.numeric(data$LOAN_CRD_CNT)
data$CRDT_CNT<-as.numeric(data$CRDT_CNT)
data$GUARN_CNT<-as.numeric(data$GUARN_CNT)
data$INCOME<-as.numeric(data$INCOME)
data$LATE_RATE<-as.numeric(data$LATE_RATE)
data$LATE_RATE_1Y<-as.numeric(data$LATE_RATE_1Y)
data$CANCEL_CNT_1Y<-as.numeric(data$CANCEL_CNT_1Y)
data$TEL_COST_MON<-as.numeric(data$TEL_COST_MON)
data$MOBILE_PRICE<-as.numeric(data$MOBILE_PRICE)
data$SUSP_DAY<-as.numeric(data$SUSP_DAY)
data$LATE_TEL<-as.numeric(data$LATE_TEL)


#1 연속형 변수들 가지고 모델 세우기
#1) 표준화 안하고 그냥 하기
datadefault<-data[,1:18]
data1<-data.frame(datadefault,data$VARSEX,data$TARGET)
data1<-rename(data1,"TARGET"="data.TARGET","VARSEX"="data.VARSEX")
train1<-data1[1:30370,]
test1<-data1[30371:43386,]
model_1 <-glm(TARGET~ AGE+	TOT_LOAN+	TOT_LOAN_CRD+	LOAN_BNK+	LOAN_CPT+	CRDT_CNT+	GUARN_CNT+	INCOME+	LOAN_CRD_CNT+	LATE_RATE+	LATE_RATE_1Y+	INS_MON_MAX+	CANCEL_CNT_1Y+	CALL_TIME+	TEL_COST_MON+	MOBILE_PRICE+	SUSP_DAY+	LATE_TEL+VARSEX, data=train1,family="binomial")

summary(model_1)

#2) 표준화 하고 하기
datanorm<-scale(data[,1:18])

data2<-data.frame(datanorm,data$VARSEX,data$PAY_METHOD,data$JOB,data$ADJTARGET,data$TARGET)
data2 <- rename(data2,"TARGET"="data.TARGET", "ADJTARGET" = "data.ADJTARGET","JOB"="data.JOB","VARSEX"="data.VARSEX","PAY_METHOD"="data.PAY_METHOD")
train2<-data2[1:30370,]
test2<-data2[30371:43386,]


model_2 <-glm(TARGET~ AGE+	TOT_LOAN+	TOT_LOAN_CRD+	LOAN_BNK+	LOAN_CPT+	CRDT_CNT+	GUARN_CNT+	INCOME+	LOAN_CRD_CNT+	LATE_RATE+	LATE_RATE_1Y+	INS_MON_MAX+	CANCEL_CNT_1Y+	CALL_TIME+	TEL_COST_MON+	MOBILE_PRICE+	SUSP_DAY+	LATE_TEL+VARSEX, data=train2,family="binomial")
summary(model_1)
str(model_1)

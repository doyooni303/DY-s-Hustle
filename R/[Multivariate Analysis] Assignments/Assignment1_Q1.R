#Step1_Q1_데이터변환
install.packages("arules")
library(arules)

#csv파일 불러오기 및 변수 명 정의
mooc_dataset<-read.csv("big_student_clear_third_version.csv")
itemName<-mooc_dataset[,c("institute","course_id","final_cc_cname_DI","LoE_DI")]

TransactionID<-mooc_dataset$userid_DI
colnames(itemName)<-c("Institute","Course","Region","Degree")

# Region 변수 내 공백제거
itemName$Region<-gsub(" ","",itemName$Region)
itemName$Region

#RawTransactions으로 변수 저장
RawTransactions<-paste(itemName$Institute,itemName$Course,itemName$Region,itemName$Degree,sep="_")

# 변수연결
MOOC_transactions<-paste(TransactionID,RawTransactions,sep=" ")
MOOC_transactions

# csv파일로 저장
write(MOOC_transactions,file="MOOC_User_Course.csv")

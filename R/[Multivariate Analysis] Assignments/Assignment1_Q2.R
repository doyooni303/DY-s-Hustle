#step2_Q2_데이터불러오기 및 기초 통계량 확인
install.packages("arules")

install.packages("arulesViz")
install.packages("wordcloud")
library(arules)
library(arulesViz)
library(wordcloud)

#Q2_1
MOOC_transactions<-read.transactions("MOOC_User_Course.csv", format="single",sep = " ",cols=c(1,2), rm.duplicates = TRUE)
summary(MOOC_transactions)
#186373행, 137003열의 transactions 데이터 / 가장 많이 듣는 수강과목은 " MITx_6.00x_UnitedStates_Secondary",""


#Q2_2
itemName <- itemLabels(MOOC_transactions)
itemCount <- itemFrequency(MOOC_transactions)*nrow(MOOC_transactions)
col <- brewer.pal(9, "Blues")
wordcloud(words = itemName, freq = itemCount, min.freq = 1000, scale=c(1.5,0.8), col = col , random.order = FALSE)


#Q2_3
itemFrequencyPlot(MOOC_transactions, support = 0.01,topN = 5, main="Top 5")
#상위 5개의 수업의 접속국가는 미국과 인도다.

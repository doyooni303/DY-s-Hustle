#step2_Q2_�����ͺҷ����� �� ���� ��跮 Ȯ��
install.packages("arules")

install.packages("arulesViz")
install.packages("wordcloud")
library(arules)
library(arulesViz)
library(wordcloud)

#Q2_1
MOOC_transactions<-read.transactions("MOOC_User_Course.csv", format="single",sep = " ",cols=c(1,2), rm.duplicates = TRUE)
summary(MOOC_transactions)
#186373��, 137003���� transactions ������ / ���� ���� ��� ���������� " MITx_6.00x_UnitedStates_Secondary",""


#Q2_2
itemName <- itemLabels(MOOC_transactions)
itemCount <- itemFrequency(MOOC_transactions)*nrow(MOOC_transactions)
col <- brewer.pal(9, "Blues")
wordcloud(words = itemName, freq = itemCount, min.freq = 1000, scale=c(1.5,0.8), col = col , random.order = FALSE)


#Q2_3
itemFrequencyPlot(MOOC_transactions, support = 0.01,topN = 5, main="Top 5")
#���� 5���� ������ ���ӱ����� �̱��� �ε���.
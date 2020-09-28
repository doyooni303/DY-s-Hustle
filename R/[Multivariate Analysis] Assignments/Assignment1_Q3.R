#Step3_Q3_규칙생성 및 결과 해석

#Q3_1_규칙생성
#rules <- apriori(Groceries, parameter=list(support=0.01, confidence=0.35))

ecl <- eclat(MOOC_transactions, parameter=list(support=2/186373,minlen=1 ,maxlen=12))
ecl
inspect(sort(ecl)[1:100])

rules <- apriori(MOOC_transactions, parameter=list(support=0.005, confidence=0.01))
rules
#(0.001,0.001)=93 /(0.001,0.0025) =46 / (0.001,0.005)= 25 /(0.001,0.01)=18
#(0.0025,0.001)=36 /(0.0025,0.0025)=36 / (0.0025,0.005)=15 / (0.0025,0.01) = 8
#(0.005,0.001) = 11 / (0.005,0.0025)=11 / (0.005,0.005) =11 / (0.005,0.01) = 4

rulematrix<-matrix(c(93,46,25,18,36,46,15,8,11,11,11,4),nrow=3,ncol=4,byrow = T)
rulematrix
colnames(rulematrix)<-c("confidence = 0.001","confidence =  0.025","confidence =  0.005","confidence = 0.01")
rownames(rulematrix)<-c("support = 0.001","support = 0.0025","support = 0.005")

rulematrix

#Q3_2_
rules<-apriori(MOOC_transactions, parameter=list(support=0.001, confidence=0.05))
rules
summary(rules)
inspect(rules)
#support가 가장 높은 규칙 : {MITx_6.002x_India_Secondary} <=> {MITx_6.00x_India_Secondary} :  0.003321297 
#confidence가 가장 높은 규칙 : {MITx_8.02x_India_Secondary} => {MITx_6.002x_India_Secondary} : 0.39276910
#lift가 가장 높은 규칙 : {HarvardX_CB22x_UnitedStates_Secondary} <=> {HarvardX_ER22x_UnitedStates_Secondary} : 22.110346
#support, confidence, lift를 모두 고려하였을 때 효용성이 가장 높은 규칙 3가지
#1. {MITx_8.02x_India_Secondary} => {MITx_6.002x_India_Secondary} : 0.02128135
#2. {MITx_6.002x_India_Secondary} => {MITx_8.02x_India_Secondary} : 0.07474577
#3. {HarvardX_CB22x_UnitedStates_Secondary} => {HarvardX_ER22x_UnitedStates_Secondary} : 0.006113795

#조건절과 결과절의 위치에 따른 지표값의 차이
#1.[MITx_6.002x_UnitedStates_Secondary, MITx_6.00x_UnitedStates_Secondary]             support    confidence    lift
#  {MITx_6.00x_India_Secondary}            => {MITx_6.002x_India_Secondary}           0.003321297 0.17843759  9.597676   
#  {MITx_6.002x_India_Secondary}           => {MITx_6.00x_India_Secondary}            0.003321297 0.17864358  9.597676 

#2.[MITx_3.091x_UnitedStates_Secondary, MITx_6.00x_UnitedStates_Secondary]
#  {MITx_3.091x_UnitedStates_Secondary}    => {MITx_6.00x_UnitedStates_Secondary}     0.001405783 0.20876494  8.747335   
#  {MITx_6.00x_UnitedStates_Secondary}     => {MITx_3.091x_UnitedStates_Secondary}    0.001405783 0.05890288  8.747335  

#3.[HarvardX_CS50x_India_Secondary, MITx_6.00x_India_Secondary]
#  {HarvardX_CS50x_India_Secondary}        => {MITx_6.00x_India_Secondary}            0.001239450 0.14807692  7.955474   
#  {MITx_6.00x_India_Secondary}            => {HarvardX_CS50x_India_Secondary}        0.001239450 0.06658980  7.955474   

# 이와 같이 조건절과 결과절의 위치가 바뀌면 lift의 값은 변함이 없지만 confidence의 값이 변하게 된다.
# 일반적으로는 support의 값도 바뀌게 되는데 위의 상황같은 경우 count의 값이 같기 때문에 support의 값도 같게 되었다.
# A->B 인 rule에 대한 confidence는 P(A,B)/P(A), lift는 P(A,B)/[P(A)*P(B)] 이다.
# 한편, B->A 인 rule에 대한 lift도 P(A,B)/[P(B)*P(A)]으로 A->B rule일 때의 lift와 같지만
# confidence는 P(A,B)/P(B)이므로 값이 바뀌게 된다.
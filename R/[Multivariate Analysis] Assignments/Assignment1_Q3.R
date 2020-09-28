#Step3_Q3_��Ģ���� �� ��� �ؼ�

#Q3_1_��Ģ����
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
#support�� ���� ���� ��Ģ : {MITx_6.002x_India_Secondary} <=> {MITx_6.00x_India_Secondary} :  0.003321297 
#confidence�� ���� ���� ��Ģ : {MITx_8.02x_India_Secondary} => {MITx_6.002x_India_Secondary} : 0.39276910
#lift�� ���� ���� ��Ģ : {HarvardX_CB22x_UnitedStates_Secondary} <=> {HarvardX_ER22x_UnitedStates_Secondary} : 22.110346
#support, confidence, lift�� ��� �����Ͽ��� �� ȿ�뼺�� ���� ���� ��Ģ 3����
#1. {MITx_8.02x_India_Secondary} => {MITx_6.002x_India_Secondary} : 0.02128135
#2. {MITx_6.002x_India_Secondary} => {MITx_8.02x_India_Secondary} : 0.07474577
#3. {HarvardX_CB22x_UnitedStates_Secondary} => {HarvardX_ER22x_UnitedStates_Secondary} : 0.006113795

#�������� ������� ��ġ�� ���� ��ǥ���� ����
#1.[MITx_6.002x_UnitedStates_Secondary, MITx_6.00x_UnitedStates_Secondary]             support    confidence    lift
#  {MITx_6.00x_India_Secondary}            => {MITx_6.002x_India_Secondary}           0.003321297 0.17843759  9.597676   
#  {MITx_6.002x_India_Secondary}           => {MITx_6.00x_India_Secondary}            0.003321297 0.17864358  9.597676 

#2.[MITx_3.091x_UnitedStates_Secondary, MITx_6.00x_UnitedStates_Secondary]
#  {MITx_3.091x_UnitedStates_Secondary}    => {MITx_6.00x_UnitedStates_Secondary}     0.001405783 0.20876494  8.747335   
#  {MITx_6.00x_UnitedStates_Secondary}     => {MITx_3.091x_UnitedStates_Secondary}    0.001405783 0.05890288  8.747335  

#3.[HarvardX_CS50x_India_Secondary, MITx_6.00x_India_Secondary]
#  {HarvardX_CS50x_India_Secondary}        => {MITx_6.00x_India_Secondary}            0.001239450 0.14807692  7.955474   
#  {MITx_6.00x_India_Secondary}            => {HarvardX_CS50x_India_Secondary}        0.001239450 0.06658980  7.955474   

# �̿� ���� �������� ������� ��ġ�� �ٲ�� lift�� ���� ������ ������ confidence�� ���� ���ϰ� �ȴ�.
# �Ϲ������δ� support�� ���� �ٲ�� �Ǵµ� ���� ��Ȳ���� ��� count�� ���� ���� ������ support�� ���� ���� �Ǿ���.
# A->B �� rule�� ���� confidence�� P(A,B)/P(A), lift�� P(A,B)/[P(A)*P(B)] �̴�.
# ����, B->A �� rule�� ���� lift�� P(A,B)/[P(B)*P(A)]���� A->B rule�� ���� lift�� ������
# confidence�� P(A,B)/P(B)�̹Ƿ� ���� �ٲ�� �ȴ�.
library(dplyr)
require(ggplot2)
require(randomForest)
#require(MASS)
require(ISLR)
require(tree)



setwd('L:/Data Analytics/membership model/')

#load in membership info with paid through date >12/31/2002
#df_memb<-read.csv("membership_09_14_2015.csv", header=TRUE)
df_memb<-read.csv("membership_08_15_2015.csv", header=TRUE)

#df_memb<-read.csv("membership_rm_old_pay_thru.csv", header=TRUE)

#load in committee information
df_comm<-read.csv("count_comm.csv", header=TRUE)


#load in convention information
df_conv<-read.csv("count_conv.csv", header=TRUE)

#load in gift information
df_gift<-read.csv("count_gift.csv", header=TRUE)

#load in meeting information
df_meet<-read.csv("count_meeting.csv", header=TRUE)

#load in order information
df_order<-read.csv("count_order.csv", header=TRUE)

lj<-left_join(df_memb,df_comm)
lj<-left_join(lj,df_conv)
lj<-left_join(lj,df_gift)
lj<-left_join(lj,df_meet)
lj<-left_join(lj,df_order)



#lj$total_member_time<-as.numeric(lj$total_member_time)

lj[is.na(lj)] <- 0 #make all NAs into zeros

#convert to a local data frame
memb<-tbl_df(lj)
head(memb)

#convert dates to date format
memb$JOIN_DATE<-as.Date(memb$JOIN_DATE, "%m/%d/%Y")
memb$PAID_THRU<-as.Date(memb$PAID_THRU, "%m/%d/%Y")


#select Full Members
FM<-memb %>%
  select(ID,MEMBER_TYPE:count_order) %>%
  filter(MEMBER_TYPE=="FM") %>%
  arrange(desc(count_conv))

#select lapsed members

LM<-memb %>%
  select(ID,MEMBER_TYPE:count_order) %>%
  filter(MEMBER_TYPE=="LM") %>%
  arrange(desc(count_conv))

#obtain percentiles of columns
quant<-function (x) quantile(x,probs=c(0.25,0.5,0.75), na.rm=TRUE)
apply(FM[5:9],2,quant)
apply(LM[5:9],2,quant)

#sum of columns for a given row
memb_tot<-memb %>%
  group_by(ID) %>%
  mutate(total=sum(count_committee,count_conv, count_gift,count_meeting, count_order),dateDIFF=(PAID_THRU-JOIN_DATE)/365)

memb_tot$dateDIFF<-as.numeric(memb_tot$dateDIFF)

#obtain only FM
member_quant<-memb_tot %>%
  filter(MEMBER_TYPE=="FM")
#obtain percentiles of FM for total
apply(member_quant[15],2,quant)

#obtain only LM
nonmemb_quant<-memb_tot %>%
  filter(MEMBER_TYPE=="LM")
#obtain percentiles of LM for total
apply(nonmemb_quant[15],2,quant)

#obtain LM with large totals who expired after 7/31/2015


nonmemb_analysis<-nonmemb_quant %>%
  filter(total>6 & PAID_THRU>"2014-07-31") 
write.csv(nonmemb_analysis,file="Engaged_members_who_expired.csv")

write.csv(member_quant, file="Member Engagement.csv")





#obtain percentiles of LM for total




#calculate mean, sd, quantiles, min, and max
mean(member_quant$dateDIFF, na.rm=TRUE)
sd(member_quant$dateDIFF, na.rm=TRUE)
quantile(member_quant$dateDIFF,probs=c(0.25,0.5,0.75), na.rm=TRUE)
min(member_quant$dateDIFF, na.rm=TRUE)
max(member_quant$dateDIFF, na.rm=TRUE)


mean(nonmemb_quant$dateDIFF, na.rm=TRUE)
sd(nonmemb_quant$dateDIFF, na.rm=TRUE)
quantile(nonmemb_quant$dateDIFF,probs=c(0.25,0.5,0.75), na.rm=TRUE)
min(nonmemb_quant$dateDIFF, na.rm=TRUE)
max(nonmemb_quant$dateDIFF, na.rm=TRUE)

nonmemb_quant %>%
  select(ID,MEMBER_TYPE:dateDIFF) %>%
  filter(MEMBER_TYPE=="LM") %>%
  arrange(desc(dateDIFF))


#find how many zeros there are in a column
zeros<-function (x) sum(x==0)
#count the total number of zeros for the columns (dividing by the total n for FM and LM to get the percentages)
apply(member_quant[10:15],2,zeros)/23316
apply(nonmemb_quant[10:15],2,zeros)/51666
#obtain summary statistics by member type
memb_summary<-memb_tot %>%
  group_by(MEMBER_TYPE) %>%
  summarise_each(funs(mean, sd, median, min, max, n()),count_committee:dateDIFF)

ggplot(memb_tot, aes(x=total, fill=MEMBER_TYPE)) + 
  geom_density()

ggplot(memb_tot, aes(x=MEMBER_TYPE, y=total)) + 
  #geom_point(position="jitter", alpha=0.2) + 
  geom_boxplot()


#comparison of people with less than 5 years at AWHONN for FM vs. LM
memb_summary<-memb_tot %>%
  group_by(MEMBER_TYPE) %>%
  filter(dateDIFF<5) %>%
  summarise_each(funs(mean, sd, median, min, max, n()),count_committee:dateDIFF)

#comparison of people with 5 years or more at AWHONN for FM vs. LM
memb_summary<-memb_tot %>%
  group_by(MEMBER_TYPE) %>%
  filter(dateDIFF>=5) %>%
  summarise_each(funs(mean, sd, median, min, max, n()),count_committee:dateDIFF)

#########creating comparison tests

wilcox.test(member_quant$count_committee, nonmemb_quant$count_committee, alternative = "two.sided")

wilcox.test(member_quant$count_conv, nonmemb_quant$count_conv, alternative = "two.sided")

wilcox.test(member_quant$count_gift, nonmemb_quant$count_gift, alternative = "two.sided")

wilcox.test(member_quant$count_meeting, nonmemb_quant$count_meeting, alternative = "two.sided")

wilcox.test(member_quant$count_order, nonmemb_quant$count_order, alternative = "two.sided")


wilcox.test(member_quant$total, nonmemb_quant$total, alternative = "two.sided")


wilcox.test(member_quant$dateDIFF, nonmemb_quant$dateDIFF, alternative = "two.sided")

##### Creating a decision tree###########################
select(memb, ID, MEMBER_TYPE:count_order)
###Create the Decision Tree
### Get a subset of data for the model

tree.data<-lj[,c(7,11:16)] #should be 11:16


tree.memb=tree(MEMBER_TYPE~.,data=tree.data)
summary(tree.memb)
plot(tree.memb)
text(tree.memb,pretty=0)

set.seed(1011)
train=sample(1:nrow(tree.data),20000)
tree.memb=tree(MEMBER_TYPE~.,tree.data,subset=train)
plot(tree.memb);text(tree.memb,pretty=0)
tree.pred=predict(tree.memb,tree.data[-train,],type="class")
with(tree.data[-train,],table(tree.pred,MEMBER_TYPE))
(2891+7258)/(2981+2141+4557+7258)


#create a logistic regression for the data

#revalue factor levels
levels(tree.data$MEMBER_TYPE)[1] <- '1'
levels(tree.data$MEMBER_TYPE)[2]<-'0'

mylogit <- glm(MEMBER_TYPE ~ ., data = tree.data, family = "binomial")
mylogit<-glm(MEMBER_TYPE~tot_memb_time_class+count_gift+count_meeting+count_order, data=tree.data, family="binomial")
summary(mylogit)


newdata = data.frame(tot_memb_time_class=1, count_gift=0, count_meeting=0, count_order=0)

predict(mylogit, newdata, type="response") 
#### Create a Random Forest Model

rf.memb=randomForest(MEMBER_TYPE~.,data=tree.data,subset=train)


lapsed<-read.csv(file="Lapsed Members.csv", header=TRUE)
names(lapsed)[1]<-"ID"
activity<-read.csv(file="count_activities.csv", header=TRUE)
names(activity)[1]<-"ID"

ijna<-inner_join(lapsed,activity,by="ID")
write.csv(ijna,file="lapsed_w_activity.csv")

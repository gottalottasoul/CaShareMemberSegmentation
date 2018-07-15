###########################################################################
# Zipcar Daily RFM scores
# 5/24/16
###########################################################################


require(zipcarFunctions)

#unload all previously loaded packages
detachAllPackages()

#load required libraries
if (!require(readr)) {
  install.packages('readr') # read preformatted dates
  require(readr)
}
if (!require(data.table)) {
  install.packages('data.table') # faster fread() and better weekdays()
  require(data.table)
}
if (!require(dplyr)) {
  install.packages('dplyr') # consistent data.frame operations
  require(dplyr)
}
if (!require(purrr)) {
  install.packages('purrr') # consistent & safe list/vector munging
  require(purrr)
}
if (!require(tidyr)) {
  install.packages('tidyr') # consistent data.frame cleaning
  require(tidyr)
}
if (!require(lubridate)) {
  install.packages('lubridate') # date manipulation
  require(lubridate)
}
if (!require(devtools)) {
  install.packages('devtools') # developer tools - install packages from github
  require(devtools)
}
if (!require(here)) {
  devtools::install_github("krlmlr/here") #relative working directory
  require(here)
}

#set working directory
setwd(here())


## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()


############################
## start functions        ##
############################

my.recency<-function(x){
  
  ifelse(x<=14, 5,
         ifelse(x>182,1,
                (182-x) *(1/(182-14)*4)+1))
}

my.frequency<-function(x){
  
  ifelse(x<=17, 5,
         ifelse(x>=56,1,
                (56-x) * (1/(56-17)*4)+1))
}

my.monetary<-function(x){
  ifelse(x>=75,5,
         ifelse(x==0,1,
                x * (1/75*4)+1))
}

my.rfm.score<-function(r,f,m){
  recency.weight=4
  frequency.weight=0.5
  monetary.weight=5.5
  
  round(((r*recency.weight)+(f*frequency.weight)+(m*monetary.weight)),0)
  
}


############################
## end functions          ##
############################

#get all members
members.all <- getMemberData("MembersWeekly-Current.txt")
#members.all <- getMemberData("MembersWeekly-LY.txt")


#get all reservations
reservations.all <- getReservationData("Reservation-Details-Current.txt")
#reservations.all <- getReservationData("Reservation-Details-LY.txt")


#reservations.cancels <- reservations.all %>% 
#  filter(CANCELLED_RES == 1)
reservations.all <- reservations.all %>% 
  filter(CANCELLED_RES == 0)
#rm(reservations.cancels)
reservations.all$RESERVATION_DATE <- as.Date(reservations.all$RESERVATION_DATE)

curr_date = as.Date("2017-05-01")
curr_ceiling = curr_date + 6
prior_ceiling = curr_date - 1
prior_floor = prior_ceiling - 182

member.weekly <- members.all %>%
  filter(SNAPSHOT_DATE == curr_date) %>%
  mutate(NewMember = ifelse(curr_date - FIRST_JOIN <= 14,1,0)
         ,TIME.TO.FIRST.DRIVE = difftime(FIRST_RES,FIRST_JOIN,units = "days")
         ,FIRST_JOIN_COHORT = paste(toupper(format(FIRST_JOIN,"%Y-%b")))) %>%
  select(MEMBER_ID,ACCOUNT_ID,RATE_PLAN_ID,FIRST_JOIN_COHORT,NewMember,ACCOUNT_ZIPFLEET_ID,BUSINESS_SEGMENT,RATEPLAN,DIRECT_BILL,FEE)

#MembersWeekly_LY_mod[is.na(MembersWeekly_LY_mod$ACCT_END),]$ACCT_END<-as.Date("2017-07-01")

#member.weekly <- MembersWeekly_LY_mod %>%
#  filter(ACCT_START < curr_date & ACCT_END > curr_date) %>%
#  mutate(NewMember = ifelse(curr_date - FIRST_JOIN <= 14,1,0)
#         ,FIRST_JOIN_COHORT = paste(toupper(format(FIRST_JOIN,"%Y-%b")))) %>%
#  select(MEMBER_ID,FIRST_JOIN_COHORT,NewMember,ZIPFLEET,BUSINESS_SEGMENT,RATEPLAN)


################################################

#prev.6.month.res<-reservations.all%>%
#  subset(RESERVATION_DATE >=prior_floor & RESERVATION_DATE<=prior_ceiling)%>%
#  setorder(RESERVATION_DATE)%>%
#  .[,Prev_RES:=c(NA,as.Date(RESERVATION_DATE[-.N])),by=MEMBER_ID]%>%
#  .[,RES_DELTA:=as.numeric(RESERVATION_DATE-Prev_RES),by=MEMBER_ID]%>%
#  .[,`:=`(Total_Res=.N,Total_Hours=sum(TOTAL_HOURS),Med.TimeToNextRes=ifelse(is.na(median(RES_DELTA,na.rm=TRUE)),75,median(RES_DELTA,na.rm=TRUE)),Most.Recent.Res=curr_date-max(RESERVATION_DATE)),by=MEMBER_ID]%>%
#  .[,`:=`(Recency=my.recency(Most.Recent.Res),Frequency=my.frequency(Med.TimeToNextRes),Monetary=my.monetary(Total_Hours)),by=MEMBER_ID]%>%
#  .[,`:=`(RecencyNew=my.recency.new(Most.Recent.Res),FrequencyNew=my.frequency.new(Med.TimeToNextRes),MonetaryNew=my.monetary.new(Total_Hours)),by=MEMBER_ID]%>%
#    .[,c('MEMBER_ID','Recency','Frequency','Monetary'),with=FALSE]%>%
#  .[,c('MEMBER_ID','Recency','Frequency','Monetary','RecencyNew','FrequencyNew','MonetaryNew'),with=FALSE]%>%
#   unique(.)


####try doing it all in dataframe
#setDF(reservations.all)
prev.6.month.res <- reservations.all %>%
  subset(RESERVATION_DATE >= prior_floor & RESERVATION_DATE <= prior_ceiling) %>%
  group_by(MEMBER_ID) %>%
  arrange(RESERVATION_DATE) %>%
  mutate(PREV_REZ = shift(RESERVATION_DATE,1,type = "lead")) %>%
  mutate(RES_DELTA = as.numeric(PREV_REZ - RESERVATION_DATE)) %>%
  summarise(TOTAL_RES = n()
            ,Med.TimeToNextRes = median(RES_DELTA,na.rm = TRUE)
            ,Most.Recent.Res = curr_date - max(RESERVATION_DATE)
            ,Total_Hours = sum(TOTAL_HOURS)) %>%
  mutate(Recency = recency_score(Most.Recent.Res)
         ,Frequency = ifelse(is.na(Med.TimeToNextRes),1,frequency_score(Med.TimeToNextRes))
         ,Monetary = ifelse(is.na(Total_Hours),1,monetary_score(Total_Hours))) %>%
  select(MEMBER_ID,Recency,Frequency,Monetary)
  

member.rfm <- left_join(member.weekly,prev.6.month.res,by = "MEMBER_ID")
#member.rfm<-data.table(member.rfm)
member.rfm[is.na(member.rfm)] <- 1



member.rfm$RFM <- rfm_score(member.rfm$Recency,member.rfm$Frequency,member.rfm$Monetary)
member.rfm$Snapshot <- curr_date


member.rfm$Profile <- ifelse(member.rfm$RFM == 50,"Power User",
                            ifelse(member.rfm$RFM >= 40,"Strong",
                                   ifelse(member.rfm$RFM >= 30,"Standard+",
                                          ifelse(member.rfm$RFM >= 20,"Standard-",
                                                 ifelse(member.rfm$RFM > 10,"Vulnerable",
                                                        "Low Activity")))))

#setDF(member.rfm)
#rfm.weekly.tab<-crosstab(member.rfm, row.vars = "Profile", col.vars = "ProfileOrig", type = "f")
#print.crosstab(rfm.weekly.tab)

next.7.day.rev <- reservations.all %>%
  filter(RESERVATION_DATE >= curr_date & RESERVATION_DATE <= curr_ceiling) %>%
  group_by(MEMBER_ID) %>%
  summarise(Reservations = n()
                   ,Revenue = sum(UP_REVENUE))

member.rfm <- left_join(member.rfm,next.7.day.rev,by = "MEMBER_ID")
member.rfm[is.na(member.rfm)] <- 0

write_delim(member.rfm,"C:/Users/babbenante/OneDrive - Avis Budget Group/my stuff/reports/rfm/rfmscores20170501.txt")

member.rfm.master<-rbind(member.rfm.master,member.rfm)

member.weekly.summary<-member.rfm %>% 
  group_by(Snapshot,ZIPFLEET,BUSINESS_SEGMENT,Profile) %>% 
  summarise(members=n()
            ,driving_members=sum(Reservations>0)
            ,Reservations=sum(Reservations)
            ,Revenue=sum(Revenue))

#member.weekly.summary.all<-rbind(member.weekly.summary.all,member.weekly.summary)

#write_delim(member.weekly.summary.all,"C:/Users/babbenante/OneDrive - Avis Budget Group/my stuff/reports/rfm/rfmYoY.txt",delim="\t")
##############################################################################################
###   Zipcar Member RFM segmentation
###
###   built on: 4/12/15
###
##############################################################################################



#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\code\\rfm\\")


#load the required libraries
if (!require(dplyr)) {
  install.packages('dplyr')
  require(dplyr)
}
if (!require(data.table)) {
  install.packages('data.table')
  require(data.table)
}
if (!require(readr)) {
  install.packages('readr')
  require(readr)
}
if (!require(lubridate)) {
  install.packages('lubridate')
  require(lubridate)
}



## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()




reservations.all=read_delim("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\Reservations201301-201603.txt"
                            ,"\t"
                            ,col_types = cols(col_character()
                                              ,col_character()
                                              ,col_character()
                                              ,col_date("%m/%d/%Y")
                                              ,col_character()
                                              ,col_number()
                                              ,col_number()
                                              ,col_number()
                                              ,col_number()
                                              ,col_number()
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_integer()
                                              ,col_character()
                                              ,col_character()
                                              ,col_character()
                                              ,col_character()
                                              ,col_integer()))
#reservations.all$MEMBER_ID<-as.factor(reservations.all$MEMBER_ID)
reservations.all$ZIPFLEET<-as.factor(reservations.all$ZIPFLEET)
reservations.all$SEGMENT_CLASS<-as.factor(reservations.all$SEGMENT_CLASS)
reservations.all$RATED_AS<-as.factor(reservations.all$RATED_AS)
reservations.all$VEHICLE_CLASS<-as.factor(reservations.all$VEHICLE_CLASS)
reservations.all$LOCATION_TYPE<-as.factor(reservations.all$LOCATION_TYPE)
reservations.all$RENTAL_TIME_UTIL<-reservations.all$TOTAL_DRIVING_TIME/reservations.all$TOTAL_HOURS
reservations.all$Rental_Month<-month(reservations.all$RESERVATION_DATE)
reservations.all$Rental_Year<-year(reservations.all$RESERVATION_DATE)
reservations.all<-data.table(reservations.all)

members.all=read_delim("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\MembersMonthly-2013-2016.txt","\t"
                       ,col_types = cols(col_character()
                                         ,col_character()
                                         ,col_character()
                                         ,col_character()
                                         ,col_date("%m/%d/%Y")
                                         ,col_date("%m/%d/%Y")
                                         ,col_character()))
names(members.all)[2]<-paste("ZIPFLEET")
names(members.all)[4]<-paste("RatePlan")
names(members.all)[7]<-paste("SNAPSHOT_YM")
members.all$MEMBER_ID<-as.factor(members.all$MEMBER_ID)
members.all$ZIPFLEET<-as.factor(members.all$ZIPFLEET)
members.all$BUSINESS_SEGMENT<-as.factor(members.all$BUSINESS_SEGMENT)
members.all$RatePlan<-as.factor(members.all$RatePlan)
members.all$SNAPSHOT_YM<-as.factor(members.all$SNAPSHOT_YM)
members.all<-data.table(members.all)




snapshot_str="2014-01"
curr_floor="2014-01-01"
curr_ceiling="2014-01-31"
prior_floor="2013-01-01"
prior_ceiling="2013-12-31"

members.rfm<-members.all%>%
  filter(SNAPSHOT_YM ==snapshot_str) ## <- change date here for the month you want

reservations.current.rfm<-reservations.all%>%
  ####  <- make sure date range below reflects the month for the first date above -> #####
filter(RESERVATION_DATE>=as.Date(curr_floor),RESERVATION_DATE<=as.Date(curr_ceiling))%>%
  group_by(MEMBER_ID)%>%
  summarise(Reservations=n()
            ,Revenue=sum(UP_REVENUE)
            ,Hours=sum(TOTAL_HOURS)
            ,Distance=sum(TOTAL_DISTANCE))
  

reservations.history.rfm<-reservations.all%>%
  ####  <- make sure date range below reflects 12 months prior to the date above -> #####
  filter(RESERVATION_DATE>=as.Date(prior_floor),RESERVATION_DATE<=as.Date(prior_ceiling))%>%
  group_by(MEMBER_ID)%>%
  summarise(Prior_Reservations=n()
            ,Frequency=n_distinct(Rental_Month)
            ,Recency=max(RESERVATION_DATE)
            ,Monetary=mean(UP_REVENUE))%>%
  mutate(Recency=as.duration(interval(Recency,as.Date(curr_floor)))) ## <- change it here too, should match first

current.rfm.score<-merge(members.rfm,reservations.history.rfm,all.x = TRUE,by="MEMBER_ID")
current.rfm.score<-merge(current.rfm.score,reservations.current.rfm,all.x = TRUE,by="MEMBER_ID")
monthly.rfm.score<-rbind(monthly.rfm.score,current.rfm.score)
rm(current.rfm.score,reservations.current.rfm,reservations.history.rfm,members.rfm)
gc()

write_rds(monthly.rfm.score,"C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\RData\\MonthlyRFMScore.rds")

monthly.rfm.score<-read_rds("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\RData\\MonthlyRFMScore.rds")

monthly.rfm.score<-monthly.rfm.score%>%
  mutate(R_Score= ifelse(Recency<dweeks(4), 3,
                         ifelse(Recency<dweeks(24), 2, 1)))%>%
  mutate(F_Score= ifelse(Frequency>6, 3,
                         ifelse(Frequency>3, 2, 1)))%>%
  mutate(M_Score= ifelse((Monetary*Prior_Reservations)/Frequency>=100, 3,
                         ifelse((Monetary*Prior_Reservations)/Frequency>=50, 2, 1)))%>%
  mutate(first_join_cohort= paste0(year(FIRST_JOIN),"-",toupper(month(FIRST_JOIN,label=TRUE))))

monthly.rfm.score$SNAPSHOT_YM<-as.character(monthly.rfm.score$SNAPSHOT_YM)
monthly.rfm.score[!complete.cases(monthly.rfm.score[,c("Recency","Frequency","Monetary"),with=FALSE]),]$R_Score<-9
monthly.rfm.score[!complete.cases(monthly.rfm.score[,c("Recency","Frequency","Monetary"),with=FALSE]),]$F_Score<-9
monthly.rfm.score[!complete.cases(monthly.rfm.score[,c("Recency","Frequency","Monetary"),with=FALSE]),]$M_Score<-9
monthly.rfm.score[which(monthly.rfm.score$R_Score==9 & monthly.rfm.score$SNAPSHOT_YM==monthly.rfm.score$first_join_cohort),]$R_Score<-0
monthly.rfm.score[which(monthly.rfm.score$F_Score==9 & monthly.rfm.score$SNAPSHOT_YM==monthly.rfm.score$first_join_cohort),]$F_Score<-0
monthly.rfm.score[which(monthly.rfm.score$M_Score==9 & monthly.rfm.score$SNAPSHOT_YM==monthly.rfm.score$first_join_cohort),]$M_Score<-0
monthly.rfm.score$RFM<-paste0(monthly.rfm.score$R_Score,monthly.rfm.score$F_Score,monthly.rfm.score$M_Score)

monthly.rfm.counts<-monthly.rfm.score%>%
  group_by(ZIPFLEET,SNAPSHOT_YM,RFM)%>%
  dplyr::summarise(members=n()
                   ,active_members=sum(!is.na(Reservations))
                   ,reservations=sum(Reservations,na.rm=TRUE)
                   ,revenue=sum(Revenue,na.rm=TRUE))

write_csv(monthly.rfm.counts,"C:\\Users\\Babbenante\\downloads\\blake.csv")





monthly.rfm.score$SNAPSHOT_YM<-mapvalues(monthly.rfm.score$SNAPSHOT_YM,from=(c("2013-01","2013-02","2013-03","2013-04","2013-05","2013-06","2013-07","2013-08","2013-09","2013-10"
                                               ,"2013-11","2013-12","2014-01","2014-02","2014-03","2014-04","2014-05","2014-06","2014-07","2014-08"
                                               ,"2014-09","2014-10","2014-11","2014-12","2015-01","2015-02","2015-03","2015-04","2015-05","2015-06"
                                               ,"2015-07","2015-08","2015-09","2015-10","2015-11","2015-12","2016-01","2016-02","2016-03"))
          ,to=(c("2013-JAN","2013-FEB","2013-MAR","2013-APR","2013-MAY","2013-JUN","2013-JUL","2013-AUG","2013-SEP","2013-OCT"
                ,"2013-NOV","2013-DEC","2014-JAN","2014-FEB","2014-MAR","2014-APR","2014-MAY","2014-JUN","2014-JUL","2014-AUG"
                ,"2014-SEP","2014-OCT","2014-NOV","2014-DEC","2015-JAN","2015-FEB","2015-MAR","2015-APR","2015-MAY","2015-JUN"
                ,"2015-JUL","2015-AUG","2015-SEP","2015-OCT","2015-NOV","2015-DEC","2016-JAN","2016-FEB","2016-MAR")))

monthly.rfm.score[!complete.cases(monthly.rfm.score[,c("Recency","Frequency","Monetary"),with=FALSE]),]
monthly.rfm.score[complete.cases(monthly.rfm.score[,c("Recency","Frequency","Monetary"),with=FALSE],)]

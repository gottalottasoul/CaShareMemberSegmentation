##############################################################################################
###   Zipcar Member RFM segmentation
###   2011-2015
###
###   built on: 12/22/15
###
##############################################################################################



#set working directory
setwd("C:\\Users\\Babbenante\\documents\\my stuff\\code\\rfm\\")


#load the required libraries
#require(plyr) || install.packages("plyr", repos="http://cran.rstudio.org") 
#library(plyr)

require(dplyr) || install.packages("dplyr", repos="http://cran.rstudio.org") 
library(dplyr)

#require(tidyr) || install.packages("tidyr", repos="http://cran.rstudio.org") 
#library(tidyr)

require(data.table) || install.packages("data.table", repos="http://cran.rstudio.org") 
library(data.table)

#require(shiny) || install.packages("shiny", repos="http://cran.rstudio.org") 
#library(shiny)

#require(devtools) || install.packages("devtools", repos="http://cran.rstudio.org") 
#library(devtools)
#install_github('ramnathv/rCharts')

#require(rCharts) 

## clear the console of all variables
rm(list = ls())
## free up unused memory
gc()




################################################################################
# Function
# 	getIndependentScore(df,r=5,f=5,m=5)
#
# Description
#	Scoring the Recency, Frequency, and Monetary in r, f, and m in aliquots independently
#
# Arguments
#	df - A data frame returned by the function of getDataFrame
#	r -  The highest point of Recency
#	f -  The highest point of Frequency
#	m -  The highest point of Monetary
#
# Return Value
#	Returns a new data frame with four new columns of "R_Score","F_Score","M_Score", and "Total_Score".
#################################################################################

getIndependentScore <- function(df,r=5,f=5,m=5) {
  
  if (r<=0 || f<=0 || m<=0) return
  
  #order and the score
  df <- df[order(df$Recency,-df$Frequency,-df$Monetary),]
  R_Score <- scoring(df,"Recency",r)
  df <- cbind(df, R_Score)
  
  df <- df[order(-df$Frequency,df$Recency,-df$Monetary),]
  F_Score <- scoring(df,"Frequency",f)
  df <- cbind(df, F_Score)
  
  df <- df[order(-df$Monetary,df$Recency,-df$Frequency),]
  M_Score <- scoring(df,"Monetary",m)
  df <- cbind(df, M_Score)
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  # caculate the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  
  return (df)
  
} # end of function getIndependentScore

################################################################################
# Function
# 	scoring(df,column,r=5)
#
# Description
#	A function to be invoked by the getIndepandentScore function
#######################################
scoring <- function (df,column,r=5){
  
  #get the length of rows of df
  len <- dim(df)[1]
  
  score <- rep(0,times=len)
  
  # get the quantity of rows per 1/r e.g. 1/5
  nr <- round(len / r)
  if (nr > 0){
    
    # seperate the rows by r aliquots
    rStart <-0
    rEnd <- 0
    for (i in 1:r){
      
      #set the start row number and end row number
      rStart = rEnd+1
      
      #skip one "i" if the rStart is already in the i+1 or i+2 or ...scope.
      if (rStart> i*nr) next
      
      if (i == r){
        if(rStart<=len ) rEnd <- len else next
      }else{
        rEnd <- i*nr
      }
      
      # set the Recency score
      score[rStart:rEnd]<- r-i+1
      
      # make sure the customer who have the same recency have the same score
      s <- rEnd+1
      if(i<r & s <= len){
        for(u in s: len){
          if(df[rEnd,column]==df[u,column]){
            score[u]<- r-i+1
            rEnd <- u
          }else{
            break;
          }
        }
        
      }
      
    }
    
  }
  return(score)
  
} #end of function Scoring


################################################################################
# Function
# 	getScoreWithBreaks(df,r,f,m)
#
# Description
#	Scoring the Recency, Frequency, and Monetary in r, f, and m which are vector object containing a series of breaks
#
# Arguments
#	df - A data frame returned by the function of getDataFrame
#	r -  A vector of Recency breaks
#	f -  A vector of Frequency breaks
#	m -  A vector of Monetary breaks
#
# Return Value
#	Returns a new data frame with four new columns of "R_Score","F_Score","M_Score", and "Total_Score".
#
#################################################################################

getScoreWithBreaks <- function(df,r,f,m) {
  
  ## scoring the Recency
  len = length(r)
  R_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,R_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=r[i-1]
    }
    p2=r[i]
    
    if(dim(df[p1<df$Recency & df$Recency<=p2,])[1]>0) df[p1<df$Recency & df$Recency<=p2,]$R_Score = len - i+ 2
  }
  
  ## scoring the Frequency	
  len = length(f)
  F_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,F_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=f[i-1]
    }
    p2=f[i]
    
    if(dim(df[p1<df$Frequency & df$Frequency<=p2,])[1]>0) df[p1<df$Frequency & df$Frequency<=p2,]$F_Score = i
  }
  if(dim(df[f[len]<df$Frequency,])[1]>0) df[f[len]<df$Frequency,]$F_Score = len+1
  
  ## scoring the Monetary	
  len = length(m)
  M_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,M_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=m[i-1]
    }
    p2=m[i]
    
    if(dim(df[p1<df$Monetary & df$Monetary<=p2,])[1]>0) df[p1<df$Monetary & df$Monetary<=p2,]$M_Score = i
  }
  if(dim(df[m[len]<df$Monetary,])[1]>0) df[m[len]<df$Monetary,]$M_Score = len+1
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  # caculate the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  
  return(df)
  
} # end of function of getScoreWithBreaks


################################################################################
# Function
# 	drawHistograms(df,r,f,m)
#
# Description
#	Draw the histograms in the R, F, and M dimensions so that we can see the quantity of customers in each RFM block.
#
# Arguments
#	df - A data frame returned by the function of getIndependent or getScoreWithBreaks
#	r -  The highest point of Recency
#	f -  The highest point of Frequency
#	m -  The highest point of Monetary
#
# Return Value
#	No return value.
#
#################################################################################
drawHistograms <- function(df,r=5,f=5,m=5){
  
  #set the layout plot window
  par(mfrow = c(f,r))
  
  names <-rep("",times=m)
  for(i in 1:m) names[i]<-paste("M",i)
  
  
  for (i in 1:f){
    for (j in 1:r){
      c <- rep(0,times=m)
      for(k in 1:m){
        tmpdf <-df[df$R_Score==j & df$F_Score==i & df$M_Score==k,]
        c[k]<- dim(tmpdf)[1]
        
      }
      if (i==1 & j==1) 
        barplot(c,col="lightblue",names.arg=names)
      else
        barplot(c,col="lightblue")
      if (j==1) title(ylab=paste("F",i))	
      if (i==1) title(main=paste("R",j))	
      
    }
    
  }
  
  par(mfrow = c(1,1))
  
} # end of drawHistograms function

###############################################################################
### END FUNCTIONS                                                           ###
###############################################################################

##############################################################################################
### if running for the first time on a new computer 
### or if data has been refreshed
### uncomment the following code
### otherwise skip right to the load
##############################################################################################

load("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\RData\\All_Member_History_Monthly.RData")

##member.history.new=read.csv("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\Monthlies\\2015-03.txt",sep="\t")
member.history.new=fread("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\Monthlies\\2016-03.txt",sep='\t')
names(member.history.new)[1]<-paste("MEMBER_ZIPFLEET")
member.history.new$SEGMENT_CLASS<-as.factor(member.history.new$SEGMENT_CLASS)
member.history.new$MEMBER_ID<-as.factor(member.history.new$MEMBER_ID)
member.history.new$FIRST_RES_DATE<-as.POSIXct(strptime(member.history.new$FIRST_RES_DATE,"%m/%d/%Y %H:%M:%S"))
member.history.new$FIRST_JOIN_DATE<-as.POSIXct(strptime(member.history.new$FIRST_JOIN_DATE,"%m/%d/%Y %H:%M:%S"))
member.history.new$SnapshotYear<-"2016"
member.history.new$SnapshotMonth<-"03"
member.history.new$SnapshotYM<-"2016-03"
member.history.new$DaysToFirstDrive<-difftime(member.history.new$FIRST_RES_DATE,member.history.new$FIRST_JOIN_DATE,units="days")

#member.history<-member.history.new
member.history<-rbind(member.history,member.history.new)
rm(member.history.new)


member.history$SnapshotYear<-as.factor(member.history$SnapshotYear)
member.history$SnapshotMonth<-as.factor(member.history$SnapshotMonth)
member.history$SnapshotYM<-as.factor(member.history$SnapshotYM)
member.history$DaysToFirstDrive<-difftime(member.history$FIRST_RES_DATE,member.history$FIRST_JOIN_DATE,units="days")

#save(member.history, file="C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\RData\\All_Member_History_Monthly.RData")

##############################################################################
##HELPER TABLE
##build the data sets we're interested in using
##the first should be 12 months of historical
##to forecast the next
##############################################################################

#set the cutpoints for the segmentation
r<-c(1,6)
f<-c(2,6)
m<-c(50,100)


member.history<-data.table(member.history)
member.history[member.history==0] <- NA

#build a list of all members for the past 2 years, and keep their relevant dimensions
all.members.new<-unique(member.history,by=c("MEMBER_ID","MEMBER_ZIPFLEET","FIRST_RES_DATE","FIRST_JOIN_DATE","DaysToFirstDrive"))%>%
              .[, c("MEMBER_ID","MEMBER_ZIPFLEET","FIRST_RES_DATE","FIRST_JOIN_DATE","DaysToFirstDrive"), with=FALSE]

load("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\RData\\Member_Monthly_RFM_Tabulated_Scores.RData")

#we want to take all the previous work we've done, and add new members (should be those acquired this month)
#to our master list of members
all.members<-merge(all.members.new,all.members,all.x = TRUE,by=c("MEMBER_ID","FIRST_RES_DATE","FIRST_JOIN_DATE","DaysToFirstDrive"))
#use the most recent zipfleet id
all.members$MEMBER_ZIPFLEET.y<-NULL
names(all.members)[5]<-paste("MEMBER_ZIPFLEET")

setcolorder(all.members, c(1:23,50:51,24:49))

#get rid of redundant data table
rm(all.members.new)

#get the past 12 months preceding the reporting period we're focusing on
um.rfm<-member.history %>%
  #add list of previous 12 months before the month you want to report on  
  filter(grepl('^2015-03$|^2015-04$|^2015-05$|^2015-06$|^2015-07$|^2015-08$|^2015-09$|^2015-10$|^2015-11$|^2015-12$|^2016-01$|^2016-02$',SnapshotYM)) %>%
#  group_by(MEMBER_ID) %>%
  #similar to above, but we need to put them in order so they are factored correctly  
  .[,Frequency := sum(!is.na(TOT_RESERVATIONS)), by=MEMBER_ID] %>%
  .[,Revenue := sum(TOT_REVENUE,na.rm=TRUE),by=MEMBER_ID] %>%
  .[,Recency := 13-max(which(complete.cases(TOT_RESERVATIONS))),by=MEMBER_ID] %>%
  .[,Monetary := Revenue/Frequency] %>%
  .[, c("MEMBER_ID","Recency","Frequency","Monetary"), with=FALSE]
  
#there will be duplicates, get singular records
um.rfm<-unique(um.rfm,by="MEMBER_ID")

um.rfm.active<-um.rfm[which(um.rfm$Frequency>0)]

um.performance<-member.history %>%
  ####change this guy for the month you want to report on
  filter(grepl('^2016-03$',SnapshotYM)) %>%
  group_by(MEMBER_ID) %>%
  summarise(Tot_Res=round(sum(TOT_RESERVATIONS),1),Tot_Rev=round(sum(TOT_REVENUE),2))
um.performance<-data.table(um.performance)

#when we want to append current performance to the member list, we should replace NA with 0
um.performance[is.na(um.performance)] <- 0
all.members<-merge(all.members,um.performance,all.x = TRUE,by="MEMBER_ID")
names(all.members)[52]<-paste("201603RES")
names(all.members)[53]<-paste("201603REV")


rfm.mem.scores<-getScoreWithBreaks(um.rfm.active,r,f,m)

rfm.seg.counts.current.period<-rfm.mem.scores %>%
  group_by(Total_Score) %>%
  summarise(Member_Count=n_distinct(MEMBER_ID))

#append rfm score
rfm.mem.performance.current.period<-merge(um.performance,rfm.mem.scores,all.x = TRUE,by="MEMBER_ID")
#find all the people who werent scored and give them a 0
rfm.mem.performance.current.period<-replace(rfm.mem.performance.current.period, is.na(rfm.mem.performance.current.period),0)%>%
                                    .[, c("MEMBER_ID","Total_Score"), with=FALSE]

all.members<-merge(all.members,rfm.mem.performance.current.period,all.x = TRUE,by="MEMBER_ID")
names(all.members)[54]<-paste("201603YM")
gc()

#append the loyalty info to the dataset
member.loyalty=fread("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Loyalty\\MembersOptingIntoLoyaltyPrograms.csv",sep=',')
#this is dumb - do we really need to unfactor to merge?
member.loyalty$MEMBER_ID<-as.character(member.loyalty$MEMBER_ID)
all.members$MEMBER_ID<-as.character(all.members$MEMBER_ID)
all.members<-merge(all.members,member.loyalty,all.x = TRUE,by="MEMBER_ID")
all.members[which(is.na(all.members$PROMOTIONS))]$PROMOTIONS<-"NONE"
all.members$PROMOTIONS<-as.factor(all.members$PROMOTIONS)
all.members$MEMBER_ID<-as.factor(all.members$MEMBER_ID)

save(all.members, file="C:\\Users\\babbenante\\documents\\My Stuff\\Data\\RFM\\RData\\Member_Monthly_RFM_Tabulated_Scores_new.RData")

#combine first res with most recent cohort
rfm.mem.performance.current.period.firstres<-merge.data.frame(rfm.mem.performance.current.period,first.res,by="MEMBER_ID")
rfm.mem.performance.current.period.firstres<-data.table(rfm.mem.performance.current.period.firstres)
####change this guy for the month you want to report on (0 padded for one digit months)
rfm.mem.performance.current.period.firstres[Total_Score == 0 & FIRST_RES_COHORT!="2016-01", Total_Score:=9]

#For RFM - 0 what's the breakdown of first res cohort
um.FR<-rfm.mem.performance.current.period.firstres %>%
  filter(grepl('^0$|^9$',Total_Score)) %>%
  group_by(FIRST_RES_COHORT) %>%
  summarise(n_distinct(MEMBER_ID)) 

#Current Period Rentals by RFM segment
rfm.seg.perf.current.period<-rfm.mem.performance.current.period.firstres %>%
  group_by(Total_Score) %>%
  summarise(ActiveMembers=n_distinct(MEMBER_ID),Avg_Res=round(mean(Tot_Res),1),Avg_Rev=round(mean(Tot_Rev),2),Avg_Rev_Res=round(mean(sum(Tot_Rev)/sum(Tot_Res)),2))

#get total reservation counts and revenue by month
#mem_summary <- rental.history.all %>%
#  group_by(RES_YEAR, RES_MONTH) %>%
#  summarise(TOT_RES=sum(TOT_RES),TOT_REV=sum(TOT_REVENUE)) 

#mem.history<-rfm.mem.performance.current.period.firstres
#mem.history$SnapshotDate<-"2015-2"

#build a dataframe of scores by member month by month
mem.his.curr<-rfm.mem.performance.current.period.firstres
mem.his.curr$SnapshotDate<-"2016-1"

mem.history<-rbind(mem.history,mem.his.curr)
rm(mem.his.curr)

#save(mem.history, file="C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Member Reservation Source Profile\\RData\\Member Monthly RFM Score Snapshot.RData")
load("C:\\Users\\babbenante\\documents\\My Stuff\\Data\\Member Reservation Source Profile\\RData\\Member Monthly RFM Score Snapshot.RData")
mem.history<-data.table(mem.history)
mem.history.x<-mem.history %>%
  .[,SnapshotDate := ordered(SnapshotDate,levels=c("2015-1","2015-2","2015-3","2015-4","2015-5","2015-6","2015-7","2015-8","2015-9","2015-10","2015-11","2015-12","2016-1"))]   

#mem.history.x<-mem.history.x %>% complete(SnapshotDate,MEMBER_ID,fill=list(Score=99))
mem.history.x<-spread(mem.history.x,SnapshotDate,Score)


#Number of Members by Number of RFM segments per year
mem.history.Seg.Num<-mem.history %>%
  group_by(MEMBER_ID) %>%
  summarise(MEM_TYPES=n_distinct(Total_Score)) %>%
  group_by(MEM_TYPES) %>%
  summarise(n_distinct(MEMBER_ID))




x<-member.history[which(member.history$MEMBER_ID==1986032)]


#####################################################################################################	
# 	Project: 	Census Gender Data																	#
#	Author: 	Jeremy Guinta																		#
#	Date: 																							#
#	Script: 	001_mk_data.r																		#
#	Purpose: 																						#
#####################################################################################################

# I. Program Setup  -------------------------------------------------------

#	Remove Objects
rm(list=ls())

# Clear Memory
gc(reset=TRUE)

# Set Working Directory
setwd("//wdc1islfls02/CHI1FLS02_TSP/LosAngeles/Admin/001_Users/jjg/NameGender/")

#Load Packages
require(data.table)
require(tidyverse)
require(dtplyr)
require(stringr)
require(reshape2)
require(progress)

#Set Options
options(scipen=30) #Sets numeric width to 30 before scientific notation starts

setDTthreads(threads = 4)

#II. Data Loading ------------------------------------------------------------------------------
#A. Territory
terr<-dir("./")[grepl("[A-Z][A-Z].TXT", dir("./"))==TRUE] #Territory 

pb<-progress_bar$new(total=length(terr))

j<-1
for (i in terr) {
	tmp<-fread(paste("./", i, sep=""))
	names(tmp)<-c("state", "gender", "year", "firstname", "count")
	tmp[, src:=i]
	
	if (j ==1) {
		out<-tmp
	}
	else {
		out<-rbind(out, tmp)
	}
	j<-j+1
	pb$tick()
	
}

terr<-out[, .(state, gender, year, firstname, count)]
rm(out)

#B. Year - Fill in when "Unknown" in Territory if overall gives a clear indication of gender
yob<-dir("./")[grepl("yob", dir("./"))==TRUE] #Year of Birth

pb<-progress_bar$new(total=length(terr))

j<-1
for (i in yob) {
	tmp<-fread(paste("./", i, sep=""))
	names(tmp)<-c("firstname", "gender", "count")
	tmp[, src:=i]
	tmp[, year:=gsub("yob", "", src)]
	tmp[, year:=gsub("\\.txt", "", year)]
	tmp[, year:=as.numeric(year)]
	
	if (j ==1) {
		out<-tmp
	}
	else {
		out<-rbind(out, tmp)
	}
	j<-j+1
	pb$tick()
	
}

yob<-out[, .(gender, year, firstname, count)]
rm(out)


#III. Data Processing --------------------------------------------------------------------------
#A. Territory
	#1.   Flip data wide 
	terr_m<-terr[gender=="M", .(state, year, firstname, count_m=count)]
	terr_f<-terr[gender=="F", .(state, year, firstname, count_f=count)]
	
	terr_w<-merge(terr_m, terr_f, by=c("state", "year", "firstname"),all=TRUE)
	terr_w<-as.data.frame(terr_w)
	terr_w[is.na(terr_w)]<-0
	
	terr_w<-as.data.table(terr_w)
		
	#2. Build Gender Classification for each Name 
	terr_w[, prop_m:=count_m/(count_m+count_f)]
	terr_w[prop_m==1, gender:="Male"]
	terr_w[prop_m==0, gender:="Female"]
	terr_w[is.na(gender)==TRUE, gender:="Unknown"]
	# terr_w[count_m+count_f<20 & prop_m<0.90 & prop_m>=0.70, gender:="Unknown"]  #Adjusting records in which there is not enough data to make a solid determination
	# terr_w[count_m+count_f<20 & prop_m<0.30 & prop_m>=0.10, gender:="Unknown"]
	terr_w[, .N, gender]
		# gender       N
	# 1:  Female 3157491
	# 2:    Male 2466928
	# 3: Unknown  214013
	
#B. Year of Birth
	#1.   Flip data wide 
	yob_m<-yob[gender=="M", .(year, firstname, count_m=count)]
	yob_f<-yob[gender=="F", .(year, firstname, count_f=count)]
	
	yob_w<-merge(yob_m, yob_f, by=c("year", "firstname"),all=TRUE)
	yob_w<-as.data.frame(yob_w)
	yob_w[is.na(yob_w)]<-0
	
	yob_w<-as.data.table(yob_w)
		
	#2. Build Gender Classification for each Name 
	yob_w[, prop_m:=count_m/(count_m+count_f)]
	yob_w[prop_m==1, gender:="Male"]
	yob_w[prop_m==0, gender:="Female"]
	yob_w[is.na(gender)==TRUE, gender:="Unknown"]
	# yob_w[count_m+count_f<20 & prop_m<0.90 & prop_m>=0.70, gender:="Unknown"]  #Adjusting records in which there is not enough data to make a solid determination
	# yob_w[count_m+count_f<20 & prop_m<0.30 & prop_m>=0.10, gender:="Unknown"]
	yob_w[, .N, gender]
		# gender      N
	# 1:    Male 629548
	# 2:  Female 985556
	# 3: Unknown 170971
	
	names(yob_w)<-c("year", "firstname", "national_count_m", "national_count_f", "national_prop_m", "national_gender")
	
#C. Name Final
	#Compare Territory to Year of Birth
	terr_w[, m:="territory"]
	yob_w[, m:="yob"]
	name_gender<-merge(terr_w, yob_w, by=c("year", "firstname"), all.x=TRUE)
			 # m.x  m.y       N
	# 1: territory  yob 5834569
	# 2: territory <NA>    3863	
	name_gender<-name_gender[m.x=="territory" & m.y=="yob",]
	
	#2. Adjust names in which national disagree with territory
	name_gender[, gender_final:=gender]
	name_gender[gender=="Unknown" & national_gender!="Unknown", gender_final:=national_gender]
	
#IV. Data Output -------------------------------------------------------------------------------
	name_gender<-name_gender[, .(year, state, firstname, count_m, count_f, prop_m, gender, national_count_m, national_count_f, national_prop_m, national_gender, gender_final)]
	save(file="./001_name_gender.rda", name_gender)



#####################################################################################################	
# 	Project: 	Census Gender Data																	#
#	Author: 	Jeremy Guinta																		#
#	Date: 																							#
#	Script: 	002_mk_features.r																	#
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
require(stringi)
require(reshape2)
require(syllable)
require(ggplot2)

#Set Options
options(scipen=30) #Sets numeric width to 30 before scientific notation starts

setDTthreads(threads = 4)

# II. Data Loading ----------------------------------------------------------

load("./001_name_gender.rda")

name_gender<-name_gender[gender_final!="Unknown", ]  #Model is conducted on clearly gender typed names

# III. Data Processing 
	#A. Remove features not suitable for prediction
	name_gender[, c("count_m", "count_f", "prop_m", "national_count_m", "national_count_f", "national_prop_m", "national_gender", "gender"):=NULL]
	
	#B. Build Features from name
		#1. Name Length
			name_gender[, name_length:=str_length(firstname)]
		
		#2. Name Count (number of words in the name)
			name_gender[, name_count:=str_count(firstname, fixed(' '))]  #For some unknown reason ssa smushes names together. 
		
		#3. Number of syllables
			name_gender[, syllable_count:=count_vector(firstname)]
			name_gender[, syllable_count:=as.numeric(unlist(syllable_count))]
			
		#4. Ends in a vowel
			name_gender[, end_in_a_vowel:="no"]
			name_gender[grepl("a|e|i|o|u", substr(stri_reverse(firstname),1,1))==TRUE, end_in_a_vowel:="yes"]
			name_gender[, end_in_a_vowel:=as.factor(end_in_a_vowel)]
		
		#5. First Initial 
			name_gender[, finit:=as.factor(substr(firstname,1,1))]
		
		#6. First Inital vowel
			name_gender[, start_with_a_vowel:="no"]
			name_gender[finit %in% c("A", "E", "I", "O", "U"), start_with_a_vowel:="yes"]
			name_gender[, start_with_a_vowel:=as.factor(start_with_a_vowel)]
			
		#7. Age (to present day) 
			name_gender[, age:=2018-as.numeric(as.character(year))]
					
	#C. Other Features
		name_gender[, year:=as.factor(year)]
		name_gender[, state:=as.factor(state)]

# IV. Data Analysis
	#A. Basic Summaries
		
	
	#B. Graphics
		#1. Syllable
		plt<-ggplot(name_gender, aes(x=as.factor(gender_final), y=syllable_count))+geom_boxplot(fill=c("pink", "light blue"))
		plt<-plt+labs(title="Boxplot of Gender by Number of Syllables", x="Gender", y="Syllable Count") + theme_classic() + theme(legend.position="bottom", legend.title=element_blank())	
		plt1<-plt

		#2. Name length
		plt<-ggplot(name_gender, aes(x=as.factor(gender_final), y=name_length))+geom_boxplot(fill=c("pink", "light blue"))
		plt<-plt+labs(title="Boxplot of Gender by Name Length", x="Gender", y="Name Length") + theme_classic() + theme(legend.position="bottom", legend.title=element_blank())	
		plt2<-plt
		
		#3. End in Vowel 
		tbl<-name_gender[, .N, by=list(gender_final, end_in_a_vowel)]
		tbl[, pct:=N/sum(N), by=list(end_in_a_vowel)]
		plt<-ggplot(tbl, aes(x=end_in_a_vowel, y=pct, group=gender_final, fill=gender_final))+geom_bar(stat="identity", position="stack")
		plt<-plt+scale_fill_manual(values=c("pink", "light blue"))
		plt<-plt+labs(title="Stacked Barplot of Gender by Number of End in a Vowel", x="End in a Vowel", y="Count")
		plt3<-plt+theme_classic() + theme(legend.position="bottom", legend.title=element_blank())	
			
		#4. First Initial
		tbl<-name_gender[, .N, by=list(gender_final, finit)]
		tbl[, pct:=N/sum(N), by=list(finit)]
		plt<-ggplot(tbl, aes(x=finit, y=pct, group=gender_final, fill=gender_final))+geom_bar(stat="identity", position="stack")
		plt<-plt+scale_fill_manual(values=c("pink", "light blue"))
		plt<-plt+labs(title="Stacked Barplot of Gender by First Initial", x="First Initial", y="Count")
		plt4<-plt+theme_classic() + theme(legend.position="bottom", legend.title=element_blank())	
		
		#5. Start with Vowel
		tbl<-name_gender[, .N, by=list(gender_final, start_with_a_vowel)]
		tbl[, pct:=N/sum(N), by=list(start_with_a_vowel)]
		plt<-ggplot(tbl, aes(x=start_with_a_vowel, y=pct, group=gender_final, fill=gender_final))+geom_bar(stat="identity", position="stack")
		plt<-plt+scale_fill_manual(values=c("pink", "light blue"))
		plt<-plt+labs(title="Stacked Barplot of Gender by Start with a Vowel", x="Start with a Vowel", y="Count")
		plt5<-plt+theme_classic() + theme(legend.position="bottom", legend.title=element_blank())			
		
		#6. State
		tbl<-name_gender[, .N, by=list(gender_final, state)]
		tbl[, pct:=N/sum(N), by=list(state)]
		plt<-ggplot(tbl, aes(x=state, y=pct, group=gender_final, fill=gender_final))+geom_bar(stat="identity", position="stack")
		plt<-plt+scale_fill_manual(values=c("pink", "light blue"))
		plt<-plt+labs(title="Stacked Barplot of Gender by State", x="State", y="Count")
		plt6<-plt+theme_classic() + theme(legend.position="bottom", legend.title=element_blank())	
		
		#7. Age
		plt<-ggplot(name_gender, aes(x=as.factor(gender_final), y=age))+geom_boxplot(fill=c("pink", "light blue"))
		plt<-plt+labs(title="Boxplot of Gender by Age", x="Gender", y="Age") + theme_classic() + theme(legend.position="bottom", legend.title=element_blank())	
		plt7<-plt	
		
		#8. Syllables by Age
		plt<-ggplot(name_gender, aes(age, syllable_count, color=gender_final))+geom_point(alpha=0.2)+theme_classic()+geom_jitter()
		plt<-plt+labs(title="Scatter of Age and Syllable Count, by Gender", x="Age", y="Syllable")
		plt<-plt+scale_color_manual(values=c("pink", "light blue"))+theme(legend.position="bottom", legend.title=element_blank())
		plt8<-plt

		#9. Name Length by Age
		plt<-ggplot(name_gender, aes(age, name_length, color=gender_final))+geom_point(alpha=0.2)+theme_classic()+geom_jitter()
		plt<-plt+labs(title="Scatter of Age and Name Length, by Gender", x="Age", y="Name Length")
		plt<-plt+scale_color_manual(values=c("pink", "light blue"))+theme(legend.position="bottom", legend.title=element_blank())
		plt9<-plt
		
# V. Data Output
	name_gender_features<-name_gender
	save(file="./002_name_gender_features.rda", name_gender_features)
	
	ggsave("./002_plot1.png", plt1, height=8, width=11)
	ggsave("./002_plot2.png", plt2, height=8, width=11)
	ggsave("./002_plot3.png", plt3, height=8, width=11)
	ggsave("./002_plot4.png", plt4, height=8, width=11)
	ggsave("./002_plot5.png", plt5, height=8, width=11)
	ggsave("./002_plot6.png", plt6, height=8, width=11)
	ggsave("./002_plot7.png", plt7, height=8, width=11)
	ggsave("./002_plot8.png", plt8, height=8, width=11)
	ggsave("./002_plot9.png", plt9, height=8, width=11)
		
		
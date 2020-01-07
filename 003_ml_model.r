#####################################################################################################
#Engagement		-	Machine Learning Name Gender Analysis											#
#FileName		-								  													#
#By				- 	Jeremy Guinta 																	#
#																	  								#
#Last Update Date:	11/25/2019									  									#
#																	  								#
#Purpose:		-	Modeling 																		#											
#				- 	Use Model to predict the "Unknown" Category 									#	
#				-	Train / Test / Hold out 60% / 20% / 20% split. 									#
#																									#
#####################################################################################################



#I. Setup -------------------------------------------------------------------------------------------

	#Remove Objects
	rm(list=ls())

	#Clear Memory
	gc(reset=TRUE)
	
	#Set Working Directory
	setwd("//wdc1islfls02/CHI1FLS02_TSP/LosAngeles/Admin/001_Users/jjg/NameGender/")

	#Package Install
	require(tidyverse)		#All things tidy 
	require(data.table)		#Data table is better
	require(dtplyr)			#Make sure Data table and dplyr work together
	require(ggplot2)		#Graphing Utilities
	require(stringr)		#String Functions
	require(reshape2)		#Data Reshape
	require(h2o)			#Auto ML
	require(openxlsx)		#Excel Tools

	#Set Options
	options(scipen=20)

	#Graphics
	out_theme <- theme_bw() + 
		  theme(panel.grid.major=element_line(color="white"), 
				text=element_text(family="ArialMT"), 
				legend.position="bottom",
				plot.title = element_text(size = rel(1.0)),
				axis.text.x = element_text(size= rel(1.0)),
				axis.text.y = element_text(size= rel(1.0)))
				
				
	color_scheme <- c("#6495ED", "#C90E17", "#001933", "#691b14", "#08519c", "#778899", "#B0C4DE", 
							  "#999999", "#000000",  "#800000", "#B23232")   	

							  
							  
	#Functions
	true_rmsle<-function(dta=c(), model=c(), response=c(), tp=c("")) {
		tmp<-get(dta) %>% as.data.table()
		mod<-get(model) %>% as.data.table()
		tmp<-cbind(as.data.table(tmp), as.data.table(mod))
		
		if (tp!="log" & tp!="sqrt" & tp!="nominal") {
			stop('Must select either "log," "sqrt," or "nominal"')
		}
		else {		
			if(tp=="log") {
				out<-tmp[, .(sp=exp(get(response)), spp=exp(mod))]
			}
			if(tp=="sqrt") {
				out<-tmp[, .(sp=get(response)^2, spp=mod^2)]
			}
			if(tp=="nominal") {
				out<-tmp[, .(sp=(get(response)), spp=mod)]
			}
			out<-out[, sqrt(mean((log(sp+1)-log(spp+1))^2))]	
			return(out)
		}
	}	
	
	format_finder<-function(.dta=c(""), type=c("")) {
		
		require(tidyverse)
		require(data.table)
		
		x<-get(.dta)
		x<-as.data.frame(x)
		x<-capture.output(str(x, list.len=ncol(x)))
		x<-x[grepl(type, x)==TRUE] %>% as.data.table()
		names(x)<-"nm"
		x[, val:=substr(nm, regexpr("\\$" , nm), regexpr(":", nm))]
		x[, val:=gsub("\\$", "", val)]	
		x[, val:=gsub("\\:", "", val)]	
		x[, val:=str_trim(val)]
		
		x<-x[val!="", .(val)][order(val)] %>% as.matrix() %>% as.vector()
		
		return(x)
	}	
							  
#II.  Data Loading ---------------------------------------------------------------------------------

#A. All data
load("./name_gender_features.rda")

#III. Data Processing ---------------------------------------------------------------------------
name_gender_features[, syllable_count:=as.numeric(unlist(syllable_count))]
name_gender_features[, gender_final:=as.factor(gender_final)]

#IV. Data Analysis ------------------------------------------------------------------------------

#A. Train / Test Sets

tot<-nrow(name_gender_features[gender_final!="Unknown",])
tmp<-name_gender_features[gender_final!="Unknown",]

cnt_trn<-round(tot*0.60,0)
cnt_tst<-round(tot*0.20,0)
cnt_hld<-round(tot*0.20,0)

stopifnot(cnt_trn+cnt_tst+cnt_hld==tot)

set.seed(2019)
trn<-sample_n(tmp, cnt_trn)

set.seed(2019)
tst<-sample_n(tmp, cnt_tst)

set.seed(2019)
hol<-sample_n(tmp, cnt_hld)

stopifnot(nrow(trn)+nrow(tst)+nrow(hol)==tot)

#B. Set h2o for modeling
	
	#1. Prepare the data for h2o
	setwd("C:/h2o/gender")  	#The network pathways are too long.  Setting directory to local C:/h2o
						#All h2o objects will be saved here	
	
	h2o.init(nthreads=4, min_mem_size="16G")	

	#2. Load into h2o
	trn<-as.h2o(trn)
	tst<-as.h2o(tst)
	fnl<-as.h2o(hol)
	
	#3. Set parameters
	
	#Generalize Linear Models (Binomial) - GLM
	hyper_params_glm <- list(
		  alpha = c(0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95)
		)	

	#GBM
	hyper_params_gbm <- list(
	  ntrees = c(5, 10, 25, 50,100,150,200), 
	  max_depth = c(5, 10,15,20,25,30,50,100), 
	  min_rows = c(2, 5,10,15,20,40,80,120), 
	  learn_rate = c(.06,.07,.08,.09,.1,.11,.12,.15),
	  sample_rate = c(0.80,0.90,.95, .975,.99,.995,1),
	  col_sample_rate = c(.1,.2,.3,.4,.5,.6,.7,.8,.9),
	  col_sample_rate_per_tree = c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1),
	  nbins_cats = c(16,32,64,128,256),
	  learn_rate_annealing=c(0.05,0.10,0.25,0.5,0.75, 1)	
	)
	
	#Random Forest
	hyper_params_rf <- list(
	  ntrees = c(5, 10, 25,50,75,100,150,200), 
	  max_depth = c(10,15,20), 
	  min_rows = c(5,10,30,70,100), 
	  sample_rate = c(.95, .975,.99,.995,1),
	  col_sample_rate_per_tree = c(.5, .6,.7,.8,.9,1),
	  nbins=c(2,5,10,15,20),
	  mtries=c(-1,-2, 3,4,5,6),
	  nbins_cats = c(64, 128, 256, 512,1024,1536)
	)	

	#DRF
	hyper_params_drf <- list(
	  ntrees = c(5, 10, 25,50,75,100,150,200), 
	  max_depth = c(10,15,20), 
	  min_rows = c(5,10,30,70,100), 
	  sample_rate = c(.95, .975,.99,.995,1),
	  col_sample_rate_per_tree = c(.5, .6,.7,.8,.9,1),
	  nbins=c(2,5,10,15,20),
	  mtries=c(-1,-2, 3,4,5,6),
	  nbins_cats = c(64, 128, 256, 512,1024,1536)
	)		

	#NN
	hyper_params_nn <- list(
	  epochs=c(5,10,15,20,25,30),
	  overwrite_with_best_model=FALSE,
	  hidden=list(c(32,32,32),c(64,64,64),c(128,128,128), c(16,16,16)),
	  max_w2=10,
	  score_duty_cycle=0.025,
	  activation=c("Rectifier","Tanh","TanhWithDropout"),
	  input_dropout_ratio=c(0,0.05),
	  score_validation_samples=10000,
	  l1=c(.00001,.000001,.0000001),
	  l2=c(.00001,.000001,.0000001),
	  rho = c(.99,.975,1,0.95),
	  rate=c(.005,.0005,.00005),
	  rate_annealing=c(.00000001,.0000001,.000001),
	  momentum_start=c(.5,.1,.01,.05,.005),
	  momentum_stable=c(0.1, 0.2, 0.3, 0.4,0.5), 
	  momentum_ramp=c(1000000,100000)
	)	
	
	#Search Criteria
	search_criteria <- list(
	  strategy = "RandomDiscrete",
	  max_runtime_secs = 30*60,  #60 minutes per run
	  max_models = 500
	)	
	
	#X variables 
		xnames<-c("year", "state", "name_length", "syllable_count", "end_in_a_vowel", "finit", "start_with_a_vowel")

#C. Conduct Models		
	#1. Random Forest
		#a. Model
		rf1 <- h2o.grid(algorithm = "randomForest", 
							x = xnames, y = "gender_final",  
							training_frame = trn, 
							hyper_params = hyper_params_rf,
							search_criteria = search_criteria,
							stopping_metric = "misclassification", stopping_tolerance = 1e-3, 
							stopping_rounds = 5,
							seed = -1,
							nfolds = 5, fold_assignment = "Modulo", 
							keep_cross_validation_predictions = TRUE
			)
			
		rf1_sort <- h2o.getGrid(grid_id = rf1@grid_id, sort_by = "auc", decreasing = TRUE)
			
		rf1_best <- h2o.getModel(rf1_sort@model_ids[[1]])
		pref_rf1<-h2o.performance(rf1_best, newdata=tst)	
		
		#b. Prediction
		pred_rf1<-h2o.predict(rf1_best, newdata = trn)
		
	#2. Distributed Random Forest
		#a. Model
		drf1 <- h2o.grid(algorithm = "drf", 
							x = xnames, y = "gender_final", 
							training_frame = trn, 
							hyper_params = hyper_params_drf,
							search_criteria = search_criteria,
							stopping_metric = "misclassification", stopping_tolerance = 1e-3, 
							stopping_rounds = 5,
							seed = -1,
							nfolds = 5, fold_assignment = "Modulo", 
							keep_cross_validation_predictions = TRUE
			)
			
		drf1_sort <- h2o.getGrid(grid_id = drf1@grid_id, sort_by = "auc", decreasing = TRUE)
			
		drf1_best <- h2o.getModel(drf1_sort@model_ids[[1]])
		pref_drf1<-h2o.performance(drf1_best, newdata=tst)	

		#b. Prediction
		pred_drf1<-h2o.predict(drf1_best, newdata = trn)
	
	#3. GLM
		#a. Model 
	    glm1 <- h2o.grid(algorithm = "glm", 
                        x = xnames, y = "gender_final", 
						training_frame = trn,
                        hyper_params = hyper_params_glm,
                        search_criteria = search_criteria,
                        stopping_metric = "misclassification", stopping_tolerance = 1e-3, 
                        stopping_rounds = 3,
                        seed = 1,
                        nfolds = 5, fold_assignment = "Modulo", 
                        keep_cross_validation_predictions = TRUE,
						family = "binomial",
						lambda_search=TRUE 
                        )
						
        glm1_sort <- h2o.getGrid(grid_id = glm1@grid_id, sort_by = "auc", decreasing = TRUE)
        glm1_sort

        glm1_best <- h2o.getModel(glm1_sort@model_ids[[1]])
        summary(glm1_best)	

		#b. Prediction
		pred_glm1<-h2o.predict(glm1_best, newdata = tst)
		pref_glm1<-h2o.performance(glm1_best, newdata=tst)		

#G. Graphics
	#1. Individual Models
	rocr_rf<-data.frame(fpr=pref_rf1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_rf1@metrics$thresholds_and_metric_scores$tpr)
	rocr_rf<-as.data.table(rocr_rf)
	rocr_rf<-rbind(data.table(fpr=0.00, tpr=0.00),rocr_rf)
	rocr_rf[, tp:="Random Forest"]
	
	rocr_drf<-data.frame(fpr=pref_drf1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_drf1@metrics$thresholds_and_metric_scores$tpr)
	rocr_drf<-as.data.table(rocr_drf)
	rocr_drf<-rbind(data.table(fpr=0.00, tpr=0.00),rocr_drf)
	rocr_drf[, tp:="Distributed Random Forest"]
		
	rocr_glm<-data.frame(fpr=pref_glm1@metrics$thresholds_and_metric_scores$fpr, tpr=pref_glm1@metrics$thresholds_and_metric_scores$tpr)
	rocr_glm<-as.data.table(rocr_glm)
	rocr_glm[, tp:="Binomial Regression"]
	
	rocr_all<-rbind(rocr_rf, rocr_drf, rocr_glm)
	
	#ROCR Curve
	plt<-ggplot(rocr_all, aes(fpr,tpr, color=tp, group=tp)) + geom_line()+theme_bw()
	plt<-plt+geom_abline(intercept=0, color="red")
	plt<-plt+scale_color_manual(values=c(color_scheme))
	plt<-plt+labs(colour = "Default", x="False Positive Rate", y="True Positive Rate", title="ROCR Curve - Gender Name")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))	
	ens1_graph3<-plt

	#ROCR Curve
	plt<-ggplot(rocr_all, aes(fpr,tpr)) + geom_line()+theme_bw()
	plt<-plt+facet_wrap(~tp)
	plt<-plt+geom_abline(intercept=0, color="red")
	plt<-plt+scale_color_manual(values=c(color_scheme))
	plt<-plt+labs(colour = "Default", x="False Positive Rate", y="True Positive Rate", title="ROCR Curve - Gender Name")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))	
	ens1_graph4<-plt
	
#H. Summary Output
	#1. Confusion Matrices
	
	glm1_conf_matrix<-as.data.table(h2o.confusionMatrix(glm1_best, newdata=tst))
	glm1_conf_matrix[, tp:="GLM"]
	rf1_conf_matrix<-as.data.table(h2o.confusionMatrix(rf1_best, newdata=tst))
	rf1_conf_matrix[, tp:="Random Forest"]
	drf1_conf_matrix<-as.data.table(h2o.confusionMatrix(drf1_best, newdata=tst))
	drf1_conf_matrix[, tp:="Distributed Random Forest"]
	
	conf_matrix<-rbind(glm1_conf_matrix,rf1_conf_matrix,drf1_conf_matrix)
	conf_matrix<-conf_matrix[, .(tp, Female, Male, Error, Rate)]
	
	#2. AUC output
	
	glm1_auc<-as.data.table(h2o.auc(pref_glm1))
	glm1_auc[, tp:="GLM"]
	rf1_auc<-as.data.table(h2o.auc(pref_rf1))
	rf1_auc[, tp:="Random Forest"]
	drf1_auc<-as.data.table(h2o.auc(pref_drf1))
	drf1_auc[, tp:="Distributed Random Forest"]

	auc_tbl<-rbind(glm1_auc, rf1_auc, drf1_auc)
	auc_tbl<-auc_tbl[, .(tp, auc=V1)]
	
	#3. Variable Importance
	drf_imp<-h2o.varimp(drf1_best) %>% as.data.table()
	drf_imp_m<-melt(drf_imp[, .(variable, scaled_importance, percentage)], id.vars=c("variable"))
	names(drf_imp_m)<-c("var", "type", "value")
		
	plt<-ggplot(drf_imp_m, aes(x=var, y=value, fill=type))+geom_bar(stat="identity", position="dodge")+theme_classic()
	plt<-plt+labs(title="Variable Importance", subtitle="Distributed Random Forest", x="Variable", y="%")
	plt<-plt+scale_fill_manual(values=c("blue", "red"))
	plt<-plt+theme(legend.position="bottom", legend.title=element_blank())
		
#V. Data Output

#A. Save Models
	rf1_best_save <- h2o.saveModel(
	  object = rf1_best,
	  path = "C:/h2o/gender/rf1.h2o", 
	  force =TRUE
	)		
	
	drf1_best_save <- h2o.saveModel(
	  object = drf1_best,
	  path = "C:/h2o/gender/drf1.h2o", 
	  force =TRUE
	)		

	glm1_best_save <- h2o.saveModel(
	  object = glm1_best,
	  path = "C:/h2o/gender/glm1.h2o", 
	  force =TRUE
	)

	save(file="C:/h2o/gender/model_paths.h2o",rf1_best_save,drf1_best_save,glm1_best_save)

#B. Sample Testing
	hol<-as.h2o(hol)
	pred_drf1<-h2o.predict(drf1_best, newdata = hol)  #Apply predictions to holdout data
	tst_dta<-as.data.table(cbind(as.data.table(hol), as.data.table(pred_drf1)))
	saveRDS(file="./003_test_predictions.rds", tst_dta)

#C. "Unknown" Values
	unk<-name_gender_features[gender_final=="Unknown",]
	unk[, gender_final:=NULL]
	unk<-as.h2o(unk)
	pred_drf1<-h2o.predict(drf1_best, newdata=unk) #There is no benchmark comparison
	unk_dta<-as.data.table(cbind(as.data.table(unk), as.data.table(pred_drf1)))
	saveRDS(file="./003_unk_predictions.rds", unk_dta)
	
#D. Graphs
	ggsave(file="./003_rocr1.png", ens1_graph3, height=8, width=11)
	ggsave(file="./003_rocr2.png", ens1_graph4, height=8, width=11)
	ggsave(file="./003_varimp.png", plt, height=8, width=11)
	
#E. Summary output
	wb <- createWorkbook()
	addWorksheet(wb,"003_confmatrix")
	addWorksheet(wb,"003_auc")
	writeData(wb, "003_confmatrix", conf_matrix)
	writeData(wb, "003_auc", auc_tbl)
	saveWorkbook(wb, "./003_ConfusionMatrix.xlsx", overwrite=TRUE)
	




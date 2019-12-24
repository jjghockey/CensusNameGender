# Census Name Gender
Repo for building and analyzing US Census Name mapping to Gender.  Source: https://www.ssa.gov/OACT/babynames/limits.html

As the data and analysis are built out, I will add to the description.  As of now (12/17/2019), this is a sandbox. 

I wanted to explore an interesting challenge that is facing companies that use machine learning to assess credit worthiness or employment based on non-gender factors.  Some of these algorithsm have been shown to be biased and discriminatory even though gender was never part of algorithm (see the Apple Card and Goldman Sachs, https://observer.com/2019/11/goldman-sachs-bias-detection-apple-card/). 

Typically, companies are NOT gathering gender information, but in order to evaluate algorithm assigning gender (Male vs. Female) to the data is essential.  One way of doing this is to use US Census data.  This data counts up the number of times a gender appear for each first name of an individual based on their birth year and territory.  

There are many names that are clearly gender typed and always appear associated as Male or associated as Female.  However, many names are not clearly male or female.  

The purpose of this repository is to develop a model that could be used to determine the gender of a name based on the characteristics of names. 

Script Directory

1. 001_mk_data.r - Creates name_gender.rda (This is the SSA file downloaded from https://www.ssa.gov/OACT/babynames/limits.html)
   The data is split into territory data (organized by state or US territory, and year of birth) and year of birth data 
   (organized by year of birth).  Each observation contains a first name and the count of gender for individuals with that name.
  
  This script processes the data and creates a gender tag for each name found in the data based the following: 
    Male - All names for the first name were assigned to a male in a given year / territory
    Female - All names for the first name were assiged to a female in a given year / territory
    Unknown - Remaining names for a territory and year that were not assigned (This is the target for prediction)
  
    
2. 002_mk_features.r - Creates name_gender_features.rda.  This takes the data created in 001 and builds out features for use in
the model.  The script also build plots with interesting breakdowns of the data. 

  plot1 -  Boxplot of Syllables by Gender 
  plot2 -  Boxplot of Name Length By Gender 
  plot3 -  Last Character in the name by Gender
  plot4 -  First Initial by Gender
  plot5 -  Start with a Vowel by Gender
  plot6 -  State by Gender

3. 003_ml_model.r - Actual Modeling
  -60/20/20 training, test, holdout set process is used to evaluate model fit. 
  
  -Key features: year (factor), territory (factor), name length (continuous), number of syllables (continuous), 
  end with a vowel (factor), first initial (factor), last character (factor)
  
  -Outcome: binary male/female label (with a probability of that label)
  
  -Initial evaluation: AUC with ROCR curve
  
  -Final model will be compared to Unknown category.  Evaluation will be the probability of which gender as assigned by 
  the model versus a simple binary yes/no based on the proportion of records assigned male vs female. 

4. 004_ml_explain.r - Explanability of the features (TBD)
  - Simple variable importance of the features
  - LIME
  - SHAP 
  

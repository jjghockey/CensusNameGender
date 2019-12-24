# Census Name Gender
Repo for building and analyzing US Census Name mapping to Gender.  Source: https://www.ssa.gov/OACT/babynames/limits.html

As the data and analysis are built out, I will add to the description.  As of now (12/17/2019), this is a sandbox. 

I wanted to explore an interesting challenge that is facing companies that use machine learning to assess credit worthiness or employment based on non-gender factors.  Some of these algorithsm have been shown to be biased and discriminatory even though gender was never part of algorithm (see the Apple Card and Goldman Sachs, https://observer.com/2019/11/goldman-sachs-bias-detection-apple-card/). 

Typically, companies are NOT gathering gender information, but in order to evaluate algorithm assigning gender (Male vs. Female) to the data is essential.  One way of doing this is to use US Census data.  This data counts up the number of times a gender appear for each first name of an individual based on their birth year and territory.  

There are many names that are clearly gender typed and always appear associated as Male or associated as Female.  However, many names are not clearly male or female.  

The purpose of this repository is to develop a model that could be used to determine the gender of a name based on the characteristics of names that are clearly gender typed. 

#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 5								
#	TOPIC - "ggplot"                                STUDEBT ID: 19230107
#----------------------------------------------------------------------------------


library(readxl)
library(ggplot2)

# Reading the excel file from local directory
original_df = data.frame(readxl::read_excel('T:\\NUIG DA\\SEM_1\\Programming for DA\\titanic3.xls'))

p_df = original_df
dim(p_df)

summary(p_df)

# Part 1 -- Preprocessing


# Coverting ppl survived into logical vector ( 1 = T , 0 = F) 
p_df$survived = apply(subset(p_df,select = c(survived)),2,function(x) ifelse(x == 1,T,F) )
summary(p_df)

# Type casting Passenger class column

p_df$pclass = sapply(p_df$pclass,function(x) ifelse (x==1,'First',ifelse(x==2,'Second','Third') ))
summary(p_df)
unique(p_df$pclass)

# Imputing age column  with mean ages
p_df$age = apply(subset(p_df,select = c(age)),2,function(x) ifelse(is.na(x),mean(x,na.rm = T),x))
summary(p_df)

# Imputing fare column  with mean fare
p_df$fare = apply(subset(p_df,select = c(fare)),2,function(x) ifelse(is.na(x),mean(x,na.rm = T),x))
summary(p_df)



# Categorical imputation using random sampling [ used row subsetting to identify the rows with NA values] 
set.seed(99)

p_df[is.na(p_df$embarked),"embarked"] = sample(c('S','C','Q'),sum(is.na(p_df$embarked)))
summary(p_df)
unique(p_df$embarked)


# New Column mutation based on age limit - - As we have imputed null values explicitly using 'Elderly' for else scenatio 
p_df$age_cohort = sapply(p_df$age,function(x) ifelse(x < 16,'Child',ifelse(x < 60,'Adult','Elderly')))
summary(p_df)


# Replacing town origin with their full names - As we have imputed null values explicitly using 'Cobh' for else scenario 

p_df$embarked = sapply(p_df$embarked,function(x) ifelse (x=='S','Southampton',ifelse(x=='C','Cherbourg','Cobh') ))
summary(p_df)
unique(p_df$embarked)

#Cross checking
head(p_df)

dim(p_df)

table(p_df$survived)

table(p_df$survived,p_df$age_cohort)

table(p_df$survived,p_df$sex)

table(p_df$survived,p_df$pclass)

table(p_df$survived,p_df$embarked)


# Part 2 -- Plotting (Note : In each plot legends are closely placed for exactly relicating graph shown in the assignment)

# Survival Numbers by travel class [Bar plot] + [Changed legend position and removed title]
plt1 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = pclass))+ labs(title="Survival Numbers by Travel Class",x='Survived',y='Number') 

plt1 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) 


# Survival Numbers by Gender [Bar plot]  + [Changed legend position and removed title]
plt2 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = sex))+ labs(title="Survival Numbers by Gender",x='Survived',y='Number') 

plt2 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) 


# Survival Numbers by Age Cohort  [Bar plot]  + [ Changed legend position and removed title]
plt3 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = age_cohort))+ labs(title="Survival Numbers by Age Cohort",x='Survived',y='Number') 

plt3 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) 


#Survival Numbers by Embarkation Location [Bar plot]  + [ Changed legend position and removed title]
plt4 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = embarked))+ labs(title="Survival Numbers by Embarkation Location",x='Survived',y='Number') 

plt4 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) 



#Survival Proportions by Travel class [Bar plot]  + [Filled the bar to the fullest] + [Changed legend  position and removed title] + [scaled y axis] 
plt5 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = pclass),position="fill")+ labs(title="Survival Proportions by Travel class",x='Survived',y='Proportion') 

plt5   + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) + scale_y_continuous(breaks = seq(0, 1.0, 0.25))


#Survival Proportions by Gender [Bar plot]  + [Filled the bar to the fullest] + [Changed legend position and removed title] + [scaled y axis] 
plt6 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = sex),position="fill")+ labs(title="Survival Proportions by Gender",x='Survived',y='Proportion') 

plt6 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) + scale_y_continuous(breaks = seq(0, 1.0, 0.25))


#Survival Proportions by Age Cohort [Bar plot]  + [Filled the bar to the fullest] + [Changed legend position and removed title] + [scaled y axis] 
plt7 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = age_cohort),position="fill")+ labs(title="Survival Proportions by Age Cohort",x='Survived',y='Proportion') 

plt7 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) + scale_y_continuous(breaks = seq(0, 1.0, 0.25))


#Survival Proportions by place of Embarkation [Bar plot]  + [Filled the bar to the fullest] + [Changed legend position and removed title] + [scaled y axis] 
plt8 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = embarked),position="fill")+ labs(title="Survival Proportions by place of Embarkation",x='Survived',y='Proportion') 

plt8 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) + scale_y_continuous(breaks = seq(0, 1.0, 0.25))


#Survival Numbers  by Age Cohort and Travel class [Bar plot]  + [Changed legend position and removed title] + [Facet wrap on Pclass ]
plt9 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = age_cohort))+ labs(title="Survival Numbers  by Age Cohort and Travel class",x='Survived',y='Number') 

plt9 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) + facet_wrap(~pclass)


#Survival Numbers  by Gender and Travel class [Bar plot]  + [Changed legend position and removed title] + [Facet wrap on Pclass]
plt10 = ggplot(data=p_df,aes(survived))+geom_bar(aes(fill = sex))+ labs(title="Survival Numbers  by Gender and Travel class",x='Survived',y='Number') 

plt10 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) + facet_wrap(~pclass)


#Age V Fare by place of Embarkation [Scatter plot]  + [ Colour based place of embarkation] + [Changed legend position and removed title]
plt11 = ggplot(data=p_df,aes(x=age,y=fare))+geom_point(aes(colour = embarked))+ labs(title="Age V Fare by Place of Embarkation",x='Age',y='Fare') 

plt11 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) 


#Age V Fare with Linear model [scatter plot] + [Linear model fit]
plt12 = ggplot(data=p_df,aes(x=age,y=fare))+geom_point()+ labs(title="Age V Fare with Linear model",x='Age',y='Fare') 

plt12  + geom_smooth(method = lm)


#Age V Fare with Survival info [scatter plot] + [Colour  based on survival] + [Changed legend position and removed title]
plt13 = ggplot(data=p_df,aes(x=age,y=fare))+geom_point(aes(colour = survived))+ labs(title="Age V Fare with Survival info ",x='Age',y='Fare') 

plt13 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) 


#Age V Fare by Travel class and place of Embarkation [scatter plot] + [ Colour based place of embarkation] + + [Changed legend position and removed title] + [Facet wrap on Pclass]
plt14 = ggplot(data=p_df,aes(x=age,y=fare))+geom_point(aes(colour = embarked))+ labs(title="Age V Fare by Travel class and Point of Departure",x='Age',y='Fare') 

plt14 + theme(legend.position="top",legend.title = element_blank(),legend.spacing.x = unit(0, 'mm')) + facet_wrap(~pclass)




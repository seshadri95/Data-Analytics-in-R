#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 4								
#	TOPIC - "Data Frames"                     STUDEBT ID: 19230107
#----------------------------------------------------------------------------------



set.seed(100)

# Using scope resolutionoperator to assign 'mpg' tibble from ggplot2 package
mpg = ggplot2::mpg

# Storing the tibble as dataframe as operations in tibble are different
df = as.data.frame(ggplot2::mpg) # op is in tibble

#Sampling 10 random observations within the dataset
randm_obvs = sample(1:nrow(df),10)

#Assigning cty to -999 for the above drawn sample
df[randm_obvs,'cty'] = -999
df[randm_obvs,]

#Converting negative values to NA ( i,e converting above initiated negative values to na)
#lapply was to be used as per the question and it returns a list so converted the op back to data frame
df = as.data.frame(lapply(df,function(x) ifelse(x<0,NA,x)))
df[randm_obvs,]

# Using tapply as its used for aggregating numerical data wrt to categorical levels
# Here calculating mean cty for each car class
op = with(df,tapply(cty, class, function(x) mean(x,na.rm = T)))
op[unique(df$class)] #tapply by default orders the levels in ascending order inorder to print output as per the problem statement used unique.

#Using !completecases to get values that are NA and assigning them with corresponding car class mean ( Used tapply oputput vector for assigning)
df[!complete.cases(df),'cty'] = op[as.vector(df[!complete.cases(df),'class'])]
df[randm_obvs,]

#Summary to check our imputation
summary(df)

#Original mean as per the dataset
mean(mpg$cty)

#Mean after our data manipulation
mean(df$cty)
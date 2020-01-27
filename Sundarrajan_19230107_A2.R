#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 2								
#	TOPIC - "Lists & Functions"                     STUDEBT ID: 19230107
#----------------------------------------------------------------------------------



#Generating linear model for 'Faithful' Dataset with eruptions as dependent variable
mod <- lm(eruptions ~ waiting, data=faithful)

#Linear Regression plot
library(ggplot2)
g <- ggplot(faithful, aes(x=waiting, y=eruptions)) + geom_point() + geom_smooth(method="lm", se=FALSE)  # here  se=FALSE to remove confidence band

#Function to return feature data used for trainig (feature name must be supplied with model as arguments)
get_data = function(linear_model,variable_name)
  {
    linear_model$model[,variable_name]
  }

get_data(mod,'waiting')[1:20]

#Function to return model coefficient (coefficient name must be supplied with model as arguments)
get_coefficient = function(linear_model,variable_name)
{
  linear_model$coefficients[variable_name]
}

get_coefficient(mod,'waiting')

#Function to return customised model summary showing - model call,coefficient & datasize (model is the only argument)
mod_summary = function (linear_model) 
  {
  list(
  call = summary(linear_model)$call,
  coeff = linear_model$coefficients,
  Datasize = c('Rows' = nrow(linear_model$model),'Cols' = ncol(linear_model$model))
  )
}

mod_summary(mod)
#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 8								
#	TOPIC - "S3"            						STUDEBT ID: 19230107
#----------------------------------------------------------------------------------

#Constructor for rep_lm class [rep_lm inherets lm] [ added another element 'Information' to list ouptputed from lm]
rep_lm = function(model_name,model,data_f_name)
{
  name = model_name
  model_name = lm(formula = as.formula(model), data = get(data_f_name))
  model_name$Information = list('Name' =  name,
								'DateRun' =  date(),
								'LinearModel' = model,
								'DataSource'=data_f_name,
								'Columns' =colnames(get(data_f_name)),
								'Observations' = nrow(get(data_f_name))
								)
  structure(model_name,class=c('rep_lm','lm'))
}

#object creation
ob = rep_lm("My Model", "eruptions~waiting","faithful")

str(ob)

#Summary function of my_lm class  --- > Prints both subclass and superclass details
summary.rep_lm = function(x)
{
	cat('(1) rep_lm class summary','\n\n',
		paste('Model Name',x$Information$Name,sep = ' : '),'\t', paste('Date of Run',x$Information$DateRun,sep = ' : '),'\n\n',
		paste('Linear Model',x$Information$LinearModel,sep=' : '),'\t',paste('Data Source',x$Information$DataSource,sep=' : '),'\n\n',
		paste('Columns : ',x$Information$Columns,'\n'),'\n',
		paste('Observations',x$Information$Observations,sep = ' : '),'\n\n',sep =''
		)
  
    cat('(2) lm class summary','\n')
    print(summary.lm(x))
}

summary(ob)
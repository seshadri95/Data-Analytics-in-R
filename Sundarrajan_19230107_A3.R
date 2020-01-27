#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 3								
#	TOPIC - "Matrices & Functionals"                     STUDEBT ID: 19230107
#----------------------------------------------------------------------------------



#Generating normal distributed sample with 20 obersvations and respective mean & SD 

set.seed(100)
CX101 <- rnorm(20,45,8)
CX102 <- rnorm(20,65,8)
CX103 <- rnorm(20,85,10)
CX104 <- rnorm(20,45,10)
CX105 <- rnorm(20,60,5)


# Creating matrix from the above generated data as follows,

student_matrix = matrix(c(CX101,CX102,CX103,CX104,CX105),nrow = 20, ncol = 5, byrow = F)

# Rownames and columnnames are created as per the mentioned alphanumeric sequence using 'sapply' as we are dealing with vectors

rownames(student_matrix) = sapply(1:20,function(x) paste0('student_',x))
colnames(student_matrix) = sapply(1:5,function(x) paste0('CX10',x))

summary(student_matrix)
student_matrix[c(1,2,20),]
student_matrix[student_matrix[,"CX103"] > 100,]

# From the above test we infered there are few samples withh value > 100 , therfore using 'apply' functional to convert values <0 or  values >100 into NA (Not available)
# apply function does operation on either column r row wise
# ifelse function needs (logival_vector,op if true,op if false)
student_matrix = apply(student_matrix,2,function(x) ifelse(x > 100 | x<0,NA,x))
student_matrix[14:20,]

# Imputing the above created null values using mean of the respective columns using 'apply' functional as we are dealing with matrix.

student_matrix = apply(student_matrix,2,function(x)ifelse(is.na(x),mean(x,na.rm = T),x))
student_matrix[14:20,]

# Aggregation performed on the each student records for obtaining their average and range using 'apply' functional

Mean = apply(student_matrix,1,mean)
Range = apply(student_matrix,1,function(x) max(x) - min(x))

student_matrix =cbind(student_matrix,Mean)
student_matrix =cbind(student_matrix,Range)
student_matrix[1:7,]

# Filter query to find the student with highest average mark
student_matrix[student_matrix[,'Mean'] == max(student_matrix[,'Mean']),,drop = FALSE]   # here drop =False preserves rowname while displaying output for single row | Incase of multiple rows rownames are displayed fr better interpretability

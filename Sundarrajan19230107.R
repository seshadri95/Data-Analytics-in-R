#----------------------------------------------------------------------------------
#	SUBJECT CODE : CT5102 (Programming for DA)      NAME: Seshadri Sundarrajan		
#	ASSIGNMENT NO : 1								
#	TOPIC - "Vectors"                               STUDEBT ID: 19230107
#----------------------------------------------------------------------------------


set.seed(1000)

# Dice 1 Roll - 1000 times
dice1 = sample(c(1,2,3,4,5,6),1000,replace = TRUE)
table(dice1)

# Dice 1 Roll - 1000 times
dice2 = sample(c(1,2,3,4,5,6),1000,replace = TRUE)
table(dice2)

# Sum of each oucomes from each dice 
total = dice1 + dice2
table(total)

# Function to implement table function
table_fn = function(input_vector)
{
	op.vector = c()
	unique_input_vector = sort(unique(input_vector))
	for ( i in unique_input_vector)
	{
		count = sum(input_vector == i)
		op.vector = c(op.vector, count)
	}
	names(op.vector) = unique_input_vector
	return(op.vector)
}

ans = table_fn(total)
ans
names(ans)

# Subsetting even numbered outcome using logical vector recyclic approach
even = ans[c(T,F)]
even

# Subsetting odd  numbered outcome using names of the vector
odd = ans[(as.numeric(names(ans))) %% 2 == 1]
odd

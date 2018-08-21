bam.train <- function(X,Y){
	A = X %*% t(Y)
	return (A)
}

bam.test <- function(A,x){
	
	oldH = 100	
	variation = 1

	while(variation > 0){
		s = as.vector(x %*% A)
		s[s < 0] = -1
		s[s > 0] = 1
		s[s == 0] = sample(c(-1,1),size=1)
			
		#Entropy
		H = -x %*%  A %*% s
		
		x = as.vector(A %*% s)
		x[x < 0] = -1
		x[x > 0] = 1
		x[x == 0] = sample(c(-1,1),size=1)

		#Verify the variation
		variation = abs(H - oldH)
		oldH = H
	}

	cat("Output\n")
	print(s)

	cat("Input\n")
	print(x)
}

f <- function(net){
	if(net >= 0.5) return (1)
	return (0)
}

perceptron.test <- function(x,weights){

	net = c(as.numeric(x),1) %*% weights 
	return (f(net))
}

perceptron.train <- function(dataset,eta = 0.1, threadshold = 1e-3){
	
	#classe no dataset
	classId = ncol(dataset)

	#atributos que definirao a classe
	X = dataset[,(1:classId-1)]

	#classe
	Y = dataset[,classId]

	#weights = [w_1, w_2, theta]
	#runif gera os pesos com valores aleatorios
	weights = runif(min = -0.5, max = 0.5, n=ncol(X)+1)
	
	sqerror = 2*threadshold
	while(sqerror > threadshold){
		sqerror = 0
		for (i in 1:nrow(X)){
			x = as.numeric(X[i,])
			y = Y[i]

			#Aplicar esse exemplo no perceptron
			net = c(x,1) %*% weights 
			y.o = f(net)
				
			#erro entre a classe esperada menos a obtida
			error = y - y.o

			#erro ao quadratico para controlar a qualidade de solucao
			sqerror = sqerror + error^2

			#derivada
			dE2 = 2*error*-c(x,1)

			weights = weights - eta * dE2
		}
		sqerror = sqerror / nrow(X)
		cat("sqerror = ",sqerror,"\n")
	}

	return (weights)
}

perceptron.plot <- function(weights){

	r = seq(0,1,length=100)
	m = outer(r,r,function(x1,x2){
		cbind(x1,x2,1) %*% weights
	})

	ids = which(m>=0.5)
	m[ids] = 1
	m[-ids] = 0
	filled.contour(r,r,m)
}

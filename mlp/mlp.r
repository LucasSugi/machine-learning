#funcao de ativacao -> funcao sigmoidal
f <- function(net){
	return (1/(1+exp(-net)))
}

#derivada da funcao de ativacao
df_dnet <- function(f_net){
	return (f_net * (1-f_net))
}

#arquitetura da mlp
mlp.architecture <- function(input.length=2,hidden.length=2,output.length=1, activation.function = f, d_activation.function = df_dnet){

	#modelo a ser retornado
	model = list()
	model$input.length = input.length
	model$hidden.length = hidden.length
	model$output.length = output.length
	
	#geracao de pesos para a camada escondida
	model$hidden = matrix(runif(min =-0.5, max=0.5, hidden.length*(input.length+1)),nrow=hidden.length,ncol=input.length+1)

	#geracao de pesos para a camada de saida
	model$output = matrix(runif(min =-0.5, max=0.5, output.length*(hidden.length+1)),nrow=output.length,ncol=hidden.length+1)

	model$f = activation.function
	model$df_dnet = d_activation.function

	return (model)
}

#aplica os dados nas equacoes
mlp.forward <- function(model, Xp) {
	
	#multiplica os pesos da camada escondida pela entrada de padrao p
	net_h_p = model$hidden %*% c(Xp,1) 

	#aplica o resultado da camada escondida na funcao de ativacao
	f_net_h_p = model$f(net_h_p)

	#multiplica os pesos da camada de saida pelas saidas da camada escondida
	net_o_p = model$output %*% c(as.numeric(f_net_h_p),1)

	#aplica o resulto da camada de saida pela funcao de ativacao
	f_net_o_p = model$f(net_o_p)

	#resultado
	ret = list()
	ret$net_h_p = net_h_p
	ret$net_o_p = net_o_p
	ret$f_net_h_p = f_net_h_p
	ret$f_net_o_p = f_net_o_p

	return (ret)
}


#treinamento
mlp.backpropagation <- function(model, dataset, eta=0.1, threadshold=1e-3){

	squaredError = 2 * threadshold
	counter = 0
	
	#roda enquanto o erro eh maior que threadshold
	while(squaredError > threadshold){
		squaredError = 0	
		
		#para cada linha do dataset aplica a forward	
		for (p in 1:nrow(dataset)) {
			Xp = as.numeric(dataset[p,1:model$input.length])	
			Yp = as.numeric(dataset[p,(model$input.length+1):ncol(dataset)])
	
			#resultado do padrao p aplicado
			results = mlp.forward(model,Xp)
			
			#valor obtido
			Yo = results$f_net_o_p

			#calculando erro
			error = Yp - Yo
				
			#erro utilizado para treinamento
			squaredError = squaredError + sum(error^2)
			
			#ajuste da output
			delta_o_p = error * model$df_dnet(results$f_net_o_p) 

			#ajuste da hidden
			w_o_kj = model$output[,1:model$hidden.length]
			delta_h_p = as.numeric(model$df_dnet(results$f_net_h_p)) * (as.numeric(delta_o_p) %*% w_o_kj)

			#trainamento
			model$output = model$output + eta * (delta_o_p%*%as.vector(c(results$f_net_h_p,1)))
			model$hidden = model$hidden + eta * (t(delta_h_p)%*%as.vector(c(Xp,1)))
		}	
		squaredError = squaredError / nrow(dataset)
		cat("Erro medio quadrado = ", squaredError, "\n")
		counter = counter + 1
	}

	ret = list()
	ret$model = model
	ret$counter = counter

	return (ret)
}

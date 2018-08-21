fitness <- function(subject, distances){
	
	sum = 0
	for(i in 1:(length(subject)-1)){
		sum = sum + distances[subject[i],subject[i+1]]
	}
	sum = sum + distances[subject[length(subject)],subject[1]]

	return (1/sum)
}


batch <- function(distances,k=5, popsize=100, ngenerations=100){
	
	#Matriz de saida
	output = NULL

	#Numero de cidades	
	ncities = nrow(distances)
	
	#Fitness para cada individuo	
	fitness = rep(0,popsize)

	#Gerando a populacao
	population = NULL
	for(i in 1:popsize){
		subject = sample(1:ncities)
		population = rbind(population,subject)
		fitness[i] = fitness(subject,distances)
	}
	
	#Numero de gerecoes
	for(i in 1:ngenerations){
		children = NULL
		children.fitness = rep(0,k)
		#Produz k filhos
		for(j in 1:k){
			#Pai que ira produzir um filho
			parentId = sample(1:popsize,size=1)
			new = population[parentId,]

			selected = sample(1:ncities,size=2)
			aux = new[selected[1]]
			new[selected[1]] = new[selected[2]]
			new[selected[2]] = aux
			children = rbind(children,new)
			children.fitness[j] = fitness(new,distances)
		}

		#Competicao
		for(j in 1:k){
			id = sample(1:popsize,size=1)	
			if(fitness[id] < children.fitness[j]){
				population[id,] = children[j,]	
				fitness[id] = children.fitness[j]
			}
		}

		output = rbind(output, cbind(i,mean(fitness),sd(fitness)))
	}

	return (output)
}

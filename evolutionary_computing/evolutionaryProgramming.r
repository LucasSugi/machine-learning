fitness <- function(subject, distances){
	
	sum = 0
	for(i in 1:(length(subject)-1)){
		sum = sum + distances[subject[i],subject[i+1]]
	}
	sum = sum + distances[subject[length(subject)],subject[1]]

	return (1/sum)
}


evolutionaryProgramming <- function(distances,popsize=100, ngenerations=100){
	
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
		children.fitness = rep(0,popsize)
		#Produz popsize filhos
		for(j in 1:popsize){
			#Pai que ira produzir um filho
			new = population[j,]

			selected = sample(1:ncities,size=2)
			aux = new[selected[1]]
			new[selected[1]] = new[selected[2]]
			new[selected[2]] = aux
			children = rbind(children,new)
			children.fitness[j] = fitness(new,distances)
		}
		
		#Unificando populacao e fitness	
		single.population = rbind(population,children)
		single.fitness = rbind(fitness,children.fitness)
		
		#Ordenando segundo o fitness
		ids = sort.list(single.fitness,dec=T)[1:popsize]
	
		#Guardando os melhores
		population = single.population[ids,]
		fitness = single.fitness[ids]

		output = rbind(output, cbind(i,mean(fitness),sd(fitness)))
	}

	return (output)
}

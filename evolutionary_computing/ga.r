fitness <- function(subject, distances){
	
	sum = 0
	for(i in 1:(length(subject)-1)){
		sum = sum + distances[subject[i],subject[i+1]]
	}
	sum = sum + distances[subject[length(subject)],subject[1]]

	return (1/sum)
}

organize.children <- function(child){

	ids = setdiff(1:length(child),child)
	if(length(ids) == 0) return (child)
	
	change.positions = c()
	for(i in 1:length(child)){
		pos = which(child == i)
		if(length(pos) > 1){
			change.positions = c(change.positions,sample(pos,size=length(pos)-1))
		}
	}
	child[change.positions] = ids

	return (child)
}

mutation <- function(child,mutation.prob){
	if(runif(min=0,max=1,n=1) < mutation.prob){
		ids = sample(1:length(child),size=2)
		aux = child[ids[1]]
		child[ids[1]] = child[ids[2]]
		child[ids[2]] = aux
	}
	return (child)
}



ga <- function(distances,popsize=100, ngenerations=100, mutation.prob=0.1){
	
	#Matriz de saida
	output = NULL

	#Numero de cidades	
	ncities = nrow(distances)
	
	#Fitness para cada individuo	
	fitness = rep(0,popsize)

	#Gerando a populacao
	population = NULL
	for(i in 1:popsize){
		chromossome = sample(1:ncities)
		population = rbind(population,chromossome)
	}
	
	
	#Numero de gerecoes
	for(i in 1:ngenerations){
		#Calcula o fitness
		for(j in 1:popsize){
			fitness[j] = fitness(population[j,],distances)
		}		
		
		output = rbind(output, cbind(i,mean(fitness),sd(fitness)))
		
		#Gerando filhos por crossover
		children = NULL	
		for(j in 1:(popsize/2)){
			parentIds = sample(1:popsize,prob=fitness/sum(fitness),size=2)	
			parent1 = population[parentIds[1],]
			parent2 = population[parentIds[2],]

			#Ponto de corte
			point = sample(1:(ncities-1),size=1)	
			
			#Crossover
			child1 = c(parent1[1:point],parent2[(point+1):ncities])
			child2 = c(parent2[1:point],parent1[(point+1):ncities])
			child1 = organize.children(child1)
			child2 = organize.children(child2)

			child1 = mutation(child1,mutation.prob)
			child2 = mutation(child2,mutation.prob)

			children = rbind(children,child1)
			children = rbind(children,child2)
		}
		#Preserva o melhor - Elitismo
		bestFitnessId = which.max(fitness)
		childId = sample(1:popsize,size=1)
		children[childId,] = population[bestFitnessId,]

		#Nova populacao
		population = children	
	}

	return (output)
}

fitness <- function(element, capacity,cost=1000){

	v = rep(0,length(capacity))

	for(i in 1:length(element)){
		coreId = element[i]
		v[coreId] = v[coreId] + cost/capacity[coreId]
	}	

	return (1/max(v))
}

modelo1 <- function(capacity,nprocess=10, popsize=100,ngenerations=1000){
	
	ncores = length(capacity)	

	#Gerar M individuos para compor a populacao
	population = NULL
	for( i in 1:popsize){
		element = rep(0,nprocess)
		for(j in 1:nprocess){
			element[j] = sample(1:ncores,size=1)
		}
		population = rbind(population,element)
	}

	for(generationId in 1:ngenerations){
		#Pai aleatorio
		parentId = sample(1:popsize,size=1)	

		#Filho sendo uma copia do pai
		new = population[parentId,]

		#Alterando o filho
		id = sample(1:nprocess,size=1)
		new[id] = sample(1:ncores,size=1)

		#Seleciona um elemento para competir
		selectedId = sample(1:popsize,size=1)
		
		#Verifica quem e o melhor
		if(fitness(new,capacity) > fitness(population[selectedId,],capacity)){
			population[selectedId,] = new
		}

		#Fitness medio
		#Desvio padrao do fitness
		fit = rep(0,nrow(population))
		for(i in 1:nrow(population)){
			fit[i] = fitness(population[i,],capacity)
		}
		cat(mean(fit)," ", sd(fit),"\n")
	}
}

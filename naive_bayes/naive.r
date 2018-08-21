naive.bayes <- function(dataset, query,minimal.prob = 1e-7){
	
	#numero de colunas
	classId = ncol(dataset)
	
	#pega de maneira unica as classes do conjunto de dados
	classes = unique(dataset[,classId])
	

	probabilities = rep(0,length(classes))

	#Verificara o argmax para cada classe 
	for (i in 1:length(classes)){
		#Conta quantas vezes a classe aparece (probabilidade da classe)
		P_class = sum(dataset[,classId]	== classes[i]) / nrow(dataset)
		
		#Vetor utilizado para computar o produtorio das probabilidades
		probabilities[i] = probabilities[i] + log(P_class)
		
		#Para cada atributo do ponto de consulta verifica sua ocorrencia	
		for(j in 1:length(query)){
			Attr = query[j]
			if(Attr != "?"){
				P_class_attr = sum(dataset[,j] == Attr & dataset[,classId] == classes[i]) / sum(dataset[,classId] == classes[i])	
				if(P_class_attr == 0){
					P_class_attr = minimal.prob	
				}
				probabilities[i] = probabilities[i] + log(P_class_attr)
			}
		}

	}
	
	#retornando do log
	probabilities = exp(probabilities)

	ret = list()
	ret$classes = classes
	ret$probabilities = probabilities / sum(probabilities)

	return (ret)	
}

#dataset = conjunto de dados
#clusters = quantidade de agrupamentos a serem criados nos dados
#threshold = limiar de parada do algoritmo
kmeans <- function(dataset,clusters=2, threshold=1e-3){
	
	#transformacao do conjunto de dados como uma matriz	
	dataset = as.matrix(dataset)

	#amostragem aleatoria dos ids das linhas de dataset na quantidade de clusters	
	ids = sample(1:nrow(dataset), size=clusters)
	
	#armazenando as linhas que serao usadas como centroides iniciais
	centers = dataset[ids,] 
	
	#vetor que contera os centroides mais proximos de cada ponto
	closest = rep(0,nrow(dataset))
	
	div = 2*threshold
	while(div > threshold){
		div = 0
		for(i in 1:nrow(dataset)){
			#armazena uma linha por vez de dataset
			row = dataset[i,]	
	
			#calcula da distancia entre a linha com cada centroide
			euclidean = apply(centers,1,function(cl){
				sqrt(sum((row - cl)^2))
			})
	
			#verifica o ID do menor centroide
			id = sort.list(euclidean,dec=F)[1]
			closest[i] = id
		}
		
		#centros antigos	
		old = centers

		#recalcula os novos centroides baseado na media dos pontos mais proximos a ele	
		for(i in 1:clusters){
			id = which(closest == i)	
			centers[i,] = colMeans(dataset[id,])
		}
		
		#calcula do "erro"
		div = sqrt(sum((old - centers)^2))
	}
	
	ret = list()
	ret$centers = centers
	ret$closest = closest

	return (ret)
}

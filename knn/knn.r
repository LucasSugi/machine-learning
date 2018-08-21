#dataset = conjunto de dados a ser analizado
#query = ponto que tera sua distancia verificada
# k = numero de vizinhos a ser analizado
knn <- function(dataset,query,k=1){
	
	#retorna o numero de colunas
	#na ultima coluna nos temos a classe da tupla
	classId = ncol(dataset)

	#E contem todas as distancias entre query com relacao a database
	E = apply(dataset,1,  function(row){
		#calculo da distancia
		sqrt(sum((query - as.numeric(row[1:(classId-1)]))^2))
	})

	#ordene as distancias e retorne as k primeiras
	ids = sort.list(E,dec=F)[1:k]

	#retorna as classes com as menores distancias
	classes = dataset[ids,classId]
	
	#unifica as ocorrencias de classes
	U = unique(classes)

	#vetor resposta
	R = rep(0,length(U))

	#verifica para cada classe em U quantas vezes ela ocorre em classes e armazena resultado em R
	for(i in 1:length(U)){
		R[i] = sum(U[i] == classes)
	}
	
	#qual a maior classe de R
	id = which.max(R)

	#criacao de lista
	ret = list()
	ret$U = U
	ret$R = R
	ret$class = as.character(U[id])

	return(ret)
}

weight <- function(xi,yi,sigma){
	euclidean = sqrt(sum((xi-yi)^2))
	return (exp(-euclidean^2/(2*sigma^2)))
}

#dataset: base de dados a ser utilizada
#query: ponto de consulta
#sigma: coeficiente a ser utilizado na funcao radial
dwnn <- function(dataset,query, sigma=0.5){

	#na ultima coluna nos temos a classe
	classId = ncol(dataset)	

	w = apply(dataset,1,function(row){
		weight(query,row[1:(classId-1)],sigma)
	})
		
	Y = dataset[,classId]
	y = sum(w*Y) / sum(w)

	return (y)
}

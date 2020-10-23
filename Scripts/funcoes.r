
# install.packages("kohonen")
# install.packages("ggplot2")

library(kohonen)
library(ggplot2)

distancia <- function(x1,y1,x2,y2){
    sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

vizinho <- function (ponto, base) {
	x <- base[which(base[,1] == ponto),2]
	y <- base[which(base[,1] == ponto),3]
	clus <- base[which(base[,2] == x & base[,3] == y),5]
	d <- vector()
	
	for (i in 1:length(base[,1])) {
		if (base[i,5] != clus){
			d[i] <- distancia(x,y,base[i,2],base[i,3])
		}
	}
	
	r = min(d[which(d>0)])
	nn = min(which(d==r))

	return(nn-1)
}

funcao_objetivo_SA <- function(base){
	pesos <- c()
	for (i in 1:max(base[,5])){
		pesos[i] = sum(base[which(base[,5] == i ),4])
	}
	
	dp = sd(pesos)
	media = mean(pesos)
	
	return(dp/media)
}


funcao_objetivo <- function(base){
	pesos <- c()
	for (i in 1:max(base[,5])){
		pesos[i] = sum(base[which(base[,5] == i ),4])
	}
	
	dp = sd(pesos)
	media = mean(pesos)

	plot(base[,2:3],col = base[,5])
	
    return(list(pesos = pesos, fo = dp/media, dp = dp, media = media))
}


rearranjo <- function(base, k, MaxPeso, MinPontos, metodo){
    if (missing(metodo)){metodo <- "mais proximo"}
    resultado <- data.frame(P = integer(),X = integer(),Y = integer(),W = integer(),C = integer(), stringsAsFactors=FALSE)
    cont_media = 0
    media <- c()
    base_troca <- base
    base_aux <- base
    FO_troca <- funcao_objetivo_SA(base_troca)
    FO_aux <- funcao_objetivo_SA(base_aux)
  
    for (i in 1:k){
        a <- base[which(base[,5] == i), 2:3]
        b <- chull(a)
        b <- a[b, ]

        for (j in 1:nrow(b)){
            resultado[nrow(resultado)+1,] <- base[which(base[,2] == b[j,1] & base[,3] == b[j,2]),]
        }
    }   

    resultado <- unique(resultado)

    # plot <- ggplot(NULL, aes(X, Y)) + 
    # geom_point(data = base[,2:3], color = base$C, pch = 1) +  
    # geom_point(data = resultado[,2:3], color = resultado$C, pch = 19) +
    # theme_bw()
    # print(plot)

    if (metodo == "mais proximo"){
        p <- sample(1:nrow(resultado), 1) #sorteia uma linha
        p2 <- resultado[p,1] #acha o ponto da linha sorteada
        c <- base_troca[which(base_troca[,1] == p2), 5] #cluster de troca

        dist <- data.frame(D = numeric(), P = integer(), C = integer())

        for (i in 1:nrow(resultado)){
            if (resultado[i,5] != resultado[p,5]){
                D <- distancia(resultado[p,2], resultado[p,3], resultado[i,2], resultado[i,3])
                P <- resultado[i,1]
                C <- resultado[i,5]
                dist[nrow(dist)+1,] <- c(D, P, C)
            }
        }
    
        menor_dist <- min(dist[,1])
        v <- dist[which(dist[,1] == menor_dist), 2]
        v <- v[1]

        pontos <- nrow(base_troca[which(base_troca[,5] == dist[which(dist[,2] == v), 3]),]) -1 #calcula pontos depois da troca

        base_aux[which(base_aux[,1] == v),5] <- c #altera cluster do vizinho (REARRANJO)
        FO_aux <- funcao_objetivo_SA(base_aux) #calcula a funcao objetivo da troca
        
        peso <- sum(base_aux[which(base_aux[,5] == c),4]) #calcula peso depois da troca de pontos

        if (peso <= MaxPeso & pontos >= MinPontos){
            if(FO_aux < FO_troca){
                base_troca <- base_aux
                FO_troca <- FO_aux
            }
        }
        
        return(base_troca)
    }

    else if (metodo == "circunferencia"){ 
        raio <- 10 #média da soma das distancias dos pontos externos
        p <- sample(1:nrow(resultado), 1) #sorteia um ponto
        p2 <- resultado[p,1]

        vizinhos <- data.frame(P = integer(), X = integer(), Y = integer(), W = integer(), C = integer())

        for (i in 1:nrow(resultado)){
            dist <- distancia(resultado[p,2], resultado[p,3], resultado[i,2], resultado[i,3])
            if (dist <= raio){
                vizinhos[nrow(vizinhos)+1,] <- resultado[i,]
            }
        }

        vizinhos <- vizinhos[which(vizinhos[,5] != resultado[p,5]),]

        c <- base_troca[which(base_troca[,1] == p2), 5] #Cluster de troca

        for (i in 1:nrow(vizinhos)){
            pontos <- nrow(base_troca[which(base_troca[,5] == vizinhos[i,5]),]) -1 #calcula pontos depois da troca

            base_aux[which(base_aux[,1] == vizinhos[i,1]),5] <- c #altera cluster do vizinho 
            FO_aux <- funcao_objetivo_SA(base_aux) #calcula a funcao objetivo da troca

            peso <- sum(base_aux[which(base_aux[,5] == c),4])

            if (peso <= MaxPeso & pontos >= MinPontos){
                if(FO_aux < FO_troca){
                    base_troca <- base_aux
                    FO_troca <- FO_aux
                }
            }

            base_aux <- base_troca
            FO_aux <- FO_troca
        }
        return(base_troca)
    }

    else if (metodo == "dois clusters"){
        p <- sample(1:nrow(resultado), 1) #sorteia uma linha
        p2 <<- resultado[p,1] #acha o ponto da linha sorteada
        c <- base_troca[which(base_troca[,1] == p2), 5] #cluster de troca

        dist <- data.frame(D = numeric(), P = integer(), C = integer())

        for (i in 1:nrow(resultado)){
            if (resultado[i,5] != resultado[p,5]){
                D <- distancia(resultado[p,2], resultado[p,3], resultado[i,2], resultado[i,3])
                P <- resultado[i,1]
                C <- resultado[i,5]
                dist[nrow(dist)+1,] <- c(D, P, C)
            }
        }

        menor_dist <- dist[order(dist[,1]),]
        vizinhos <- menor_dist[1,]
        i = 2
        while (i <= nrow(menor_dist)){
            if (menor_dist[i,3] != vizinhos[1,3]){
                vizinhos[nrow(vizinhos)+1,] <- menor_dist[i,]
                i <- nrow(menor_dist) + 1
            }

            i <- i + 1
        }

        for (i in 1:nrow(vizinhos)){
            pontos <- nrow(base_troca[which(base_troca[,5] == vizinhos[i,3]),]) -1 #calcula pontos depois da troca

            base_aux[which(base_aux[,1] == vizinhos[i,2]),5] <- c #altera cluster do vizinho 
            FO_aux <- funcao_objetivo_SA(base_aux) #calcula a funcao objetivo da troca

            peso <- sum(base_aux[which(base_aux[,5] == c),4])

            if (peso <= MaxPeso & pontos >= MinPontos){
                if(FO_aux < FO_troca){
                    base_troca <- base_aux
                    FO_troca <- FO_aux
                    v <<- vizinhos[i,2]
                }
            }

            base_aux <- base_troca
            FO_aux <- FO_troca
        }
        
        return(base_troca)
    }

}



sim_anneal <- function (base, T, MaxIter_T, MaxAceit_T, MaxPeso, MinPontos, metodo){
    
    base_troca <- base
    base_otima <- base
    base_atual <- base
    FO_otima <- funcao_objetivo_SA(base)
    FO_atual <- funcao_objetivo_SA(base)
    nro_aceites <- 0
    analise_otima <- data.frame(T = integer(), FO_otima = integer())	
    analise <- data.frame(T = integer(), FO = integer())
    
    while (T > 0.1){
        iter_t <- 0
        nro_aceites <- 0
        
        while (iter_t <= MaxIter_T & nro_aceites <= MaxAceit_T){
            
            base_troca <- rearranjo(base_troca, max(base_troca[,5]), MaxPeso, MinPontos, metodo)
            FO_troca <- funcao_objetivo_SA(base_troca) #calcula Função Objetivo
            
            analise[nrow(analise)+1,] <- c(T, FO_troca) #registra índice
                
            if( FO_troca <= FO_atual ){ #verifica se a Função Objetivo é menor
                base_atual <- base_troca #troco bases
                FO_atual <- FO_troca #atualizo o FO_otima
                
                if (FO_troca < FO_otima) {
                    base_otima <- base_troca #troco bases
                    FO_otima <- FO_troca
                }
                
                nro_aceites <- nro_aceites + 1
                analise_otima[nrow(analise)+1,] <- c(T, FO_troca) #registra índice
            }
            else if ( sample(0:10000000000,1)/10000000000 <= exp(-(((FO_troca*10000) - (FO_atual*10000))/T))){ #aceita se dentro da prob. boltzman
                base_atual <- base_troca #troco somente a base atual, pois já que o FO_otima não é menor, não é ideal
                
                FO_atual <- FO_troca
                analise[nrow(analise)+1,] <- c(T, FO_atual) #registra índice
                
                nro_aceites <- nro_aceites + 1
            }
            else {
                base_troca <- base_atual #caso não se encaixe em nenhum critério de troca, reinicio a base a ser analisada
            } 
            
            iter_t <- iter_t + 1
        }
        
        T = T * 0.99
        
    }
    
    
    plot(base_otima[,2:3],col = base_otima[,5])
    plot(unique(analise_otima$FO_otima), type="o")
    plot(analise$FO, type="o")
    
    resultado_final <- list(base_otima = base_otima, analise = analise)

    return(base_otima)
}


tabuSearch <- function (base, maxIterTabu, tamLista, MaxPeso, MinPontos, percAceite, metodo){

    iterTabu = 0
    listaTabu <- data.frame(Ponto = character(), FO = integer(), stringsAsFactors=FALSE)
    trocaPontos <- data.frame(Ponto = character(), FO = integer(), stringsAsFactors=FALSE)
    analise <- data.frame(Iter = character(), FO = integer(), stringsAsFactors=FALSE)
    base_troca <- base
    base_atual <- base
    base_otima <- base
    FO_atual <- funcao_objetivo_SA(base)
    FO_troca <- funcao_objetivo_SA(base)
    FO_otima <- funcao_objetivo_SA(base)

    while (iterTabu <= maxIterTabu){
        
        base_troca <- rearranjo(base_troca, max(base_troca[,5]), MaxPeso, MinPontos, metodo)
        FO_troca <- funcao_objetivo_SA(base_troca) #calcula Função Objetivo
        
        if (FO_troca < FO_atual){
            trocaPontos <- trocaPontos[-1,]
            trocaPontos[nrow(trocaPontos) + 1,] <- c(paste(p2,v,sep="|"), FO_troca)
            analise[nrow(analise) + 1,] <- c(iterTabu, FO_troca)

            if ( trocaPontos$Ponto %in% listaTabu$Ponto ) {
                if( FO_troca < FO_atual * percAceite){
                    # print("aceitei mesmo tando no tabu")
                    base_atual <- base_troca
                    FO_atual <- FO_troca
                }
                else {
                    # print("Esta no tabu mas nao aceitei")
                }
            }
            else {
                # print("Não esta no tabu entao aceitei")
                base_atual <- base_troca
                FO_atual <- FO_troca
            }

            if(FO_atual < FO_otima){
                base_otima <- base_atual
                FO_otima <- FO_atual
            }

            #registra indice
            if (nrow(listaTabu) >= tamLista){
                listaTabu <- listaTabu[-1,]
                listaTabu[nrow(listaTabu) + 1,] <- trocaPontos #registra índice
            }
            else {
                listaTabu[nrow(listaTabu) + 1,] <- trocaPontos #registra índice
            }  
        }
        else {
            base_troca <- base_atual
            FO_troca <- base_atual
        }
    

        iterTabu <- iterTabu + 1
    }

    plot(unique(listaTabu$FO), type="o")
    plot(analise, type="o")
    return(base_otima)
}



rec_kmeans <- function(base, MaxPeso, MinPontos, k, funcao, metodo){
    inicio <- Sys.time()
    print(inicio)

    max = 0
    vetor_max <- c()

    cat("Clusterizando com: ", k, "clusters\n")
    aux <- kmeans(base[,2:3], k)
    base$C <- aux$cluster

    if(funcao == 'TS'){
	    base <- tabuSearch(base, 2500, 100, MaxPeso, MinPontos, 0.95, metodo)
    }
    else if (funcao == 'SA'){
        base <- sim_anneal(base, 90, 80, 8, MaxPeso, MinPontos, metodo) 
    }
    else{
        base <- sim_anneal(base, 90, 80, 8, MaxPeso, MinPontos, metodo) 
    }

	for (i in 1:max(base$C)){
		vetor_max = append(vetor_max,c(sum(base[which(base[,5] == i),4])))
		max = max(vetor_max)
	}    

    if(max > MaxPeso){
        rec_kmeans(base, MaxPeso, MinPontos, k+1, funcao, metodo)  
    }
    else {
        termino <- Sys.time()
        print(termino)
        return(base)
    }
}


rec_SOM <- function (base, MaxPeso, MinPontos, k, funcao, metodo) {
	inicio <- Sys.time()
    print(inicio)
	cat("Clusterizando com", k, "clusters\n")

	#cria vetores auxiliares
	vetor_max <- c()
	max = 0
	
	#clusteriza SOM
	base_aux <- data.matrix(base[,2:3])
	g <- somgrid(xdim = k, ydim = 1, topo = "rectangular" )
	map <- som(base_aux,grid = g, alpha = c(0.05, 0.01), radius = 1)
	
	#guarda clusterização na coluna C
	base$C <- map$unit.classif

    if(funcao == 'TS'){
	    base <- tabuSearch(base, 2500, 100, MaxPeso, MinPontos, 0.95, metodo)
    }
    else if (funcao == 'SA'){
        base <- sim_anneal(base, 90, 80, 8, MaxPeso, MinPontos, metodo) 
    }
    else{
        base <- sim_anneal(base, 90, 80, 8, MaxPeso, MinPontos, metodo) 
    }

	#reclasteriza se necessário
	for (i in 1:max(base$C)){
		vetor_max = append(vetor_max,c(sum(base[which(base[,5] == i),4])))
		max = max(vetor_max)
	}
	
	cat("maximo depois da funcao: ", max, "\n")
	
	if (max > MaxPeso) {
		rec_SOM(base, MaxPeso, MinPontos, k+1, funcao, metodo)
	}
	else {
		termino <- Sys.time()
        print(termino)
		return (base)
	}
}
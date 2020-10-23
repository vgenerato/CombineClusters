
source("C:\\Users\\windows\\Documents\\Iniciação Científica\\scripts\\funcoes.r")

############################### 20 pontos ###############################
base20 = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\20pontos.csv",header=TRUE,sep=";")

#Kmeans
base20_KSA <- rec_kmeans(base20, 8000, 2, 2, "SA", "dois clusters") # 5 min
base20_KTS <- rec_kmeans(base20, 8000, 2, 2, "TS", "dois clusters") # 3 min

#SOM
base20_SSA <- rec_SOM(base20, 8000, 2, 2, "SA", "dois clusters") # 4 min
base20_STS <- rec_SOM(base20, 8000, 2, 2, "TS", "dois clusters") # 2 min

#Funções Objetivos
funcao_objetivo(base20_KSA)
funcao_objetivo(base20_KTS)
funcao_objetivo(base20_SSA)
funcao_objetivo(base20_STS)



############################### 100 pontos ###############################
base100 = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\100pontos.csv",header=TRUE,sep=";")

#Kmeans
base100_KSA <- rec_kmeans(base100, 200, 5, 7, "SA", "dois clusters") # 6 minutos
base100_KTS <- rec_kmeans(base100, 200, 5, 7, "TS", "dois clusters") # 3 minutos

#SOM
base100_SSA <- rec_SOM(base100, 200, 5, 7, "SA", "dois clusters") # 7 minutos
base100_STS <- rec_SOM(base100, 200, 5, 7, "TS", "dois clusters") # 3

#Funções Objetivos
funcao_objetivo(base100_KSA)
funcao_objetivo(base100_KTS)
funcao_objetivo(base100_SSA)
funcao_objetivo(base100_STS)


############################### 500 pontos ###############################
base500 = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\500pontos.csv",header=TRUE,sep=",")

#Kmeans
base500_KSA <- rec_kmeans(base500, 6500, 50, 7, "SA", "mais proximo") # 8 minutos 
base500_KTS <- rec_kmeans(base500, 6500, 50, 7, "TS", "mais proximo") # 3 mnutos

#SOM
base500_SSA <- rec_SOM(base500, 6500, 50, 7, "SA", "mais proximo") # 8 minutos
base500_STS <- rec_SOM(base500, 6500, 50, 7, "TS", "mais proximo") # 3 minutos

#Funções Objetivos
funcao_objetivo(base500_KSA)
funcao_objetivo(base500_KTS)
funcao_objetivo(base500_SSA)
funcao_objetivo(base500_STS)


############################### 1000 pontos ###############################
baseSP = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\baseSP.csv",header=TRUE,sep=",")
#Kmeans
baseSP_KSA <- rec_kmeans(baseSP, 13500, 50, 7, "SA", "mais proximo") # 10 minutos
baseSP_KTS <- rec_kmeans(baseSP, 13500, 50, 7, "TS", "mais proximo") # 4 minutos

#SOM
baseSP_SSA <- rec_SOM(baseSP, 13500, 50, 7, "SA", "mais proximo") # 10 minutos
baseSP_STS <- rec_SOM(baseSP, 13500, 50, 7, "TS", "mais proximo") # 3 minutos

#Funções Objetivos
funcao_objetivo(baseSP_KSA)
funcao_objetivo(baseSP_KTS)
funcao_objetivo(baseSP_SSA)
funcao_objetivo(baseSP_STS)



############################### 1000 pontos - UCHOA ###############################
uchoa1000 = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\Uchoa\\UCHOA_1001-k43.csv",header=TRUE,sep=";")
#Kmeans
uchoa1000_KSA <- rec_kmeans(uchoa1000, 800, 50, 7, "SA", "mais proximo")
uchoa1000_KTS <- rec_kmeans(uchoa1000, 800, 50, 7, "TS", "mais proximo")

#SOM
uchoa1000_SSA <- rec_SOM(uchoa1000, 800, 50, 7, "SA", "mais proximo")
uchoa1000_STS <- rec_SOM(uchoa1000, 800, 50, 7, "TS", "mais proximo") 

#Funções Objetivos
funcao_objetivo(uchoa1000_KSA)
funcao_objetivo(uchoa1000_KTS)
funcao_objetivo(uchoa1000_SSA)
funcao_objetivo(uchoa1000_STS)


############################### 800 pontos - UCHOA ###############################
uchoa800 = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\Uchoa\\UCHOA_801-k40.csv",header=TRUE,sep=";")
#Kmeans
uchoa800_KSA <- rec_kmeans(uchoa800, 2300, 50, 7, "SA", "mais proximo")
uchoa800_KTS <- rec_kmeans(uchoa800, 2300, 50, 7, "TS", "mais proximo")

#SOM
uchoa800_SSA <- rec_SOM(uchoa800, 2300, 50, 7, "SA", "mais proximo")
uchoa800_STS <- rec_SOM(uchoa800, 2300, 50, 7, "TS", "mais proximo")

#Funções Objetivos
funcao_objetivo(uchoa800_KSA)
funcao_objetivo(uchoa800_KTS)
funcao_objetivo(uchoa800_SSA)
funcao_objetivo(uchoa800_STS)


############################### 600 pontos - UCHOA ###############################
uchoa600 = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\Uchoa\\UCHOA_613-k62.csv",header=TRUE,sep=";")
#Kmeans
uchoa600_KSA <- rec_kmeans(uchoa600, 4700, 50, 7, "SA", "mais proximo")
uchoa600_KTS <- rec_kmeans(uchoa600, 4700, 50, 7, "TS", "mais proximo")

#SOM
uchoa600_SSA <- rec_SOM(uchoa600, 4700, 50, 7, "SA", "mais proximo")
uchoa600_STS <- rec_SOM(uchoa600, 4700, 50, 7, "TS", "mais proximo")

#Funções Objetivos
funcao_objetivo(uchoa600_KSA)
funcao_objetivo(uchoa600_KTS)
funcao_objetivo(uchoa600_SSA)
funcao_objetivo(uchoa600_STS)



############################### 400 pontos - UCHOA ###############################
uchoa400 = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\Uchoa\\UCHOA_401-k29.csv",header=TRUE,sep=";")
#Kmeans
uchoa400_KSA <- rec_kmeans(uchoa400, 43500, 20, 7, "SA", "mais proximo")
uchoa400_KTS <- rec_kmeans(uchoa400, 43500, 20, 7, "TS", "mais proximo")

#SOM
uchoa400_SSA <- rec_SOM(uchoa400, 43500, 20, 7, "SA", "mais proximo") 
uchoa400_STS <- rec_SOM(uchoa400, 43500, 20, 7, "TS", "mais proximo") 

#Funções Objetivos
funcao_objetivo(uchoa400_KSA)
funcao_objetivo(uchoa400_KTS)
funcao_objetivo(uchoa400_SSA)
funcao_objetivo(uchoa400_STS)



############################### 200 pontos - UCHOA ###############################
uchoa200 = read.csv("C:\\Users\\windows\\Documents\\Iniciação Científica\\Base de dados\\Uchoa\\UCHOA_200-k36.csv",header=TRUE,sep=";")
#Kmeans
uchoa200_KSA <- rec_kmeans(uchoa200, 2600, 5, 7, "SA", "mais proximo")
uchoa200_KTS <- rec_kmeans(uchoa200, 2600, 5, 7, "TS", "mais proximo")

#SOM
uchoa200_SSA <- rec_SOM(uchoa200, 2600, 5, 7, "SA", "mais proximo") 
uchoa200_STS <- rec_SOM(uchoa200, 2600, 5, 7, "TS", "mais proximo")

#Funções Objetivos
funcao_objetivo(uchoa200_KSA)
funcao_objetivo(uchoa200_KTS)
funcao_objetivo(uchoa200_SSA)
funcao_objetivo(uchoa200_STS)
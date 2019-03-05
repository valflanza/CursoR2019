
# Clase dia 1: 04/03/2019

# Introducción a los tipos de variables.

A <- 10
B = 2
C = A*B

Palabra_Para_Ejemplo = "10"

A*B
Palabra_Para_Ejemplo * B

## Convertir tipos
D = as.numeric(Palabra_Para_Ejemplo)


# Funciones
log(10)

resultadoFuncion = sqrt(log(A))

# Vectores
Vector = c(23,55,335,743,2345,21)
Vector2 = Vector*50
sqrt(Vector)

Vector * Vector2

Vector[2,3,4]

Vector[Vector == 55]

Vector == 55

## Ejercicio 1.1

Vectoraleatorio = runif(100, min = 0, max = 10)

VectorAleatorio

media = mean(Vectoraleatorio)
mediana = median(Vectoraleatorio)
desviacion = sd(Vectoraleatorio)


# Matrices

matriz = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, ncol = 3)

matriz[1,3]   #Extrea un elemento unico
matriz[1,]    #Extra una fila
matriz[,2]    #Extrae una columna
matriz[1:2,]  #Extrae un conjunto de filas
matriz[c(1,3),] #Extra un conjunto de filas (no consecutivas)

matriz[matriz > 5] #Extrae los elementos que cumplen una condicion.


## Ejercicio 2.1

Vectoraleatorio
Vectoraleatorio[Vectoraleatorio >3]
Resultado = matrix(Vectoraleatorio[Vectoraleatorio >3],ncol = 3)

## Ejercicio 2.2

Primeros50 = matrix(Vectoraleatorio[1:50], ncol = 5)

## Ejercicio 2.3

Segundos50 = matrix(Vectoraleatorio[51:100], ncol = 5)

Suma50 = Primeros50 + Segundos50

## Ejercicio 2.4

Matriz100 = matrix(runif(10000,min = 0,max = 10), ncol = 100, nrow = 100)


## Ejercicio 2.5

pares = c(1:25)*2
impares = pares -1


pares = seq(from = 2, to = 100, by = 2)

MatrizPares = Matriz100[pares,pares]



## Data.frame

df = data.frame(x = 1:3, y = c("A","B","C"))

DF = as.data.frame(MatrizPares)

iris = iris

columnas = cbind(iris$Sepal.Length,as.character(iris$Species))
DF = data.frame(iris$Sepal.Length,as.character(iris$Species))

colnames(DF)   ### Me devuelve el nombre de las columnas

colnames(DF) = c("Sepal.Length","Species")  ## Pongo el nombre de las columnas

iris[50:67,2:4]

iris[iris$Sepal.Length > 6.5,]

colMeans(iris[,1:4])

### Importar datos.
# Usar "Import Dataset" de la ventana derecha superior

## Ejercicio 4.2
mean(Films$budget, na.rm = TRUE)


## Ejercicio 4.3

NewDF = data.frame(Films$title,Films$rating)



## Las siguientes dos funciones tienen como finalidad almacenar en caché
## el inverso de una matriz. Se hará una descripción del paso a paso de
## cada una de éstas


## Mediante esta función, se asigna un valor a una matriz en un environment
## que es diferente al environment actual

makeCacheMatrix <- function(cm = matrix()) {
      
      ## se inicializa la matriz inversa con valores nulos
	cminversa <- NULL
      
	## se establece el valor de la matriz que se ingresa
	## como argumento en la función makeCacheMatrix
	set <- function (cm2){
		cm <<- cm2
		cminversa <<- NULL
	}

	## se establece el valor de la matriz inversa
	get <- function()cm
	setinversa <- function(inversa)cminversa <<- inversa

	## se obtiene el valor de la matriz inversa
	getinversa <- function()cminversa	
	list (set=set,get=get, setinversa=setinversa, getinversa=getinversa)

}


## Esta función calcula la inversa de la matriz creada, y verifica primero
## si la inversa se ha calculado. Si es así, obtiene el vaklor de la matriz inversa 
## del caché. En caso contrario, lo calcula.


cacheSolve <- function(cm, ...) {
 
	## verificación si la matriz inversa del argumento cm se ha calculado
	cminversa <- cm$getinversa()
	if(!is.null(cminversa)){
		message("matriz inversa en caché")
		return(cminversa)
	}

      ## cálculo de la matriz inversa
	data <- cm$get()
	cminversa <- solve(data,...)
	cm$setinversa(cminversa)
	cminversa

}

num_aleato <- sample(1 : 100, 1)
condador <- 1
while (condador <= 5) {
  cat('\nIngresa un numero')
  num <-readLines(n=1)
  num <-as.integer(num)
  
  if (num==num_aleato){
    cat ("\nFelicidades lo lograste en ', contador,'intentos \n")
    break
  }else{
    if (num < num_aleato){
      cat('\nEl numero es menor al numero generado')
    }else{
      cat('\nEl numero es mayor al numero generado')
    }
  
    cat('\nSuerte para la proxima')  
  }
  condador <- condador + 1
}
cat('\nLastima no pudiste adivinar el numero que era: ', num_aleato,' en ', condador, ' intentos')

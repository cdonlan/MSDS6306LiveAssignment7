

#part B
base10to7<-function(x){
  
  if(!is.numeric(x))
    stop("Argurment must be numeric")
  
  i=0
  sum=0
  while(x%/%7!=0){
    sum<-sum+((x%%7)*(10^i))
    i=i+1
    x<-x%/%7
  }
  sum<-sum+((x%%7)*(10^i))
  return(sum)
}

#part A
p7 <-function(x) 
{
  if(!is.numeric(x))
    stop("Argurment must be numeric")
  
  results <- c()
  i = 0
  
  while(i < x)
  {
    results <- c(results,base10to7(i))
    i <- i +1
  }
  
  return(results)
}


#part C
base7to10 <- function(x){
  
  if(!is.numeric(x))
    stop("Argurment must be numeric")
  
  y <-nchar(x)
  result = 0
  for(i in 1:y)
  {
    #print(y)
    digit <- as.numeric(substr(x,(y-i+1),(y-i+1)))
    #print(digit)
    converted <-digit*(7^(i-1))
    
    #print(converted)
    result <- result + converted
  }
  
  return(result)
}

#part D
baseKto10 <- function(k,x){
  
  if(!is.numeric(k))
    stop("First argurment must be numeric")
  
  if(!is.numeric(x))
    stop(" Second argurment must be numeric")
  
  xchar <-nchar(x)
  result = 0
  for(i in 1:xchar)
  {
    #print(y)
    digit <- as.numeric(substr(x,(xchar-i+1),(xchar-i+1)))
    #print(digit)
    converted <-digit*(k^(i-1))
    
    #print(converted)
    result <- result + converted
  }
  
  return(result)
}


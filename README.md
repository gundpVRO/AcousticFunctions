# AcousticFunctions

Author: vro
Date:   2020-10-05


## Energetic sum
```{r}
EnergSum <- function(...){
  x = list(...)
  n = length(unlist(x))
  total=0
  for(i in 1:n){
    total <- total + (10^(0.1*unlist(x)[i]))
  }
  total <- 10*log10(total)
  # print(n)
  # print(total)
}
```

## Energetic sum with final result
With this you will receive the result if you wish to see it
```{r}
EnergSumView <- function(...){
  x = list(...)
  n = length(unlist(x))
  total=0
  for(i in 1:n){
    total <- total + (10^(0.1*unlist(x)[i]))
  }
  total <- 10*log10(total)
  print(n)
  return(total)
  cat("Energetic Sum: ", total, "\n")
}
```

Note that if you want to calculate several energetic sums, you should use the first formula in a loop

## Energetic Mean
```{r}
EnergMW <- function(...){
  x = list(...)
  n = length(unlist(x))
  total=0
  for(i in 1:n){
    total <- total + (10^(0.1*unlist(x)[i]))
  }
  total <- total/n
  total <- 10*log10(total)
  # print(n)
  # print(total)
}
```
## Energetic mean with result returned
```{r}
EnergMWView <- function(...){
  x = list(...)
  n = length(unlist(x))
  total=0
  for(i in 1:n){
    total <- total + (10^(0.1*unlist(x)[i]))
  }
  total <- total/n
  total <- 10*log10(total)
  print(n)
  cat("Energetic Mean: ", total, "\n")
}
```

## Function for “Mischverkehr”
```{r}
Mischverkehr <- function(N1, N2, p, v=50){
  stlmvk = -(10*log10(10^((43+10*log10(1+(v/50.0)^3)+10*log10(1-p))/10)
                         +10^((43+10*log10((1+(v/50.0)^3)*(1+20*(1-(v/150.0))))
                                +10*log10(p))/10))-10*log10(10^((43+10*log10(1+(v/50.0)^3)+10*log10(1-p)+N1)/10)
                                                                  +10^((43+10*log10((1+(v/50.0)^3)*(1+20*(1-(v/150.0))))+10*log10(p)+N2)/10)))
  return(stlmvk)
}
```
How to use this function:
* N1 is cars
* N2 is heavy trafic
* p is percentage of heavy trafic
* v is velocity

## Conversions
### Bei Ref-Geschwindigkeit 50 km/h
```{r}
Ref50PW <- function(CPXp){
  KB50kmhPW = 1.2468 * CPXp-112.3
  return(KB50kmhPW)
}

Ref50LKW <- function(CPXh){
  KB50kmhLKW = 1.3617 * CPXh-126.16
  return(KB50kmhLKW)
}  
```
### Bei Ref-Geschwindigkeit 80 km/h
```{r}
Ref80PW <- function(CPXp){
  KB80PW = 1.5152 * CPXp - 147.17
  return(KB80PW)
}

Ref80LKW <- function(CPXp){
  KB80LKW = 1.3949 * CPXp - 135.81
  return(KB80LKW)
}
```

















#******************************************
# Title:       Acoustic functions for R and CPX
# Source:      (I:\temp\_User\fs\Install_python\gundp\acoustic_functions.py) # URL or similar
# Keywords:    -- # separate with ","
#******************************************
# Author:      vro
# ProjNo:      (ProjNo)
# Date:        06-03-2019
# Time:        11:03
#******************************************
# Modified by:    --
# Modified date:  --
#
# Notes:
# Die Norm ist abgelegt unter: O:\Belagsakustik\Normen\
# Es handelt sich dabei um die Norm: ISO/FDIS 11819-2:2016 (Seite 16)
#
#
#******************************************


#**************************
#---- Libs & Functions ----
#**************************


#*******************
#---- Dataspace ----
#*******************


#**************
#---- Main ----
#**************

# # Energetische Addition mit 3 Inputs----
# AkkAdd <- function(a1, a2, a3, n){
#   Summenpgl = 10 * (log10((10^(0.1 * a1)) + (10^(0.1*a2)) + (10^(0.1*a3))))
#   return(Summenpgl)
# }
# Energetische Addition mit n Inputs
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

# # Energetischer Mittelwert mit zwei Inputs----  
# EnergMittW <- function(a1, a2, n){
#   EnMW = 10 * log10((10^(0.1*a1)+10^(0.1*a2))/n)
#   return(EnMW)
# }

# Energetisches Mittel mit n inputs

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


# Berechnung Mischverkehr----
Mischverkehr <- function(N1, N2, p, v=50){
  stlmvk = -(10*log10(10^((43+10*log10(1+(v/50.0)^3)+10*log10(1-p))/10)
                         +10^((43+10*log10((1+(v/50.0)^3)*(1+20*(1-(v/150.0))))
                                +10*log10(p))/10))-10*log10(10^((43+10*log10(1+(v/50.0)^3)+10*log10(1-p)+N1)/10)
                                                                  +10^((43+10*log10((1+(v/50.0)^3)*(1+20*(1-(v/150.0))))+10*log10(p)+N2)/10)))
  return(stlmvk)
}


# calc_ESM <- function(df_in, col_names, axis=1){
#   return(log10(10^(.1 * df_in)))
#   }
# 


# Umrechnungen
Ref50PW <- function(CPXp){
  KB50kmhPW = 1.2468 * CPXp-112.3
  return(KB50kmhPW)
}

Ref50LKW <- function(CPXh){
  KB50kmhLKW = 1.3617 * CPXh-126.16
  return(KB50kmhLKW)
}  

# Bei Ref-Geschwindigkeit 80

Ref80PW <- function(CPXp){
  KB80PW = 1.5152 * CPXp - 147.17
  return(KB80PW)
}

Ref80LKW <- function(CPXp){
  KB80LKW = 1.3949 * CPXp - 135.81
  return(KB80LKW)
}



#**************************************************
# Temperature adjustment

# Version with different road types
# tempAdj <- function(Tp, V, rdtyp){
#   yt <- if(rdtyp=="SMA"||rdtyp=="ACMR8" || rdtyp=="TAL" || rdtyp=="AC11" || rdtyp=="SDA4" || rdtyp){
#     -0.14+0.0006 * V
#     } else {
#       if(rdtyp=="Beton"){
#         -0.08+0.0004 * V
#       } else{
#       -0.10 + 0.0004 * V}}
#   CTt = yt*(Tp-20)
# }

# Version for RRT ()
# V = effektive Geschwindigkeit
# Tp = effektive Temperatur
# rdtype = Belag

tempAdj <- function(Tp, V, rdtyp){
  yt <- ifelse(rdtyp=="Beton", -0.10 + 0.0004*V, 
               ifelse(rdtyp=="PA11", -0.08 + 0.0004*V,-0.14 + 0.0006*V))
  
  #   if(rdt=="Beton"){
  #   -0.10 + 0.0004 * V
  # } else {
  #   -0.14 + 0.0006 * V
  # }
  return(CTt = yt * (Tp-20))
}



#****************************************************
# Korrekturen CPX
# Speed correction CPX----

# BconstDense <- 30
# BconstCement <- 35
# BconstAll <- 30
# BconstPorous <- 25

SpCorr <- function(rdtyp, Vref, V){
  bconst <- ifelse(rdtyp=="Beton", 35, 
                   ifelse(rdtyp=="PA11", 25, 30))
  
  return(-bconst*log10(V/Vref))
  }


# ShoreA Correction

ShCorr <- function(Ttype, HA){
  beta <- ifelse(Ttype=="P1", 0.12, 0.2)
  
  return(beta*(HA-66))
}


# # Trailer correction
# trailerC <- function(...){
#   x = list(...)
#   n = length(unlist(x))
#   total=0
#   for(i in 1:n){
#     total <- total + (10^(0.1*(unlist(x)[i]+)))
#   }
#   total <- 10*log10(total)
#   # print(n)
#   # print(total)
# }



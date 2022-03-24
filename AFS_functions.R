##Formules random utilisables
## Distance géodésique: 2*arcos(sum(sqrt(p*q)))


dist_geod<- function(x,y){
  return (2*acos(sum(sqrt(x*y))))
}

Hellingerizer<-function(x){

  return (sign(x)*sqrt(abs(x)))
}


Nuage_X<-function(t,mI){
  ##mI proba sur I mJ proba sur J

  I<-length(mI)
  X<-t%*%diag(1/sqrt(mI))
return(X)
}

Nuage_Y<-function(t,mJ){
  ##mI proba sur I mJ proba sur J

  J<-length(mJ)
  Y<-diag(1/sqrt(mJ))%*%t
  return(Y)
}


turn_to_freq<-function(x){
  return(x/sum(x))
}
Profil_ligne<-function(x){
  for (i in 1:length(x[,1])){
    x[i,]<-turn_to_freq(x[i,])
  }
  return(x)

}
Profil_colonne<-function(x){
  for (i in 1:length(x[1,])){
    x[,i]<-turn_to_freq(x[,i])
  }
  return(x)

}


I_alpha<-function (p,q,alpha){
  return ((1/(alpha-1))*log2((sum(((p^alpha)/(q^alpha))*p))))

}

dist_Hellinger<-function(p,q){
  return (sum((sqrt(p)-sqrt(q))^2))
}





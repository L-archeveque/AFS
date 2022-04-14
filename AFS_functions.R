##Formules random utilisables
## Distance géodésique: 2*arcos(sum(sqrt(p*q)))


dist_geod<- function(x,y){
  return (2*acos(sum(sqrt(x*y))))
}

Hellingerizer<-function(x){

  return (sign(x)*sqrt(abs(x)))
}


Nuage_X<-function(t,mI){

  I<-length(mI)
  X<-diag(1/sqrt(mI))%*%t
return(X)
}

Nuage_Y<-function(t,mJ){


  Y<-t%*%diag(1/sqrt(mJ))
  return(Y)
}


turn_to_freq<-function(x){
  return(x/sum(abs(x)))
}
poids_lignes<-function(f){

  I<-length(f[,1])

  mI<-rep(0,I)

  for (i in 1:I){
    mI[i]<-sum(f[i,])
}

  return(mI)
}

poids_colonne<-function(f){
  J<-length(f[1,])
  mJ<-rep(0,J)
  for (j in 1:J){
    mJ[j]<-sum(f[,j])


  }
  return(mJ)
}
depistage<-function(t){
  J<-length(f[1,])
  I<-length(f[,1])
  F2<-f
  for(j in 1:J){
    if(sum(f[,j]==0)){F2<-f[,-j]}}
  for ( i in 1:I){
    if(sum(f[i,]==0)){F2<-f[-i,]}
  }

return(F2)
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

setClass("ACP",representation(lbd="numeric",U_vects="matrix",W_vects="matrix",compos_F="matrix",compos_G="matrix",
                                 cos2="matrix",CTR="matrix",CONTR="numeric"))


produit_des_marges<-function(mI,mJ){
  phi<-matrix(mI,ncol=1)%*%aperm(matrix(mJ,ncol = 1))

return (phi)
}


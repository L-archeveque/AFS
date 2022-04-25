##Formules random utilisables
## Distance géodésique: 2*arcos(sum(sqrt(p*q)))


dist_geod<- function(x,y){
  return (2*acos(sum(sqrt(x*y))))
}

Hellingerizer<-function(x){

  return (sign(x)*sqrt(abs(x)))
}


Nuage_X<-function(t,mI=poids_lignes(t)){
  Z<-depistage(t)
  X<-Z@mat
  if (Z@ili != c(0)){
    linull<-Z@ili
    n<-length(linull)
    for (x in 1:n){ i<-linull[x]
    mI<-mI[-i]}
  }
  X<-diag(1/sqrt(abs(mI)))%*%X
  return(X)
}
Nuage_Y<-function(t,mJ=poids_colonne(t)){
  Z<-depistage(t)
  Y<-Z@mat
  if (Z@icol != c(0)){
    conull<-Z@icol
    n<-length(conull)
    for (x in 1:n){j<-conull[x]
    mJ<-mJ[-j]}}
  Y<-Y%*%diag(1/sqrt(abs(mJ)))
  return(Y)
}
poids_colonne<-function(f){
  J<-length(f[1,])
  mJ<-rep(0,J)
  for (j in 1:J){
    mJ[j]<-sum(f[,j])
                    }
  return(mJ)
}
poids_lignes<-function(f){
  J<-length(f[,1])
  mI<-rep(0,J)
  for (j in 1:J){
    mI[j]<-sum(f[j,])}
  return(mI)}
    
    
    
setClass("matrice_d",representation(mat="matrix",icol="numeric",ili="numeric"))
         
         depistage<-function(f){
           J<-length(f[1,])
           I<-length(f[,1])
           F2<-f
           ind_li<-c()
           ind_col<-c()
           for(j in 1:J){
             if(sum(f[,j])==0){F2<-f[,-j]
             
             ind_col<-c(ind_col,j)}}
           for (i in 1:I){
             if(sum(f[i,])==0){F2<-f[-i,]
             ind_li<-c(ind_li,j)}}
           if(is.null(ind_col)){ind_col<-c(0)}
           if(is.null(ind_li)){ind_li<-c(0)}
           r=new("matrice_d",mat=F2,ili=ind_li,icol=ind_col)
           return(r)
         }


         turn_to_freq<-function(x){
           return(x/sum(abs(x)))
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


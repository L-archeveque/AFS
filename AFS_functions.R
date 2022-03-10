##Formules random utilisables
## Distance géodésique: 2*arcos(sum(sqrt(p*q)))
install.packages(geosphere)
N=100
dist_geod<- function(x,y){
  return (2*acos(sum(sqrt(x*y))))

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
usethis::use_git_config(user.name = "L-archeveque", user.email = "valentin.larcheveque@etu.umontpellier.fr")
credentials::set_github_pat("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")
usethis::create_github_token()
edit_r_environ()
usethis::edit_r_environ()
usethis::git_sitrep()

sum(sqrt(x*y))
library("pracma")
install.packages("pracma")

install.packages("distrEx")
library("distrEx")

p<-rep(1/6,6)
q<-(1:6)/sum(1:6)
dist_Hellinger(p,q)
dist_Hellinger(p,p)
B<-matrix((9:1),3,3,byrow=T)/sum(1:9)
dist_Hellinger(B,A)

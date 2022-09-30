# CARREGANDO PACOTES ------------------------------------------------------
library(glmnet)


# Gerando a Matriz de pesos sinápticos ----------------------------------------------
set.seed(1)
n = 5
w = matrix(c(0,0,5,0,5,
             0,0,5,5,0,
             5,5,0,0,5,
             0,5,0,0,5,
             -20,0,-20,-20,0), nrow=n,ncol=n, byrow = T)

#neurons <- c(1:n); #set of neurons
#exc <- vector(); #set of excitatory neurons
#ini <- vector(); #set of inhibitory neurons
#exc <- c(1:(round(0.8*n)));
#ini <- c((1+exc[length(exc)]):n);
#
#for(i in 1:n){
#  if (i %in% exc){
#    w[i,] <- 5;
#  }
#  else if (i %in% ini){
#    w[i,] <- -20 ;
#  }
#  w[i,i] <- 0;
#  nulo <- vector();
#  nulo <- sample(neurons[-i], round((1/6)*n));
#  w[i,nulo] <- 0
#  w[nulo,i] <- 0;
#}






# Funções necessárias para gerar X ----------------------------------------

#Função que seleciona todas as linhas da matriz "m" menos a do "indice" indicado 
matriz_pesos = function(m,indice){
  matriz_pesos = matrix(c(m[!(row(m) %in% c(indice))]), nrow=nrow(m)-1)
  return(matriz_pesos)
}

#Função que seleciona a linha "indice" da matriz "m" indicada
matriz_x = function(m,indice){
  matriz_x = c(m[(row(m) %in% c(indice))])
  return(matriz_x)
}

#Função taxa de disparo logísstica
phi = function(x){
  p = exp(x)/(1+exp(x))
  return(p)
}

#Função verifica indice do ultimo disparo que ocorreu na sequência de disparos x, que á a linha do neurônio especificada
ultimo = function(x,tempo_inicial,tempo_final){
  u = tail(which(x[tempo_inicial:tempo_final] == 1), n = 1)
  return(u)
}

#Função que calcula o potencial de membrana do neurônio "indice_neurônio" no tempo t 
V_novo = function(X,indice_neuronio,tempo){
  potencial = if(ultimo(matriz_x(X,indice_neuronio),1,tempo) == tempo) 0  
  else if(ultimo(matriz_x(X,indice_neuronio),1,tempo) != (tempo-1)) sum((1/(2^(tempo - ultimo(matriz_x(X,indice_neuronio),1,tempo))))*rowSums(matriz_pesos(X,indice_neuronio)[,(ultimo(matriz_x(X,indice_neuronio),1,tempo)+1):tempo])%*%matriz_pesos(w,indice_neuronio)[,indice_neuronio])
  else sum((1/(2^(tempo - ultimo(matriz_x(X,indice_neuronio),1,tempo))))*matriz_pesos(X,indice_neuronio)[,(ultimo(matriz_x(X,indice_neuronio),1,tempo)+1):tempo]%*%matriz_pesos(w,indice_neuronio)[,indice_neuronio])
  return(potencial)
}

#ifelse((ultimo(matriz_x(X,indice_neuronio),1,tempo)+1) != tempo, sum((1/(2^(tempo - ultimo(matriz_x(X,indice_neuronio),1,tempo))))*rowSums(matriz_pesos(X,indice_neuronio)[,(ultimo(matriz_x(X,indice_neuronio),1,tempo)+1):tempo])%*%matriz_pesos(w,indice_neuronio)[,indice_neuronio]),
#       sum((1/(2^(tempo - ultimo(matriz_x(X,indice_neuronio),1,tempo))))*sum(matriz_pesos(X,indice_neuronio)[,(ultimo(matriz_x(X,indice_neuronio),1,tempo)+1):tempo])%*%matriz_pesos(w,indice_neuronio)[,indice_neuronio]))


##Função que gera a cadeia X
gera_cadeia <- function(X){
  for (j in 5:t){
    for (i in 1:n){
      X[i,j] = ifelse(runif(1,0,1) <= phi(V_novo(X,i,j-1)), 1, 0)
    }
  }
  return(X)
}



##Função para gerar Y, se necessário
#gera_Y <- function(y,x){
#  for (j in 1: n){
#    for ( i in 1:t){
#      X_aux = X[,1:(i+4)]
#      u = ultimo(X_aux[j,],(i+4),1)
#      y[i,j] = ifelse(u==(i+4),
#                      t(X_aux[,ultimo(X_aux[j,],(i+4),1):(i+4)])%*%w[,j]
#                      ,t(rowSums(X_aux[,ultimo(X_aux[j,],(i+4),1):(i+4)]))%*%w[,j])
#    }
#  }
#  return(y)
#}


w



# Estimação ---------------------------------------------------------------

#if (u==(i+4)) (1/(2^((i+4) - ultimo(matriz_x( X_aux,j),1,(i+4)))))*t(X_aux[,ultimo(X_aux[j,],1,(i+4)):(i+4)]) else (1/(2^((i+4) - ultimo(matriz_x( X_aux,j),1,(i+4)))))*t(rowSums(X_aux[,ultimo(X_aux[j,],1,(i+4)):(i+4)]))

t = 48000/0.8
n_interacoe <- 100

for (a in 1:n_interacoe){

set.seed(a)
X = matrix(c(1,0,0,1,
               0,1,1,0,
               0,0,1,0,
               0,1,0,1,
               1,0,0,0), nrow=n, byrow=T)
  
  

X = cbind(X, matrix(0,nrow=n,ncol=t))
X <- gera_cadeia(X)

for (j in 1: n){
  z = matrix(0,nrow=t, ncol = n)
  for ( i in 1:t){
    X_aux = X[,1:(i+3)]
    u = ultimo(X_aux[j,],1,(i+3)) 
    z[i,]  = if (u==(i+3)) matrix(0,ncol=n,nrow=1)  else if (u==(i+2))  (1/(2^((i+3) - ultimo(matriz_x( X_aux,j),1,(i+3)))))*t(X_aux[,(ultimo(X_aux[j,],1,(i+3))+1):(i+3)]) else (1/(2^((i+3) - ultimo(matriz_x( X_aux,j),1,(i+3)))))*t(rowSums(X_aux[,(ultimo(X_aux[j,],1,(i+3))+1):(i+3)]))

  }
  z = z[-(1:(0.2*t)),]
  nome = paste0("matriz", j)
  assign(nome, z)
}


for (i in 1:n){
  resp <- X[i,(5 + 0.2*t):(t+4)]
  nome_resp <- paste0('y',i)
  assign(nome_resp,resp)
}


w_est <- matrix(0,nrow=n,ncol=n)
for (i in 1:n){
  mod <- cv.glmnet(get(paste0('matriz',i)), as.factor(get(paste0('y',i))), intercept=F, family='binomial')
  w_est[,i] <- as.numeric(coef(mod, s='lambda.min'))[-1]
}

assign(paste0('w',a), w_est)
print(a)

}


# Métricas ----------------------------------------------------------------

soma_w <- matrix(0,nrow=n,ncol=n)
for (i in 1:n_interacoe){
  soma_w <- soma_w  + (w - get(paste0('w',i)))^2 
}


eqm <- round(soma_w/n_interacoe,4)
eqm


w_media  <- matrix(0,nrow=n, ncol=n)
for (i in 1:n_interacoe){
  w_media <- w_media + get(paste0('w',i))
}
w_media/n_interacoe



w_t <- ifelse(w!=0,1,0)
for (i in 1:n_interacoe){
t <- ifelse(get(paste0('w',i))!=0, 1,0)  
m <- ifelse(t == w_t,1,0)
assign(paste0('m',i), m)
}


soma_m <- matrix(0,nrow=n,ncol=n)
for (i in 1:n_interacoe){
  soma_m <- soma_m  + get(paste0('m',i)) 
}
soma_m/100



w_t <- ifelse(w>0,'exi',ifelse(w<0,'inib',0))
w_t


for (i in 1:n_interacoe){
  m <- ifelse(get(paste0('w',i))>0,'exi',ifelse(get(paste0('w',i))<0,'inib',0))
  vec[i] = ifelse(m==w_t,'correto','errado')
}
which(vec=='correto')



# Salvando ----------------------------------------------------------------
save.image(file = 'C:\\Users\\vitor\\Desktop\\Códigos TG\\Simulações\\12000tempos_100int.RData')



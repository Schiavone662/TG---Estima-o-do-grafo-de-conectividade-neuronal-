load('C:\\Users\\vitor\\Desktop\\Códigos TG\\Simulações\\120tempos_100int.RData') 


soma_w <- matrix(0,nrow=n,ncol=n)
for (i in 1:n_interacoe){
  soma_w <- soma_w  + (w - get(paste0('w',i)))^2 
}

soma_w
eqm <- round(soma_w/n_interacoe,4)
eqm



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
conex <- soma_m/100








w_t <- ifelse(w>0,'exi',ifelse(w<0,'inib',0))



for (i in 1:n_interacoe){
  t <- ifelse(get(paste0('w',i))>0, 'exi', ifelse(get(paste0('w',i))<0,'inib',0))  
  m <- ifelse(t == w_t,1,0)
  assign(paste0('m',i), m)
}


soma_m <- matrix(0,nrow=n,ncol=n)
for (i in 1:n_interacoe){
  soma_m <- soma_m  + get(paste0('m',i)) 
}
peso <- soma_m/100





sup_estimativa_48 <- eqm[3,5]
#0.0422
sup_estimativa_24 <- eqm[3,5]
#0.0729
sup_estimativa_12 <- eqm[3,5]
#0.0986
sup_estimativa_012 <- eqm[3,5]
#0.8803
sup_estimativa_0012 <- eqm[3,5]
#124.4655

dados <- data.frame(amostra = c(120,1200,12000,24000,48000), eqm = c(124.4655,0.8803,0.0986,0.0729,0.0422))

library(ggplot2)
library(grid)

grafico <- ggplot(dados, aes(x=amostra, y=eqm)) + 
  geom_line(col='navy', size=1) + 
  geom_point(col='black')+
  theme_bw() + 
  labs(x='Tamanho da amostra',y='Erro quadrático médio', title='Erro quadrático médio pelo tamanho amostral') + 
  theme(plot.title=element_text(hjust = 0.5)) +
  xlim(0,50000) + 
  annotate("text", x=1000,y=124.4655,label='124.4655', vjust=-.20,
           col='darkred', angle=360) + 
  annotate("text", x=3000,y=1,label='0.8803', vjust=-.20,
           col='darkred', angle=360) + 
  annotate("text", x=12000,y=1,label='0.0986', vjust=-.20,
           col='darkred', angle=360) + 
  annotate("text", x=24000,y=1,label='0.0729', vjust=-.20,
           col='darkred', angle=360) + 
  annotate("text", x=48000,y=1,label='0.0422', vjust=-.20,
           col='darkred', angle=360) +
  scale_x_continuous(breaks=c(120,1200,12000,24000,48000)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
grafico

ggsave('EQM.pdf',grafico,height = 12,width = 22, units = c('cm'), path = 'C:\\Users\\vitor\\Desktop')


#+ 
#    geom_segment(aes(x = 120 , y = 0, xend = 120, yend = 124.4655)) + 
#    geom_segment(aes(x = 1200 , y = 0, xend = 1200, yend = 0.8803)) + 
#    geom_segment(aes(x = 12000 , y = 0, xend = 12000, yend = 0.0986)) + 
#    geom_segment(aes(x = 24000 , y = 0, xend = 24000, yend = 0.0729)) +
#    geom_segment(aes(x = 48000 , y = 0, xend = 48000, yend = 0.0422))  
  
  
  
#+ 
#  scale_x_continuous(breaks=c(120,1200,12000,24000,48000,50000))  
  
  
  
grafico





dados_zoom<- data.frame(amostra = c(1200,12000,24000,48000), eqm = c(0.8803,0.0986,0.0729,0.0422))

library(ggplot2)
library(grid)

grafico_zoom <- ggplot(dados_zoom, aes(x=amostra, y=eqm)) + 
  geom_line(col='navy', size=1) + 
  geom_point(col='black')+
  theme_bw() + 
  labs(x='Tamanho da amostra',y='Erro quadrático médio', title='Erro quadrático médio pelo tamanho amostral') + 
  theme(plot.title=element_text(hjust = 0.5)) +
  xlim(0,50000)  + 
  annotate("text", x=1200,y=0.9,label='0.8803', vjust=-.20,
           col='darkred', angle=360) + 
  annotate("text", x=14000,y=0.11,label='0.0986', vjust=-.20,
           col='darkred', angle=360) + 
  annotate("text", x=24000,y=0.09,label='0.0729', vjust=-.20,
           col='darkred', angle=360) + 
  annotate("text", x=48000,y=0.06,label='0.0422', vjust=-.20,
           col='darkred', angle=360) +
  scale_x_continuous(breaks=c(1200,12000,24000,48000)) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
grafico_zoom

ggsave('grafico_zoom.pdf',grafico_zoom,height = 12,width = 22, units = c('cm'), path = 'C:\\Users\\vitor\\Desktop')







soma_w <- matrix(0,nrow=n,ncol=n)
for (i in 1:n_interacoe){
  soma_w <- soma_w  + get(paste0('w',i))
}
round(soma_w/100,1)

w15

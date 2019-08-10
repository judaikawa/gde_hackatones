## TRABALHO ME430 - AMOSTRAGEM

# Respostas questionario Google Docs
library(readxl)
dados<-read_excel("Trabalho ME430 (respostas).xlsx",sheet = 1)
dados<-dados[,-1]

names(dados)
names(dados)<-c("Ano"                                          
                ,"Curso"                                                    
                ,"Cursou_MC102"                
                ,"Qtd_Vezes_MC102"
                ,"Facilidade_MC102"     
                ,"Interesse_MC102"                          
                ,"Outra_materia_MC"                           
                ,"Nota_MC102"           
                ,"Cursou_ME323"                
                ,"Qtd_Vezes_ME323"
                ,"Facilidade_ME323"  
                ,"Interese_ME323"                       
                ,"Outra_Materia_ME"                           
                ,"Nota_ME323")

# Erros em algumas respostas de notas
dados$Nota_MC102 <- as.character(dados$Nota_MC102)
dados$Nota_ME323 <- as.character(dados$Nota_ME323)
dados$Nota_MC102[135]<-5
dados$Nota_ME323[102]<-5
dados$Nota_MC102[50]<-8.9



# Calculo para amostra estratificada para 5%
N<-nrow(cursosok)
B<-0.05
z<-1.96
D<-(B/z)^2
n<-N/(4*(N-1)*D+1)

# Total populacional de cada curso 
N_estat<-nrow(cursosok[which(grepl("Estat",cursosok$Curso)),])
N_cienc_comp<-nrow(cursosok[which(grepl("Ci",cursosok$Curso)),])
N_eng_comp<-nrow(cursosok[which(grepl("Engenharia",cursosok$Curso)),])

# Estratos 
W_estat<-N_estat/N
W_cienc_comp<-N_cienc_comp/N
W_eng_comp<-N_eng_comp/N

W_estat+W_eng_comp+W_cienc_comp

# Amostra

Amostra<-data.frame(Estrato=c("Estatística","Ciência da computação","Engenharia da Computação"),N_h=c(N_estat,N_cienc_comp,N_eng_comp),W_h=c(W_estat,W_cienc_comp,W_eng_comp))

Amostra$n_h<-0
n<-round(n)
for(i in 1:3){
  Amostra$n_h[i]<-round(Amostra$W_h[i]*n)
}

# Calculo para amostra estratificada para 10%
N<-nrow(cursosok)
B2<-0.1
z2<-1.96
D2<-(B2/z2)^2
n2<-N/(4*(N-1)*D2+1)

# Numero minimo para respeitar a proporcionalidade
n2 <- round(48/W_eng_comp)

# Amostra 2
Amostra2<-data.frame(Estrato=c("Estatística","Ciência da computação","Engenharia da Computação"),N_h=c(N_estat,N_cienc_comp,N_eng_comp),W_h=c(W_estat,W_cienc_comp,W_eng_comp))

Amostra2$n_h<-0
n2<-round(n2)
for(i in 1:3){
  Amostra2$n_h[i]<-round(Amostra2$W_h[i]*n2)
}


dados_eng_comp<-dados[which(grepl("Engenharia",dados$Curso)),]
dados_eng_comp<-dados_eng_comp[,-c(3:8)]
dados_estat<-dados[which(grepl("Estat",dados$Curso)),]
dados_estat<-dados_estat[,c(1:8)]
dados_cienc_comp<-dados[which(grepl("Ci",dados$Curso)),]
dados_cienc_comp<-dados_cienc_comp[,-c(3:8)]
dados_comp<-rbind(dados_eng_comp,dados_cienc_comp)

# Amostra final
samp_estat<-dados_estat[sample(nrow(dados_estat),Amostra2$n_h[1],replace = FALSE),]
samp_cienc_comp<-dados_cienc_comp[sample(nrow(dados_cienc_comp),Amostra2$n_h[2],replace = FALSE),]
samp_eng_comp<-dados_eng_comp[sample(nrow(dados_eng_comp),Amostra2$n_h[3],replace = FALSE),]
samp_comp<-dados_comp[sample(nrow(dados_comp),Amostra2$n_h[2]+Amostra2$n_h[3],replace = FALSE),]

rm(dados_cienc_comp)
rm(dados_comp)
rm(dados_estat)
rm(dados_eng_comp)

# Estimação media de nota
m_geral <- mean(samp_geral$nota)
m_estat <- mean(as.numeric(samp_estat$Nota_MC102))
m_cc <- mean(as.numeric(samp_cienc_comp$Nota_ME323))
m_eg <- mean(as.numeric(samp_eng_comp$Nota_ME323))


#samp_comp$Curso[which(samp_comp$Curso=="Engenharia da Computação")]<-"Computação"
#samp_comp$Curso[which(samp_comp$Curso=="Ciência da Computação")]<-"Computação"

samp_geral<-data.frame(Curso=c(samp_estat$Curso,samp_comp$Curso),Ano=c(samp_estat$Ano,samp_comp$Ano),Interesse=c(samp_estat$Interesse_MC102,samp_comp$Interese_ME323),Facilidade=c(samp_estat$Facilidade_MC102,samp_comp$Facilidade_ME323),Qtd_cursada=c(samp_estat$Qtd_Vezes_MC102,samp_comp$Qtd_Vezes_ME323),outra_materia=c(samp_estat$Outra_materia_MC,samp_comp$Outra_Materia_ME),nota=c(samp_estat$Nota_MC102,samp_comp$Nota_ME323))
library(ggplot2)

samp_geral$nota<-as.double(as.character(samp_geral$nota))

ggplot(samp_geral,aes(x=Curso,y=nota,fill=Interesse))+geom_bar(stat="identity",position="dodge")
ggplot(samp_geral,aes(x=Curso,y=nota,fill=Interesse))+geom_boxplot()

ggplot(samp_geral,aes(x=Curso,y=nota,fill=Facilidade))+geom_bar(stat="identity",position="dodge")


b<-aggregate(list(Media=samp_geral$nota),by=list(samp_geral$Curso),FUN=mean)
aFacilidade<-aggregate(list(Nota=samp_geral$nota),by=list(samp_geral$Facilidade,samp_geral$Curso),FUN=mean)
aInteresse<-aggregate(list(Nota=samp_geral$nota),by=list(samp_geral$Interesse,samp_geral$Curso),FUN=mean)
aAno<-aggregate(list(Nota=samp_geral$nota),by=list(as.factor(samp_geral$Ano),samp_geral$Curso),FUN=mean)
aQtdCursada<-aggregate(list(Nota=samp_geral$nota),by=list(as.factor(samp_geral$Qtd_cursada),samp_geral$Curso),FUN=mean)

c<-aggregate(list(Media=samp_geral$Qtd_cursada),by=list(samp_geral$Interesse,samp_geral$Curso), FUN=mean)

ggplot(aFacilidade,aes(fill=Group.1,y=Nota,x=Group.2))+geom_bar(stat="identity",position="dodge")
ggplot(aQtdCursada,aes(fill=Group.1,y=Nota,x=Group.2))+geom_bar(stat="identity",position="dodge")
ggplot(aAno,aes(fill=Group.1,y=Nota,x=Group.2))+geom_bar(stat="identity",position="dodge")
ggplot(aInteresse,aes(fill=Group.1,y=Nota,x=Group.2))+geom_bar(stat="identity",position="dodge")

ggplot(c,aes(fill=Group.1, y=Media, x=Group.2))+geom_bar(stat="identity", position = "dodge")



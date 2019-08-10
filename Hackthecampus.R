install.packages("magrittr")
install.packages("httr")
install.packages("XML")
install.packages("rvest")
library(magrittr)
library(httr)
library(XML)
library(rvest)

load("concluintes")
load("origem")
origem$cidade <- sapply(strsplit(origem$uf," - "), "[", 1)
origem$uf <- sapply(strsplit(origem$uf," - "), "[", 2)

alunosRA <- c(as.numeric(origem$ra))
curso <- c()
erroRA <- c()

concluintes$ra <- as.integer(gsub("[[:punct:]]", "", levels(factor(concluintes$ra))))
origem <- origem[which(!origem$ra %in% concluintes$ra),]
rownames(origem) <- 1:nrow(origem)

for (i in 1:length(alunosRA)) {
  tryCatch({
    url <-"https://grade.daconline.unicamp.br/visoes/VisaoLogin.php"
    pgsession <- html_session(url)
    pgform    <- html_form(pgsession)[[1]]
    filled_form <- set_values(pgform,
                              "ra" = "171189", 
                              "senha" = "juju1997")
    teste <- submit_form(pgsession,filled_form,'OK')
    memberlist <- jump_to(pgsession, paste0("https://grade.daconline.unicamp.br/visoes/Perfil.php?ra=",alunosRA[i],"#tab_academico"))
    page <- read_html(memberlist)
    cursoAluno <- as.character(html_text(html_node(page,paste0('#tab_academico'))))
    cursoAluno <- strsplit(cursoAluno,"\t|\n")
    cursoAluno <- subset(cursoAluno[[1]], !cursoAluno[[1]]=="")
    curso[i] <- cursoAluno[which(cursoAluno=="Curso:")+1]
    print(i)
  }, error=function(e) {
    erroRA <<- append(erroRA,i)
    print(paste("Erro",i))
  })
}

# ----------------------------

library(ggplot2)
devtools::install_github("ropensci/plotly")
library(plotly)

ggplot(dadosCV_CR, aes(x=))
colnames(dadosCV_CR)[4:26] <- as.numeric(gsub("X","",colnames(dadosCV_CR)[4:26]))
dadosCR <- subset(dadosCV_CR, (as.numeric(rownames(dadosCV_CR)) %% 2)==0)
artesCR <- as.character(as.vector(dadosCV_CR[2,]))
artesCR <- as.numeric(artesCR[4:26])
dadosCRpeq <- dadosCR[,c(1,2,4:26)]

test<-melt(dadosCRpeq,id.vars = c("Curso", "Area"))

test$value[which(test$value<0)] <- 0.3525714
ggplot(test, aes(x=variable, y=value, group=as.factor(Curso))) +geom_line(mapping = aes(group = as.factor(Curso), color= as.factor(Curso))) + theme(legend.position = "none")

g <- ggplot(test, aes(x=variable, y=value, group=as.factor(Curso))) +geom_line(mapping = aes(group = as.factor(Curso), color= as.factor(Curso))) + facet_wrap(~Area) + theme(legend.position = "none") 
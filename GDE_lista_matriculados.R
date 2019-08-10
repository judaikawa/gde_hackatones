library(magrittr)

library(httr)
library(XML)
library(RSelenium)

# -----------------

library(rvest)

url <-"https://grade.daconline.unicamp.br/visoes/VisaoLogin.php"
pgsession <- html_session(url)
pgform    <- html_form(pgsession)[[1]]
filled_form <- set_values(pgform,
                          "ra" = "171189", 
                          "senha" = "juju1997")
teste <- submit_form(pgsession,filled_form,'OK')
memberlist <- jump_to(pgsession, paste0("https://grade.daconline.unicamp.br/visoes/Oferecimento.php?id=205784#lista_alunos"))
page <- read_html(memberlist)
obsDisciplina <- html_node(page,paste0('#lista_alunos'))

obsDisciplina <- strsplit(obsDisciplina,"\t")
vezesCursada <- append(vezesCursada, gsub(" desde 2007","",obsDisciplina[[1]][13]))
numReprovacoes <- append(numReprovacoes, strsplit(obsDisciplina[[1]][26], " ")[[1]][1])
porcReprovacoes <- append(porcReprovacoes,gsub("\\(|\\)","",strsplit(obsDisciplina[[1]][26], " ")[[1]][2]))
numDesistencias <- append(numDesistencias, obsDisciplina[[1]][39])

dadosDiscplinas <- data.frame(disciplinas,vezesCursada,numReprovacoes,porcReprovacoes,numDesistencias)
colnames(dadosDiscplinas) <- c("disciplinas", "vezesCursada (desde 2007)", "numReprovacoes", "porcReprovacoes","numDesistencias")


# Start the Server
rD <- rsDriver(browser = "phantomjs", verbose = FALSE)

# Assign the client
remDr <- rD$client

# Establish a wait for an element
remDr$setImplicitWaitTimeout(1000)

appurl <- "https://grade.daconline.unicamp.br/visoes/Oferecimento.php?id=205784#lista_alunos"
remDr$navigate(appurl)

# Give a crawl delay to see if it gives time to load web page
Sys.sleep(10)   # Been testing with 10

checkForServer()
startServer()
RSelenium::rsDriver()







###3 Pegando artistas do Billboard ###
# https://billboard.uol.com.br/rankings
# DIa 31/07/2018


# Pegando as informa��es da p�gina, n�o foi poss�vel atrav�s
# do pagote rvest, usaremos RCurl para pegar todas as informa��es
# e filtrar o necess�rio

pagina = getURL("https://billboard.uol.com.br/rankings")
linhasdecodigo = readLines(tc <- textConnection(pagina))

# Identificando que parte cont�m Hot 100 Brasil atrav�s o pattern linha brasil100

i = which(grepl(pattern = "linha brasil100", linhasdecodigo) == T) 
j = which(grepl(pattern = "linha billboard200", linhasdecodigo) == T) 
textototal = linhasdecodigo[i[1]:j[1]]

# Alguns dos artistas possuem a tag </div> na sua linha
# Identificando as linhas com essa tag para achar os nomes

i = which(grepl(pattern = "</div>", textototal) == T)
linhaartista = textototal[i]

# Limpando a informa��o anterior para pegar somente o nome dos artistas
nomartista = linhaartista %>%
  str_replace_all("                                                                                            </div>", "") %>%
  str_replace_all("                                                <br>","")
nomartista = data.frame(nomartista, stringsAsFactors = F)

# Verificado que alguns campos capturados ficaram sem informa��o
# Com isso foi identificado um outro padr�o para os artistas que
# no site acabaram de entrar nos 100 HOT
# Contamos quantos caracteres que tem cada elemento para retirar os 
# que n�o possuem informa��o

quanto = sapply(nomartista, nchar) 
nartista = nomartista[which(quanto != 0),] #selecionando somente os elementos com informa��o


# Verificando em quais linhas tem o novo padr�o dos artistas novos no hanking

novoarti = textototal[which(grepl(pattern = "<img style=\"position:absolute; right: 0px; top: 0px;\" src=\"https://billboard.uol.com.br/public/images/badge_new.png\">", textototal) == T)]
novoarti

# Limpando a informa��o anterior para pegar somente o nome dos artistas

novolimpo = novoarti %>%
  str_replace_all("                                                                                                 <img style=\"position:absolute; right: 0px; top: 0px;\" src=\"https://billboard.uol.com.br/public/images/badge_new.png\">", "") %>%
  str_replace_all("                                                <br>", "") %>%
  str_replace_all("                                                                                                <img style=\"position:absolute; right: 0px; top: 0px;\" src=\"https://billboard.uol.com.br/public/images/badge_new.png\">", "")
novolimpo

# Juntando todos os nomes encontrados, por�m pegando somente os nomes �nicos, sem repeti��es

bilartistas = c(nartista,novolimpo) %>%
  unique

## Fazendo uma nova lista com o formato igual no link
## Substituir espa�o por "_"
## Substituir & por "%26"
## juntando com o link

link = function(x){ 
  y = x%>%
    str_replace_all(" ", "_") %>%
    str_replace_all("&","%26")
  
  a = paste("https://pt.wikipedia.org/wiki/", y, sep = "")
  a}


lin = sapply(bilartistas, link)

# Verificando se o link existe ###

existe = sapply(lin, url.exists)

# Dataframe com as informa��es j� coletadas

bilartistas = data.frame(Nome = bilartistas, Link = lin, Existe = existe, stringsAsFactors = F)


# Tirando os artistas que n�o possui perfil no wiki

bilartistas2 = bilartistas[existe == TRUE, 1:2]

# Corrigindo o link do Cantor Ferrugem

i = which(bilartistas2 == "Ferrugem")
bilartistas2$Link[i] = paste(bilartistas2$Link[i], "_(cantor)", sep = "")


# retirando os cantores que n�o s�o brasileiros

bilartistas2= bilartistas2[-which(bilartistas2$Nome == "Shakira" |  
        bilartistas2$Nome == "Zeeba" |
        bilartistas2$Nome == "Dua Lipa"|
        bilartistas2$Nome == "Justin Timberlake"| 
        bilartistas2$Nome == "Camila Cabello"),]


row.names(bilartistas2) = 1:(nrow(bilartistas2))

# Salvando a tabela com os artistas

write.csv(bilartistas2, file = "Artistas.csv", col.names = T,
          fileEncoding = "UTF-8")


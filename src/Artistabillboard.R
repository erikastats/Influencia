###3 Pegando artistas do Billboard ###
# https://billboard.uol.com.br/rankings
# DIa 31/07/2018


# Pegando as informações da página, não foi possível através
# do pagote rvest, usaremos RCurl para pegar todas as informações
# e filtrar o necessário

pagina = getURL("https://billboard.uol.com.br/rankings")
linhasdecodigo = readLines(tc <- textConnection(pagina))

# Identificando que parte contém Hot 100 Brasil através o pattern linha brasil100

i = which(grepl(pattern = "linha brasil100", linhasdecodigo) == T) 
j = which(grepl(pattern = "linha billboard200", linhasdecodigo) == T) 
textototal = linhasdecodigo[i[1]:j[1]]

# Alguns dos artistas possuem a tag </div> na sua linha
# Identificando as linhas com essa tag para achar os nomes

i = which(grepl(pattern = "</div>", textototal) == T)
linhaartista = textototal[i]

# Limpando a informação anterior para pegar somente o nome dos artistas
nomartista = linhaartista %>%
  str_replace_all("                                                                                            </div>", "") %>%
  str_replace_all("                                                <br>","")
nomartista = data.frame(nomartista, stringsAsFactors = F)

# Verificado que alguns campos capturados ficaram sem informação
# Com isso foi identificado um outro padrão para os artistas que
# no site acabaram de entrar nos 100 HOT
# Contamos quantos caracteres que tem cada elemento para retirar os 
# que não possuem informação

quanto = sapply(nomartista, nchar) 
nartista = nomartista[which(quanto != 0),] #selecionando somente os elementos com informação


# Verificando em quais linhas tem o novo padrão dos artistas novos no hanking

novoarti = textototal[which(grepl(pattern = "<img style=\"position:absolute; right: 0px; top: 0px;\" src=\"https://billboard.uol.com.br/public/images/badge_new.png\">", textototal) == T)]
novoarti

# Limpando a informação anterior para pegar somente o nome dos artistas

novolimpo = novoarti %>%
  str_replace_all("                                                                                                 <img style=\"position:absolute; right: 0px; top: 0px;\" src=\"https://billboard.uol.com.br/public/images/badge_new.png\">", "") %>%
  str_replace_all("                                                <br>", "") %>%
  str_replace_all("                                                                                                <img style=\"position:absolute; right: 0px; top: 0px;\" src=\"https://billboard.uol.com.br/public/images/badge_new.png\">", "")
novolimpo

# Juntando todos os nomes encontrados, porém pegando somente os nomes únicos, sem repetições

bilartistas = c(nartista,novolimpo) %>%
  unique

## Fazendo uma nova lista com o formato igual no link
## Substituir espaço por "_"
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

# Dataframe com as informações já coletadas

bilartistas = data.frame(Nome = bilartistas, Link = lin, Existe = existe, stringsAsFactors = F)


# Tirando os artistas que não possui perfil no wiki

bilartistas2 = bilartistas[existe == TRUE, 1:2]

# Corrigindo o link do Cantor Ferrugem

i = which(bilartistas2 == "Ferrugem")
bilartistas2$Link[i] = paste(bilartistas2$Link[i], "_(cantor)", sep = "")


# retirando os cantores que não são brasileiros

bilartistas2= bilartistas2[-which(bilartistas2$Nome == "Shakira" |  
        bilartistas2$Nome == "Zeeba" |
        bilartistas2$Nome == "Dua Lipa"|
        bilartistas2$Nome == "Justin Timberlake"| 
        bilartistas2$Nome == "Camila Cabello"),]


row.names(bilartistas2) = 1:(nrow(bilartistas2))

# Salvando a tabela com os artistas

write.csv(bilartistas2, file = "Artistas.csv", col.names = T,
          fileEncoding = "UTF-8")


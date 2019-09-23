#' Extrai influencias de um artista com base em sua página da Wikipedia
#'
#' @param link Link para a página da Wikipedia (em pt-BR) de um artista
#'
#' @return DataFrame contendo nome do artista, o nome do artista/banda que o influencia (influenciado_por)
#'  e o link para a página deste artista/banda
extrair_influencias <- function(link, nome_artista){
  
  #TODO: Explico melhor esse xpath selector quando a gente se ligar
  #Seleciona o local exato onde estão listadas as influências de um artista
  xpath_selector <- "//td[contains(text(), 'Influência')]//ancestor::tr//a"
  
  #Note que vai ser preciso limpar essa lista depois, 
  #  alguns links apontam para citações da Wikipedia e não para páginas de artistas
  lista_influencias <- read_html(link, encoding = "UTF-8") %>% html_nodes(xpath=xpath_selector)
  
  if(length(lista_influencias) == 0){
    print(paste("P�gina do artista", nome_artista, "n�oo tem caixinha de influ�ncias"))
    data.frame("artista"=nome_artista, "influenciado_por"=NA, "link_artista_influencia"=NA)
  }else{
    nome_artistas_influenciadores <- lista_influencias %>% html_attr("title")
    link_artistas_influenciadores <- lista_influencias %>% html_attr("href")
    
    #Adiciona URL completa aos links
    link_artistas_influenciadores <- paste0("https://pt.wikipedia.org", link_artistas_influenciadores)
    
    data.frame("artista"=nome_artista, 
               "influenciado_por"=nome_artistas_influenciadores,
               "link_artista_influencia"=link_artistas_influenciadores)
  }
}

#' Compila as influências de todos os artistas no DataFrame
#'
#' @param artistasDF DataFrame de artistas, com colunas: Nome e Link
#'
#' @return DataFrame de todos os artistas influenciados
compila_todas_influencias <- function(artistasDF){
  
  #Construi um DataFrame com todos os links de artistas e suas influências
  lista_resultados <- mapply(extrair_influencias, artistasDF$Link, artistasDF$Nome, SIMPLIFY=FALSE, USE.NAMES=FALSE)
  #Transforma a lista em DataFrame
  df_resultado <- do.call(rbind, lista_resultados)
  
  #Remove links para notas de rodapé
  linhas_invalidas <- is.na(df_resultado$influenciado_por) & !is.na(df_resultado$link_artista_influencia)
  df_resultado <- df_resultado[!linhas_invalidas,]
  
  #Converte os Factors para Characters
  df_resultado$artista <- as.character(df_resultado$artista)
  df_resultado$influenciado_por <- as.character(df_resultado$influenciado_por)
  df_resultado$link_artista_influencia <- as.character(df_resultado$link_artista_influencia)
  
  df_resultado
}


## Tirando os artistas com informação "NA" cuja influência está no dataset adicional
#'  @param dadoscoletados DataFrame de artistas coletado a mão no texto do Wikipedia
#'  @param tabelaexistente DataFrame com os dados das influências coletado por código
#'  
#'  @return DataFrame único com os artistas e suas influências
#'  

influencia_de_fora <- function(dadoscoletados, tabelaexistente){
  nome = unique(dadoscoletados$artista)
  a = sapply(nome, function(x){
    which(tabelaexistente$artista == x)
  })%>% unlist()
  
  influencias2 = tabelaexistente[-a,]
  influencias3 = rbind(influencias2, dadoscoletados)
  influencias3
}


##' Cria um DataFrame com os artistas únicos (exlui os repetidos)
#'
#' @param influ DataFrame de artistas, com colunas: artista, influenciado_por e link_artista_influencia
#'
#' @return DataFrame de todos os artistas influenciados

artistas_influenciadores <- function(influ){
  artistas_unicos = data.frame(Nome=as.character(unique(influ$influenciado_por)),
                                       Link=as.character(unique(influ$link_artista_influencia)),
                                       stringsAsFactors = FALSE)

  
  # Remove casos inválidos e os artistas que já foram capturados (ou seja, já estão em artistas$Link)
  artistas_a_extrair <- !is.na(artistas_unicos$Nome) & !(artistas_unicos$Link %in% artistas$Link)
  artistas_unicos <- artistas_unicos[artistas_a_extrair,]
  artistas_unicos
}

##' Padronizar os nomes e links coletados
#'
#' @param link DataFrame de links da coluna link_artista_influencia
#' @param nome DataFrame de nomes da coluna influenciado_por
#'
#' @return DataFrame com links corrigidos
#' @return DataFrame com nomes corrigidos
#' 

corrigindo_link <- function(link){
  link %>%
    str_replace_all("_e_", "_%26_") %>%
    str_replace_all("_di_","_Di_") %>%
    str_replace_all("Carmem", "Carmen")
}

corrigindo_nomes <- function(nome){
  nome %>%
    str_replace_all(" e ", " & ") %>%
    str_replace_all(" di "," Di ") %>%
    str_replace_all("Carmem", "Carmen")
}


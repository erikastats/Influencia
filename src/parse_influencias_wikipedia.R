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
    print(paste("Página do artista", nome_artista, "não tem caixinha de influências"))
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
######################################################################
######################## MONITOREO REDES SOCIALES ####################
######################################################################

# 1. CAMBIAR LAS RUTAS COMUNES
# 2. CAMBIAR LA RUTA A FUNCIONES.R

######### LIBRERIAS ###########
library(xml2)
library(rvest)
library(stringr)
library(plyr); library(dplyr)
library(twitteR)
library(ROAuth)
library(Rfacebook)
library(RCurl)

source('D:/Data/R/SocialMining/Funciones.r')

######## RUTAS COMUNES #######

posFile = 'D:/Data/R/SocialMining/Parametros/positive-words.txt'
negFile = 'D:/Data/R/SocialMining/Parametros/negative-words.txt'
municipios = 'D:/Data/R/SocialMining/Parametros/Infraestructura.csv'

salidaWeb = 'D:/Data/R/SocialMining/WebCrawling/Salidas'
salidaTwitter = 'D:/Data/R/SocialMining/Twiter/salidas'
salidaFacebook = 'D:/Data/R/SocialMining/Facebook/Salidas'
salidaFinal = 'D:/Data/R/SocialMining/Salida'

######## PARAMETROS COMUNES #######
term = 'arbelaez%ecopetrol'
##term = 'Ecopetrol'

####fbIDpage <- 159616034235
#Ecopetrol: 117649051695
#https://www.facebook.com/NoticiasAcacias/ 713234648746028
#https://www.facebook.com/AlcaldiaAcaciasMeta 419761254869697

##fbIDpage <- 419761254869697
##ID pagina Ecopetrol: 117649051695
fbIDpage <- 117649051695


######################################################################
########################     NOTICIAS GOOGLE      ####################
######################################################################

setwd(salidaWeb)

strbusqueda1 <- "https://www.google.com/search?hl=es&gl=co&tbm=nws&q=" 
strbusqueda1 <- paste(strbusqueda1, term, sep="")
strbusqueda <- paste(strbusqueda1, "&tbs=qdr:d,sbd:1",sep="")

search_google <- read_html(strbusqueda)
links = (html_attr(html_nodes(search_google, "a"), "href"))
a.texts = html_text(html_nodes(search_google, "a"))

titles = html_text(search_google %>% html_nodes("h3") %>% html_node("a"))

df.links = as.data.frame(links)

indices = which(grepl("/url?", as.character(links)) == TRUE)

df.links = as.data.frame(substr(df.links[indices,], 8, 1000))

divs = html_text(html_nodes(search_google, "div"))
divs.texts = texts = gsub("\n||\t||\r", "", divs)
df.divs.texts = as.data.frame(divs.texts)

divs.class = html_attr(html_nodes(search_google, "div"), "class")
df.divs.class = as.data.frame(divs.class)

df.divs = cbind(df.divs.texts, df.divs.class)
names(df.divs) = c("Text", "Clase")

df.divs.indices = which(df.divs[,2] == "st")

df.divs.final = df.divs[df.divs.indices,]

fuente = html_text(search_google %>% html_nodes("div") %>% html_nodes("div") %>% html_nodes("div") %>% html_node("span"))
indices = which(nchar(fuente) > 0)
df.fuente = fuente[indices]

################################

fuente.time = NULL

for(i in 1:length(df.fuente)){
  registro = tolower(df.fuente[i])
  tiempo.fin = gregexpr(" ", registro)[[1]]
  tiempo.inicio = gregexpr("hace", registro)[[1]]
  
  date.index = gregexpr("[0-9]", substr(registro, tiempo.inicio, max(tiempo.fin) + 1))[[1]]
  
  date.marco = substr(registro, max(tiempo.fin) + 1, max(tiempo.fin) + 1)
  
  date.time = as.numeric(substring(substr(registro, tiempo.inicio, max(tiempo.fin)), date.index, date.index))
  
  date.time = as.numeric(substring(substr(registro, tiempo.inicio, max(tiempo.fin) + 1), min(date.index), max(date.index)))
  
  if(date.marco == "h"){
    time = as.character(format(Sys.time() - date.time[1]*3600, '%Y-%m-%d %H:%M'))
  } else
  {
    time = as.character(format(Sys.time() - date.time[1]*60, '%Y-%m-%d %H:%M'))
  }
  
  fuente.time = append(fuente.time, time)
  
}
################################

df.divs.final = cbind(df.divs.final, titles, df.fuente)

df.divs.final.1 = cbind(df.fuente, titles, as.character(df.divs.final[,1]), fuente.time)
df.divs.final.1 = as.data.frame(df.divs.final.1)
df.divs.final = df.divs.final.1
names(df.divs.final) = c("Fuente", "Titulo", "Resumen", "Fecha_Actualizacion")

rm(df.divs.final.1)

write.table(df.divs.final, file='WebCrawling_original.txt', row.names=F,  col.names = T, sep = "\t")

if (file.exists('WebCrawling_stack.txt')==FALSE)
  write.table(df.divs.final, file='WebCrawling_stack.txt', row.names=F,  col.names = T, sep = "\t")

#merge last access with cumulative file and remove duplicates
stack <- read.table(file='WebCrawling_stack.txt', sep = "\t", header = T, fill = T)
stack <- rbind(stack, df.divs.final)
stack <- subset(stack, !duplicated(stack$Titulo))
write.table(stack, file='WebCrawling_stack.txt', row.names=F,  col.names = T, sep = "\t")

################ EVALUACION ######################

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, ' ')
    words <- unlist(word.list)
    
    pos.words <- tolower(as.character(pos.words[,1]))
    neg.words <- tolower(as.character(neg.words[,1]))
    
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences, date=stack$Fecha_Actualizacion, user=stack$Fuente)
  return(scores.df)
}
pos <- read.table(posFile, encoding = 'UTF-8') #folder with positive dictionary
neg <- read.table(negFile, encoding = 'UTF-8') #folder with negative dictionary

Dataset <- stack
Dataset$Resumen <- as.factor(Dataset$Resumen)
scores <- score.sentiment(paste(Dataset$Titulo, " ", Dataset$Resumen), pos, neg, .progress='text')
write.table(scores, file='WebCrawling_scores.txt', row.names=F, col.names = T, sep = '\t') #save evaluation results into the file

geoloc = read.csv(file=municipios, header = TRUE, sep = ";", dec = ",")
geoloc = geoloc[, 3:6]
geoloc$MUNICIPIO =iconv(tolower(geoloc$MUNICIPIO), to="ASCII//TRANSLIT")

texts = read.table(file='WebCrawling_stack.txt', sep = "\t", header = T, fill = T)
dates = texts$Fecha_Actualizacion
texts = texts$Resumen

texts = str_replace_all(texts,"[^[:graph:]]", " ")
texts = str_replace_all(texts,"[[:punct:]]", " ") 
texts = str_replace_all(texts,"[[:cntrl:]]", " ") 
texts = tolower(texts)
word.list = str_split(texts, ' ')
words = unlist(word.list)

keywords = NULL

for (i in 1:length(words)){
  
  if (nchar(words[i]) >= 4){
    
    keywords = append(keywords, words[i])
    
  }
  
}

unicos = unique(keywords)
unicos.formato = iconv(unicos, to="ASCII//TRANSLIT")
frecuencia.list = NULL

for (i in 1:length(unicos)){
  
  frecuencia = length(which(unicos[i] == keywords))
  frecuencia.list = append(frecuencia.list, frecuencia)
  
}

geo.yes = NULL

for (i in 1:length(unicos)){
  
  geo.len = length(which(unicos[i] == geoloc$MUNICIPIO))
  
  if (geo.len > 0){
    
    geo.bool = 1
    x = geoloc$COORDENADA.X[which(geoloc$MUNICIPIO == unicos[i])[1]]
    y = geoloc$COORDENADA.Y[which(geoloc$MUNICIPIO == unicos[i])[1]]
    dane = geoloc$CODIGO.MUNICIPIO[which(geoloc$MUNICIPIO == unicos[i])[1]]
    final.bool = cbind(geo.bool, x, y, dane)
    geo.yes = rbind(geo.yes, final.bool)
  }else{
    
    geo.bool = 0
    x = 0
    y = 0
    dane = 0
    final.bool = cbind(geo.bool, x, y, dane)
    geo.yes = rbind(geo.yes, final.bool)
    
  } 
  
}

frecuencias = cbind(unicos,frecuencia.list, geo.yes, format(Sys.time(), '%Y-%m-%d'))
frecuencias = as.data.frame(frecuencias)
names(frecuencias) = c('Palabra', 'Frecuencia', 'geo.bool', 'x', 'y', 'DANE', 'Actualizacion')

maximos = which(as.numeric(as.character(frecuencias$Frecuencia)) > 3)

tendencias = frecuencias[maximos,]

##############################################

words.date = NULL

until.unicos = length(tendencias[,1])
until.texts = length(texts)

for (i in 1:until.unicos){
  palabra = as.character(tendencias[i,1])
  for (j in 1:length(texts)){
    count = gregexpr(palabra, texts[j])[[1]]
    if(count[1] != -1){
      contador = length(count)
      words.date = rbind(words.date, cbind(id=j, palabra=palabra, count=contador, date=as.character(dates[j])))
    } 
  }
}

write.table(words.date, file=paste('WebCrawling_words.txt', sep = ''), row.names=F,  col.names = T, sep = "\t")

##############################################


write.table(tendencias[,1:2], file='WebCrawling_tendencia.txt', row.names=F,  col.names = T, sep = "\t")

frecuenciastemp = frecuencias[which(as.numeric(as.character(frecuencias$geo.bool)) == 1),]

write.table(frecuenciastemp, file='WebCrawling_Ecopetrol_stack_geo.txt', row.names=F,  col.names = T, sep = "\t")

scores = read.table(file = 'WebCrawling_scores.txt', sep = "\t", header = T, fill = T)

columnas.nuevas = c('Codigo', 'Municipio', 'X', 'Y', 'Calificacion')
scores[,columnas.nuevas] = NA

for (i in 1:length(texts)){
  
  word.list.i = str_split(texts[i], ' ')
  words.i = unlist(word.list.i)
  
  for (j in 1:length(words.i)){
    
    index.i = which(words.i[j] == frecuenciastemp$Palabra)
    
    if (length(index.i) > 0){
      
      scores$Municipio[i] =  words.i[j]
      scores$X[i] = geoloc$COORDENADA.X[which(geoloc$MUNICIPIO == words.i[j])[1]]
      scores$Y[i] = geoloc$COORDENADA.Y[which(geoloc$MUNICIPIO == words.i[j])[1]]
      scores$Codigo[i] = geoloc$CODIGO.MUNICIPIO[which(geoloc$MUNICIPIO == words.i[j])[1]]
    }
  }
  
  score = scores$score[i]
  if (score > 0){
    
    scores$Calificacion[i] = 'Positivo'
    
  } else if (score < 0){
    
    scores$Calificacion[i] = 'Negativo'
    
  } else {
    
    scores$Calificacion[i] = 'Neutral'
    
  }
  
}

write.table(scores, file = 'WebCrawling_scores_geo.txt', row.names=F,  col.names = T, sep = "\t")

######################################################################
########################    FIN NOTICIAS GOOGLE   ####################
######################################################################

######################################################################
############################     TWITER      #########################
######################################################################

setwd(salidaTwitter)

#connect to API

consumerKey <- 'QN87DWoqgIGk9OvtjcfD8ntCm'
consumerSecret <- 'CJhZuJn5tJ9LFRFy9qjksYzazj27nnRZxuE6yb66IZSrNjuJSl'
accessToken = '202032159-XzsHSnLW5efjHlAiWu7ZWp7GsBoh3D0voPgAnvdQ'
accessSecret = 'jLqInr5UkrQPpSL8Y0ltGWjhP5mfIwG6mwn8ART85ilko'

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)

searchTermTwitter(term, posFile = posFile, negFile = negFile)

geoloc = read.csv(file=municipios, header = TRUE, sep = ";", dec = ",")
geoloc = geoloc[, 3:6]
geoloc$MUNICIPIO =iconv(tolower(geoloc$MUNICIPIO), to="ASCII//TRANSLIT")

texts = read.table(file=paste(term,'_stack.txt', sep = ''), sep = "\t", header = T, fill = T)
dates = texts$created
texts = texts$text

texts = str_replace_all(texts,"[^[:graph:]]", " ")
texts = str_replace_all(texts,"[[:punct:]]", " ") 
texts = str_replace_all(texts,"[[:cntrl:]]", " ") 
texts = tolower(texts)
word.list = str_split(texts, ' ')
words = unlist(word.list)

keywords = NULL

for (i in 1:length(words)){
  
  if (nchar(words[i]) >= 4){
    
    keywords = append(keywords, words[i])
    
  }
  
}

unicos = unique(keywords)
unicos.formato = iconv(unicos, to="ASCII//TRANSLIT")
frecuencia.list = NULL

for (i in 1:length(unicos)){
  
  frecuencia = length(which(unicos[i] == keywords))
  frecuencia.list = append(frecuencia.list, frecuencia)
  
}

geo.yes = NULL

for (i in 1:length(unicos)){
  
  geo.len = length(which(unicos[i] == geoloc$MUNICIPIO))
  
  if (geo.len > 0){
    
    geo.bool = 1
    x = geoloc$COORDENADA.X[which(geoloc$MUNICIPIO == unicos[i])[1]]
    y = geoloc$COORDENADA.Y[which(geoloc$MUNICIPIO == unicos[i])[1]]
    dane = geoloc$CODIGO.MUNICIPIO[which(geoloc$MUNICIPIO == unicos[i])[1]]
    final.bool = cbind(geo.bool, x, y, dane)
    geo.yes = rbind(geo.yes, final.bool)
  }else{
    
    geo.bool = 0
    x = 0
    y = 0
    dane = 0
    final.bool = cbind(geo.bool, x, y, dane)
    geo.yes = rbind(geo.yes, final.bool)
    
  } 
  
}

frecuencias = cbind(unicos,frecuencia.list, geo.yes, format(Sys.time(), '%Y-%m-%d %H:%M'))
frecuencias = as.data.frame(frecuencias)
names(frecuencias) = c('Palabra', 'Frecuencia', 'geo.bool', 'x', 'y', 'DANE', 'Actualizacion')

maximos = which(as.numeric(as.character(frecuencias$Frecuencia)) > 50)

tendencias = frecuencias[maximos,]

##############################################

words.date = NULL

until.unicos = length(tendencias[,1])
until.texts = length(texts)

for (i in 1:until.unicos){
  palabra = as.character(tendencias[i,1])
  for (j in 1:length(texts)){
    count = gregexpr(palabra, texts[j])[[1]]
    if(count[1] != -1){
      contador = length(count)
      words.date = rbind(words.date, cbind(id=j, palabra=palabra, count=contador, date=as.character(dates[j])))
    } 
  }
}

write.table(words.date, file=paste('tw_',term,'_words.txt', sep = ''), row.names=F,  col.names = T, sep = "\t")

##############################################


write.table(tendencias[,1:2], file=paste('tw_',term,'_tendencia.txt', sep = ''), row.names=F,  col.names = T, sep = "\t")

frecuenciastemp = frecuencias[which(as.numeric(as.character(frecuencias$geo.bool)) == 1),]

write.table(frecuenciastemp, file=paste('tw_',term,'_Ecopetrol_stack_geo.txt', sep = ''), row.names=F,  col.names = T, sep = "\t")

scores = read.table(file=paste(term,'_scores.txt', sep = ''), sep = "\t", header = T, fill = T)

columnas.nuevas = c('Codigo', 'Municipio', 'X', 'Y', 'Calificacion')
scores[,columnas.nuevas] = NA

for (i in 1:length(texts)){
  
  word.list.i = str_split(texts[i], ' ')
  words.i = unlist(word.list.i)
  
  for (j in 1:length(words.i)){
    
    index.i = which(words.i[j] == frecuenciastemp$Palabra)
    
    if (length(index.i) > 0){
      
      scores$Municipio[i] =  words.i[j]
      scores$X[i] = geoloc$COORDENADA.X[which(geoloc$MUNICIPIO == words.i[j])[1]]
      scores$Y[i] = geoloc$COORDENADA.Y[which(geoloc$MUNICIPIO == words.i[j])[1]]
      scores$Codigo[i] = geoloc$CODIGO.MUNICIPIO[which(geoloc$MUNICIPIO == words.i[j])[1]]
    }
  }
  
  score = scores$score[i]
  if (score > 0){
    
    scores$Calificacion[i] = 'Positivo'
    
  } else if (score < 0){
    
    scores$Calificacion[i] = 'Negativo'
    
  } else {
    
    scores$Calificacion[i] = 'Neutral'
    
  }
  
}

write.table(scores, file=paste('tw_',term, '_scores_geo.txt', sep = ''), row.names=F,  col.names = T, sep = "\t")

######################################################################
############################    FIN TWITTER   ########################
######################################################################

######################################################################
###########################     FACEBOOK      ########################
######################################################################

#httr::set_config( config( ssl_verifypeer = 0L ) )
#set_config(use_proxy(url='titan.ecopetrol.com.co',8080))

setwd(salidaFacebook)

##app_id = "839951899439143"
app_id = "1974037539493552"

##app_secret = "1ebfa719a5eb0b8ae8ea9ec37d5fcead"
app_secret = "d57ba852a41d5a06df669a4a67f21722"

##fb_oauth <- fbOAuth(app_id, app_secret, extended_permissions = TRUE)

##save(fb_oauth, file="fb_oauth")

load("fb_oauth")

#pages2 <- searchPages( string='Ecopetrol', token=fb_oauth)
pages <- searchPages( string=term, token=fb_oauth, 400)


###Se lee la pagina de Ecopetrol sin comentarios
###ID pagina Ecopetrol: 117649051695

searchInfacebook(fbIDpage, posFile, negFile)

geoloc = read.csv(file=municipios, header = TRUE, sep = ";", dec = ",")
geoloc = geoloc[, 3:6]
geoloc$MUNICIPIO =iconv(tolower(geoloc$MUNICIPIO), to="ASCII//TRANSLIT")

texts = read.table(file='Ecopetrol_post_comments.txt', sep = "\t", header = T, fill = F)
dates = texts$created_time
texts = texts$message

texts = str_replace_all(texts,"[^[:graph:]]", " ")
texts = str_replace_all(texts,"[[:punct:]]", " ") 
texts = str_replace_all(texts,"[[:cntrl:]]", " ")
texts = str_replace_all(texts,"\r", " ")
texts = tolower(texts)
word.list = str_split(texts, ' ')
words = unlist(word.list)

keywords = NULL

for (i in 1:length(words)){
  
  if (nchar(words[i]) >= 4){
    
    keywords = append(keywords, words[i])
    
  }
  
}

unicos = unique(keywords)
unicos.formato = iconv(unicos, to="ASCII//TRANSLIT")
frecuencia.list = NULL

for (i in 1:length(unicos)){
  
  frecuencia = length(which(unicos[i] == keywords))
  frecuencia.list = append(frecuencia.list, frecuencia)
  
}

geo.yes = NULL

for (i in 1:length(unicos)){
  
  geo.len = length(which(unicos[i] == geoloc$MUNICIPIO))
  
  if (geo.len > 0){
    
    geo.bool = 1
    x = geoloc$COORDENADA.X[which(geoloc$MUNICIPIO == unicos[i])[1]]
    y = geoloc$COORDENADA.Y[which(geoloc$MUNICIPIO == unicos[i])[1]]
    dane = geoloc$CODIGO.MUNICIPIO[which(geoloc$MUNICIPIO == unicos[i])[1]]
    final.bool = cbind(geo.bool, x, y, dane)
    geo.yes = rbind(geo.yes, final.bool)
  }else{
    
    geo.bool = 0
    x = 0
    y = 0
    dane = 0
    final.bool = cbind(geo.bool, x, y, dane)
    geo.yes = rbind(geo.yes, final.bool)
    
  } 
  
}

frecuencias = cbind(unicos,frecuencia.list, geo.yes, format(Sys.time(), '%Y-%m-%d %H'))
frecuencias = as.data.frame(frecuencias)
names(frecuencias) = c('Palabra', 'Frecuencia', 'geo.bool', 'x', 'y', 'DANE', 'Actualizacion')

maximos = which(as.numeric(as.character(frecuencias$Frecuencia)) > 1)

tendencias = frecuencias[maximos,]

##############################################

words.date = NULL

until.unicos = length(tendencias[,1])
until.texts = length(texts)

for (i in 1:until.unicos){
  palabra = as.character(tendencias[i,1])
  for (j in 1:length(texts)){
    count = gregexpr(palabra, texts[j])[[1]]
    if(count[1] != -1){
      contador = length(count)
      words.date = rbind(words.date, cbind(id=j, palabra=palabra, count=contador, date=as.character(dates[j])))
    } 
  }
}

write.table(words.date, file=paste('fb_words.txt', sep = ''), row.names=F,  col.names = T, sep = "\t")

##############################################


write.table(tendencias[,1:2], file='fb_tendencia.txt', row.names=F,  col.names = T, sep = "\t")

frecuenciastemp = frecuencias[which(as.numeric(as.character(frecuencias$geo.bool)) == 1),]

write.table(frecuenciastemp, file='Ecopetrol_stack_geo.txt', row.names=F,  col.names = T, sep = "\t")

scores = read.table(file='Ecopetrol_score.txt', sep = "\t", header = T)

columnas.nuevas = c('Codigo', 'Municipio', 'X', 'Y', 'Calificacion')
scores[,columnas.nuevas] = NA

for (i in 1:length(texts)){
  
  word.list.i = str_split(texts[i], ' ')
  words.i = unlist(word.list.i)
  
  for (j in 1:length(words.i)){
    
    index.i = which(words.i[j] == frecuenciastemp$Palabra)
    
    if (length(index.i) > 0){
      
      scores$Municipio[i] =  words.i[j]
      scores$X[i] = geoloc$COORDENADA.X[which(geoloc$MUNICIPIO == words.i[j])[1]]
      scores$Y[i] = geoloc$COORDENADA.Y[which(geoloc$MUNICIPIO == words.i[j])[1]]
      scores$Codigo[i] = geoloc$CODIGO.MUNICIPIO[which(geoloc$MUNICIPIO == words.i[j])[1]]
    }
  }
  
  score = scores$score[i]
  if (score > 0){
    
    scores$Calificacion[i] = 'Positivo'
    
  } else if (score < 0){
    
    scores$Calificacion[i] = 'Negativo'
    
  } else {
    
    scores$Calificacion[i] = 'Neutral'
    
  }
  
}

write.table(scores, file='fb_Ecopetrol_scores_geo.txt', row.names=F,  col.names = T, sep = "\t")

######################################################################
###########################    FIN FACEBOOK   ########################
######################################################################

######################################################################
####################   GENERACION ARCHIVOS FINALES   #################
######################################################################

########### CAMBIAR RUTA ##############
setwd(salidaTwitter)

scores = read.table(file=paste('tw_',term, '_scores_geo.txt', sep = ''), sep = "\t", header = T, fill = T)
tendencia = read.table(file=paste('tw_',term, '_tendencia.txt', sep = ''), sep = "\t", header = T, fill = T)
palabras = read.table(file=paste('tw_',term,'_words.txt', sep = ''), sep = "\t", header = T, fill = T)

scores[,'Fuente'] = NA
scores$Fuente = 'Twitter'

tendencia[,'Fuente'] = NA
tendencia$Fuente = 'Twitter'

palabras[,'Fuente'] = NA
palabras$Fuente = 'Twitter'


setwd(salidaFacebook)

scoresFb = read.table(file='fb_Ecopetrol_scores_geo.txt', sep = "\t", header = T, fill = T)
tendenciaFb = read.table(file='fb_tendencia.txt', sep = "\t", header = T, fill = T)
palabrasFb = read.table(file='fb_words.txt', sep = "\t", header = T, fill = T)

scoresFb[,'Fuente'] = NA
scoresFb$Fuente = 'Facebook'

tendenciaFb[,'Fuente'] = NA
tendenciaFb$Fuente = 'Facebook'

palabrasFb[,'Fuente'] = NA
palabrasFb$Fuente = 'Facebook'


setwd(salidaWeb)

scoresWC = read.table(file='WebCrawling_scores_geo.txt', sep = "\t", header = T, fill = T)
tendenciaWC = read.table(file='WebCrawling_tendencia.txt', sep = "\t", header = T, fill = T)
palabrasWC = read.table(file='WebCrawling_words.txt', sep = "\t", header = T, fill = T)

scoresWC[,'Fuente'] = NA
scoresWC$Fuente = 'WebCrawling'

tendenciaWC[,'Fuente'] = NA
tendenciaWC$Fuente = 'WebCrawling'

palabrasWC[,'Fuente'] = NA
palabrasWC$Fuente = 'WebCrawling'

scoresTotal = rbind(scores, scoresFb, scoresWC)
tendenciaTotal = rbind(tendencia, tendenciaFb, tendenciaWC)
palabrasTotal = rbind(palabras, palabrasFb, palabrasWC)

scoresTotal[,'Total'] = NA
scoresTotal$Total = 1

tendenciaTotal[,'Total'] = NA
tendenciaTotal$Total = 1

palabrasTotal[,'Total'] = NA
palabrasTotal$Total = 1

setwd(salidaFinal)
write.table(scoresTotal, file='scores_geo_total.txt', row.names=F,  col.names = T, sep = "\t")
write.table(tendenciaTotal, file='tendencia_total.txt', row.names=F,  col.names = T, sep = "\t")
write.table(palabrasTotal, file='palabras_total.txt', row.names=F,  col.names = T, sep = "\t")


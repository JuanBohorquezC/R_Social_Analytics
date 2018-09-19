#########################################
############## TWITTER #################
#########################################

searchTermTwitter <- function(searchterm, posFile, negFile)
{
  since = as.Date(Sys.time()) - 7
  since = as.character(format(since, '%Y-%m-%d'))
  until = as.Date(Sys.time())
  until = as.character(format(until, '%Y-%m-%d'))
  
  #access tweets and create cumulative file
  list <- searchTwitter(searchterm, n = 1000, lang = 'es', retryOnRateLimit = 100, since=since, until=until)
  df <- twListToDF(list)
  df <- df[, order(names(df))]
  df$created <- strftime(df$created, '%Y-%m-%d %H:%M')
  
  if (file.exists(paste(searchterm, '_stack_original.txt', sep = ''))==FALSE)
    write.table(df, file=paste(searchterm, '_stack_original.txt', sep = ''), row.names=F, col.names = T, sep = "\t")
  
  for(i in 1:length(df$text)){
    
    df[i,15] = gsub("\n", " ", df[i,15])
    df[i,15] = str_replace_all(df[i,15],"[^[:graph:]]", " ")
    df[i,15] = gsub('\"', " ", df[i,15])
    df[i,15] = gsub('https', " ", df[i,15])
    df[i,15] = gsub('bxogedf1jr', " ", df[i,15])
    df[i,15] = gsub('xfgsvge1dz', " ", df[i,15])
    
  }
  
  if (file.exists(paste(searchterm, '_stack.txt', sep = ''))==FALSE)
    write.table(df, file=paste(searchterm, '_stack.txt', sep = ''), row.names=F, col.names = T, sep = "\t")
  
  #merge last access with cumulative file and remove duplicates
  stack <- read.table(file=paste(searchterm, '_stack.txt', sep = ''), sep = "\t", header = T, fill = T)
  stack <- rbind(stack, df)
  stack <- subset(stack, !duplicated(stack$text))
  write.table(stack, file=paste(searchterm, '_stack.txt', sep = ''), row.names=F,  col.names = T, sep = "\t")
  
  #evaluation tweets function
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
    scores.df <- data.frame(score=scores, text=sentences, date=stack$created, user=stack$screenName)
    return(scores.df)
  }
  pos <- read.table(posFile, encoding = 'UTF-8') #folder with positive dictionary
  neg <- read.table(negFile, encoding = 'UTF-8') #folder with negative dictionary
  Dataset <- stack
  Dataset$text <- as.factor(Dataset$text)
  scores <- score.sentiment(Dataset$text, pos, neg, .progress='text')
  write.table(scores, file=paste(searchterm, '_scores.txt', sep = ''), row.names=F, col.names = T, sep = '\t') #save evaluation results into the file

}

#########################################
############## FACEBOOK #################
#########################################

searchInfacebook <- function(pageId, posFile, negFile)
{
  
  since = as.Date(Sys.time()) - 7
  since = as.character(format(since, '%Y/%m/%d'))
  until = as.Date(Sys.time())
  until = as.character(format(until, '%Y/%m/%d'))
  
  fb_page <- getPage(page=pageId, token=fb_oauth, n = 1000, since=since, until=until)
  fb_page$created_time <- strftime(fb_page$created_time, '%Y-%m-%d %H:%M')
  
  if (file.exists(file='Ecopetrol_page_original.txt')==FALSE)
    write.table(fb_page, file='Ecopetrol_page_original.txt', row.names=F, col.names = T, sep = "\t")
  
  for(i in 1:length(fb_page$message)){
    
    fb_page$message = gsub("\n", " ", fb_page$message)
    fb_page$message = str_replace_all(fb_page$message,"[^[:graph:]]", " ") 
    
  }
  
  write.table(fb_page, file='Ecopetrol_page.txt', row.names=F,  col.names = T, sep = "\t")
  
  ###Se lee la pagina de Ecopetrol con comentarios

  post <- getPage(page=pageId, token = fb_oauth, n = 1000, feed = TRUE, reactions = TRUE, since=since, until=until)
  post$created_time <- strftime(post$created_time, '%Y-%m-%d')
  
  if (file.exists('Ecopetrol_post_original.txt')==FALSE)
    write.table(post, file='Ecopetrol_post_original.txt', row.names=F, col.names = T, sep = "\t")
  
  for(i in 1:length(post$message)){
    
    post$message = gsub("\n", " ", post$message)
    post$message = str_replace_all(post$message,"[^[:graph:]]", " ") 
    
  }
  
  write.table(post, file='Ecopetrol_post.txt', row.names=F,  col.names = T, sep = "\t")
  
  df.post.comments = NULL
  df.post.post = NULL
  df.post.likes = NULL
  
  for (i in 1:length(post$message)){
    
    if(is.na(post$id[i]) == FALSE){
      comments = getPost(post=post$id[i], token = fb_oauth, comments = TRUE, likes = TRUE, n.comments = 1000, n.likes = 1000)
      
      if(length(which(names(comments) == "comments")) > 0){
        if(dim(comments$comments)[1] > 0){
          df.post.comments = rbind(df.post.comments, comments$comments)
        }
      }
      if(length(which(names(comments) == "post")) > 0){
        if(dim(comments$post)[1] > 0){
          df.post.post = rbind(df.post.post, comments$post)
        }
      }
      if(length(which(names(comments) == "likes")) > 0){
        if(dim(comments$likes)[1] > 0){
          df.post.likes = rbind(df.post.likes, comments$likes)
        }
      }
    }
  }
  
  
  
  write.table(df.post.comments, file='Ecopetrol_post_comments_original.txt', row.names=F,  col.names = T, sep = "\t")
  write.table(df.post.post, file='Ecopetrol_post_post_original.txt', row.names=F,  col.names = T, sep = "\t")
  write.table(df.post.likes, file='Ecopetrol_post_likes_original.txt', row.names=F,  col.names = T, sep = "\t")
  
  df.post.comments$created_time <- strftime(df.post.comments$created_time, '%Y-%m-%d')
  for(i in 1:length(df.post.comments$message)){
    
    df.post.comments$message = gsub("\n", " ", df.post.comments$message)
    df.post.comments$message = str_replace_all(df.post.comments$message,"[^[:graph:]]", " ") 
    
  }
  
  df.post.post$created_time <- strftime(df.post.post$created_time, '%Y-%m-%d')
  for(i in 1:length(df.post.post$message)){
    
    df.post.post$message = gsub("\n", " ", df.post.post$message)
    df.post.post$message = str_replace_all(df.post.post$message,"[^[:graph:]]", " ") 
    
  }
  
  write.table(df.post.comments, file='Ecopetrol_post_comments.txt', row.names=F,  col.names = T, sep = "\t")
  write.table(df.post.post, file='Ecopetrol_post_post.txt', row.names=F,  col.names = T, sep = "\t")
  
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
    scores.df <- data.frame(score=scores, text=sentences, date=df.post.comments$created_time, user=df.post.comments$from_name)
    return(scores.df)
  }
  
  pos <- read.table(posFile, encoding = 'UTF-8') #folder with positive dictionary
  neg <- read.table(negFile, encoding = 'UTF-8') #folder with negative dictionaryDataset <- df.post.comments
  Dataset <- df.post.comments
  Dataset$text <- as.factor(df.post.comments$message)
  scores <- score.sentiment(Dataset$text, pos, neg, .progress='text')
  write.table(scores, file='Ecopetrol_score.txt', row.names=F, col.names = T, sep = '\t') #save evaluation results into the file
  
}
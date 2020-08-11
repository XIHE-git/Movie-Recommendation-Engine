###Load library
library(shiny)
library(shinyjs)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plyr)
library(data.table)

###Data and environment preparation
movie_all = read.table("./data/movie_all.rdata")
header1 = read.table("./data/movie_cor.rdata", header = TRUE, nrow = 1)
movie_cor = fread("./data/movie_cor.rdata", skip=1, header=FALSE)[,-1]
setnames(movie_cor, colnames(header1))
colnames(movie_cor) = gsub("X(\\d+)","\\1",colnames(movie_cor)) 
rownames(movie_cor) = colnames(movie_cor)
header2 = read.table("./data/movie_cor2.rdata", header = TRUE, nrow = 1)
movie_cor2 = fread("./data/movie_cor2.rdata", skip=1, header=FALSE)[,-1]
setnames(movie_cor2, colnames(header2))
colnames(movie_cor2) = gsub("X(\\d+)","\\1",colnames(movie_cor2)) 
rownames(movie_cor2) = colnames(movie_cor2)
people_all = fread("./data/people_all.csv")
outlier_list = read.table("./data/outlier_list.rdata")
settle_cluster = read_rds("./data/settle_cluster.rdata")
movie_cluster = read.table("./data/movie_cluster.rdata")
docu =data.frame(score=c(1), id = c(1)) ###Initialization the user-docu-vector!!!
write.table(docu,"./data/docu.rdata") ###Initialization the user-docu-vector!!!

create_movies = function(){
  docu2 <- read.table("./data/docu.rdata")
  docu3 = docu2[-1,]
  
  scored = docu3$score
  scored_id = docu3$id
  
  movie_recon <- function(movie_cor){
    movie_cor = as.matrix(movie_cor)
    rownames(movie_cor) = colnames(movie_cor)
    n = length(colnames(movie_cor))
    names_in = as.numeric(colnames(movie_cor)[colnames(movie_cor) %in% scored_id])
    score_frame = data.frame(id = names_in) %>% left_join(docu3, by = "id")
    score_vector = numeric(n)
    score_vector[which(colnames(movie_cor) %in% scored_id)] = score_frame$score
    Movie_1 = (movie_cor - diag(rep(1,n))) %*% t(t(score_vector))
    Location = as.numeric(colnames(movie_cor) %in% scored_id)
    Movie_2 = (movie_cor - diag(rep(1,n))) %*% t(t(Location))
    Movie = Movie_1/Movie_2
    
    Movie_pool = Movie[!(rownames(Movie) %in% scored_id),]
    recon_id = as.numeric(names(Movie_pool))[tail(order(Movie_pool), n=5)]
    return(recon_id)
  }
  
  return(c(movie_recon(movie_cor), movie_recon(movie_cor2)))
}

function(input, output, session) {
  v = reactiveValues(id=NULL, user_rate = NULL, id_list=NULL,
                     movie_might_id1=NULL, movie_might_score1=NULL,
                     movie_might_id2=NULL, movie_might_score2=NULL,
                     movie_might_id3=NULL, movie_might_score3=NULL,
                     movie_might_id4=NULL, movie_might_score4=NULL,
                     movie_might_id5=NULL, movie_might_score5=NULL,
                     user_might_id1=NULL,  user_might_score1=NULL,
                     user_might_id2=NULL,  user_might_score2=NULL,
                     user_might_id3=NULL,  user_might_score3=NULL,
                     user_might_id4=NULL,  user_might_score4=NULL,
                     user_might_id5=NULL,  user_might_score5=NULL,
                     cluster_id1=NULL, cluster_score1=NULL,
                     cluster_id2=NULL, cluster_score2=NULL,
                     cluster_id3=NULL, cluster_score3=NULL,
                     cluster_id4=NULL, cluster_score4=NULL,
                     cluster_id5=NULL, cluster_score5=NULL,
                     cluster_id6=NULL, cluster_score6=NULL,
                     cluster_id7=NULL, cluster_score7=NULL,
                     cluster_id8=NULL, cluster_score8=NULL,
                     cluster_id9=NULL, cluster_score9=NULL,
                     cluster_id10=NULL, cluster_score10=NULL,
                     cluster_id11=NULL, cluster_score11=NULL,
                     cluster_id12=NULL, cluster_score12=NULL,
                     cluster_id13=NULL, cluster_score13=NULL,
                     cluster_might_id1=NULL, cluster_might_score1=NULL,
                     cluster_might_id2=NULL, cluster_might_score2=NULL,
                     cluster_might_id3=NULL, cluster_might_score3=NULL,
                     cluster_might_id4=NULL, cluster_might_score4=NULL,
                     cluster_might_id5=NULL, cluster_might_score5=NULL,
                     cluster_might_id6=NULL, cluster_might_score6=NULL)
  observeEvent(input$refresh, {
    v$user_rate = NULL
    v$check = NULL
    docu2 = read.table("./data/docu.rdata")
    docu2 = docu2[-1,]
    if (length(docu2[,1]) <=10 ){
      v$id = sample(c(settle_cluster[[1]],settle_cluster[[2]],settle_cluster[[3]]),1)
      output$sys_condition <- renderUI({tags$h3('Sys:Freezing!')})
    }else if((length(docu2[,1])-10)%%6 == 1){
      v$id_list = create_movies()
      v$id = sample(v$id_list,1)
      v$id_list = v$id_list[v$id_list!=v$id]
      output$sys_condition <- renderUI({tags$h3('Sys:Start!')})
    }else{
      if(length(v$id_list)>1){
        v$id = sample(v$id_list,1)
        v$id_list = v$id_list[v$id_list!=v$id]
      }else{
        v$id = v$id_list[1]
        v$id_list = NULL
      }
      output$sys_condition <- renderUI({tags$h3('Sys:Recommending...')})
    }
    poster = movie_all[movie_all$id==v$id, 'poster']
    output$movie_info_main <- renderUI({
      type = movie_all[movie_all$id==v$id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==v$id, 'score']
      pop = movie_all[movie_all$id==v$id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==v$id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==v$id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==v$id, 'English_name']),
        h4(movie_all[movie_all$id==v$id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
    
    ###Check_block
      dist = select(movie_all[movie_all$id==v$id,], starts_with("rating"))
      dist = rev(as.numeric(sub("%", "",dist))/100)
      check = eventReactive(input$save_inputs, {dist[as.numeric(gsub('(\\d)star','\\1',input$user_score))]})
      output$outlier <- renderUI(
        if(check()<=0.1){
          tags$h3('Oops...')
        }
      )
      
    #Comment_Block
      comment = people_all[people_all$movie_id == v$id,]
      comment = comment[!is.na(comment$movie_score),]
      comment$movie_comment = as.character(comment$movie_comment)
      comment = comment[comment$movie_comment!="",]
      output$movie_inspect5<-renderTable({
        comment[comment$movie_score==5,c('movie_comment','movie_time','user_id')]
      })
      output$movie_inspect4<-renderTable({
        comment[comment$movie_score==4,c('movie_comment','movie_time','user_id')]
      })
      output$movie_inspect3<-renderTable({
        comment[comment$movie_score==3,c('movie_comment','movie_time','user_id')]
      })
      output$movie_inspect2<-renderTable({
        comment[comment$movie_score==2,c('movie_comment','movie_time','user_id')]
      })
      output$movie_inspect1<-renderTable({
        comment[comment$movie_score==1,c('movie_comment','movie_time','user_id')]
      })
    
    ###Dist_block
      print(v$id)
      x = 1:5
      y = select(movie_all[movie_all$id==v$id,],starts_with('rating'))
      y = sapply(y, function(x) as.numeric(sub("%", "", x))/100)
      y = rev(y)
      a = data.frame(x = x, y = y)
      outlier_frame = outlier_list[outlier_list$movie_id==v$id,]
      outlier_score = outlier_frame$movie_score
      outlier_id = outlier_frame$user_id
      b = data.frame(x = outlier_score, y = rep(0, length(outlier_score)), Woo = as.factor(outlier_id))
      print(b)
      outlier_comment = people_all[people_all$movie_id==v$id,c(4,6:8)]
      outlier_comment = outlier_comment[outlier_comment$user_id %in% outlier_id,]
      print(outlier_comment)
      
      output$user_dist <- renderPlot({
        if (check()>0.1){
            ggplot(a, mapping = aes(x = x, y = y)) + geom_smooth(col='royalblue')+
            theme_hc() + geom_point(x=v$user_rate,y=0, col='tomato',
                                  alpha = 0.25, cex=5)
        }else{
            ggplot(a, mapping = aes(x = x, y = y)) + geom_smooth(col='royalblue')+
            theme_hc() + geom_point(x=v$user_rate,y=0, col='tomato',alpha = 0.25, cex=5) +
            geom_point(b, mapping = aes(x=x,y=y,color=Woo), alpha = 0.4, cex=8)
        }
      })
      output$outlier_said <- renderTable({
        if(check()<=0.1){
          outlier_comment
        }
      })
      
    ###Movie_might
      poster_list=c()
      cor_list = movie_cor[v$id==rownames(movie_cor),]
      movie_might = names(cor_list)[tail(order(cor_list))][1:5]
      
      v$movie_might_id1 = movie_might[1]
      output$movie_might1 <- renderUI({
        tags$img(src = movie_all[movie_all$id==v$movie_might_id1,'poster'])
      })
      observeEvent(input$movie_select1,{
        id = v$movie_might_id1
        output$movie_might_info1 <- renderUI({
          type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
          type = type[!is.na(type)]
          type = paste(type,collapse = "/")
          score = movie_all[movie_all$id==id, 'score']
          pop = movie_all[movie_all$id==id, 'pop']
          rate = paste(score," (",pop,"  users scored)",sep = "")
          director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
          director = director[!is.na(director)]
          director = paste(director,collapse = "/")
          director = paste("Director:",director)
          actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
          actor = actor[!is.na(actor)]
          actor = paste(actor,collapse = "/")
          actor = paste("Actors",actor)
          tags$div(
            h3(movie_all[movie_all$id==id, 'English_name']),
            h4(movie_all[movie_all$id==id, 'name']),
            h4(rate),
            h5(type),
            h6(director),
            h6(actor)
          )
        })
      })
      
      v$movie_might_id2 = movie_might[2]
      output$movie_might2 <- renderUI({
        tags$img(src = movie_all[movie_all$id==v$movie_might_id2,'poster'])
      })
      observeEvent(input$movie_select2,{
        id = v$movie_might_id2
        output$movie_might_info2 <- renderUI({
          type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
          type = type[!is.na(type)]
          type = paste(type,collapse = "/")
          score = movie_all[movie_all$id==id, 'score']
          pop = movie_all[movie_all$id==id, 'pop']
          rate = paste(score," (",pop,"  users scored)",sep = "")
          director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
          director = director[!is.na(director)]
          director = paste(director,collapse = "/")
          director = paste("Director:",director)
          actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
          actor = actor[!is.na(actor)]
          actor = paste(actor,collapse = "/")
          actor = paste("Actors",actor)
          tags$div(
            h3(movie_all[movie_all$id==id, 'English_name']),
            h4(movie_all[movie_all$id==id, 'name']),
            h4(rate),
            h5(type),
            h6(director),
            h6(actor)
          )
        })
      })
      
      v$movie_might_id3 = movie_might[3]
      output$movie_might3 <- renderUI({
        tags$img(src = movie_all[movie_all$id==v$movie_might_id3,'poster'])
      })
      observeEvent(input$movie_select3,{
        id = v$movie_might_id3
        output$movie_might_info3 <- renderUI({
          type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
          type = type[!is.na(type)]
          type = paste(type,collapse = "/")
          score = movie_all[movie_all$id==id, 'score']
          pop = movie_all[movie_all$id==id, 'pop']
          rate = paste(score," (",pop,"  users scored)",sep = "")
          director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
          director = director[!is.na(director)]
          director = paste(director,collapse = "/")
          director = paste("Director:",director)
          actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
          actor = actor[!is.na(actor)]
          actor = paste(actor,collapse = "/")
          actor = paste("Actors",actor)
          tags$div(
            h3(movie_all[movie_all$id==id, 'English_name']),
            h4(movie_all[movie_all$id==id, 'name']),
            h4(rate),
            h5(type),
            h6(director),
            h6(actor)
          )
        })
      })
      
      v$movie_might_id4 = movie_might[4]
      output$movie_might4 <- renderUI({
        tags$img(src = movie_all[movie_all$id==v$movie_might_id4,'poster'])
      })
      observeEvent(input$movie_select4,{
        id = v$movie_might_id4
        output$movie_might_info4 <- renderUI({
          type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
          type = type[!is.na(type)]
          type = paste(type,collapse = "/")
          score = movie_all[movie_all$id==id, 'score']
          pop = movie_all[movie_all$id==id, 'pop']
          rate = paste(score," (",pop,"  users scored)",sep = "")
          director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
          director = director[!is.na(director)]
          director = paste(director,collapse = "/")
          director = paste("Director:",director)
          actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
          actor = actor[!is.na(actor)]
          actor = paste(actor,collapse = "/")
          actor = paste("Actors",actor)
          tags$div(
            h3(movie_all[movie_all$id==id, 'English_name']),
            h4(movie_all[movie_all$id==id, 'name']),
            h4(rate),
            h5(type),
            h6(director),
            h6(actor)
          )
        })
      })
      
      v$movie_might_id5 = movie_might[5]
      output$movie_might5 <- renderUI({
        tags$img(src = movie_all[movie_all$id==v$movie_might_id5,'poster'])
      })
      observeEvent(input$movie_select5,{
        id = v$movie_might_id5
        output$movie_might_info5 <- renderUI({
          type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
          type = type[!is.na(type)]
          type = paste(type,collapse = "/")
          score = movie_all[movie_all$id==id, 'score']
          pop = movie_all[movie_all$id==id, 'pop']
          rate = paste(score," (",pop,"  users scored)",sep = "")
          director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
          director = director[!is.na(director)]
          director = paste(director,collapse = "/")
          director = paste("Director:",director)
          actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
          actor = actor[!is.na(actor)]
          actor = paste(actor,collapse = "/")
          actor = paste("Actors",actor)
          tags$div(
            h3(movie_all[movie_all$id==id, 'English_name']),
            h4(movie_all[movie_all$id==id, 'name']),
            h4(rate),
            h5(type),
            h6(director),
            h6(actor)
          )
        })
      })
      
      
      ###User_might
        poster_list2=c()
        cor_list2 = movie_cor2[v$id==rownames(movie_cor2),]
        user_might = names(cor_list2)[tail(order(cor_list2))][1:5]
        
        v$user_might_id1 = user_might[1]
        output$user_might1 <- renderUI({
          tags$img(src = movie_all[movie_all$id==v$user_might_id1,'poster'])
        })
        observeEvent(input$user_select1,{
          id = v$user_might_id1
          output$user_might_info1 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"  users scored)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("Director:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("Actors",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        
        v$user_might_id2 = user_might[2]
        output$user_might2 <- renderUI({
          tags$img(src = movie_all[movie_all$id==v$user_might_id2,'poster'])
        })
        observeEvent(input$user_select2,{
          id = v$user_might_id2
          output$user_might_info2 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"  users scored)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("Director:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("Actors",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        
        v$user_might_id3 = user_might[3]
        output$user_might3 <- renderUI({
          tags$img(src = movie_all[movie_all$id==v$user_might_id3,'poster'])
        })
        observeEvent(input$user_select3,{
          id = v$user_might_id3
          output$user_might_info3 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"  users scored)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("Director:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("Actors",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        
        v$user_might_id4 = user_might[4]
        output$user_might4 <- renderUI({
          tags$img(src = movie_all[movie_all$id==v$user_might_id4,'poster'])
        })
        observeEvent(input$user_select4,{
          id = v$user_might_id4
          output$user_might_info4 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"  users scored)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("Director:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("Actors",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        
        v$user_might_id5 = user_might[5]
        output$user_might5 <- renderUI({
          tags$img(src = movie_all[movie_all$id==v$user_might_id5,'poster'])
        })
        observeEvent(input$user_select5,{
          id = v$user_might_id5
          output$user_might_info5 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"  users scored)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("Director:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("Actors",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        
    ###User analyse
        observeEvent(input$analyse,{
          if(length(docu2[,1])>3){
            docu2 = read.table("./data/docu.rdata")
            docu3 = docu2[-1,]
            tem_score = data.frame(x = docu3$score)
            output$score_hist <- renderPlot({
                ggplot(tem_score, aes(x)) + geom_histogram(fill='royalblue') + theme_hc()
              })
          }
          
        })
        
  })
  
  
  observeEvent(input$refresh1,{
    v$cluster_id1 = sample(settle_cluster[[1]],1) 
    poster = movie_all[movie_all$id==v$cluster_id1, 'poster']
    id = v$cluster_id1
    output$cluster1_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh2,{
    v$cluster_id2 = sample(settle_cluster[[2]],1) 
    poster = movie_all[movie_all$id==v$cluster_id2, 'poster']
    id = v$cluster_id2
    output$cluster2_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh3,{
    v$cluster_id3 = sample(settle_cluster[[3]],1) 
    poster = movie_all[movie_all$id==v$cluster_id3, 'poster']
    id = v$cluster_id3
    output$cluster3_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh4,{
    v$cluster_id4 = sample(settle_cluster[[4]],1) 
    poster = movie_all[movie_all$id==v$cluster_id4, 'poster']
    id = v$cluster_id4
    output$cluster4_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh5,{
    v$cluster_id5 = sample(settle_cluster[[5]],1) 
    poster = movie_all[movie_all$id==v$cluster_id5, 'poster']
    id = v$cluster_id5
    output$cluster5_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh6,{
    v$cluster_id6 = sample(settle_cluster[[6]],1) 
    poster = movie_all[movie_all$id==v$cluster_id6, 'poster']
    id = v$cluster_id6
    output$cluster6_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh7,{
    v$cluster_id7 = sample(settle_cluster[[7]],1) 
    poster = movie_all[movie_all$id==v$cluster_id7, 'poster']
    id = v$cluster_id7
    output$cluster7_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh8,{
    v$cluster_id8 = sample(settle_cluster[[8]],1) 
    poster = movie_all[movie_all$id==v$cluster_id8, 'poster']
    id = v$cluster_id8
    output$cluster8_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh9,{
    v$cluster_id9 = sample(settle_cluster[[9]],1) 
    poster = movie_all[movie_all$id==v$cluster_id9, 'poster']
    id = v$cluster_id9
    output$cluster9_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh10,{
    v$cluster_id10 = sample(settle_cluster[[10]],1) 
    poster = movie_all[movie_all$id==v$cluster_id10, 'poster']
    id = v$cluster_id10
    output$cluster10_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh11,{
    v$cluster_id11 = sample(settle_cluster[[11]],1) 
    poster = movie_all[movie_all$id==v$cluster_id11, 'poster']
    id = v$cluster_id11
    output$cluster11_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh12,{
    v$cluster_id12 = sample(settle_cluster[[12]],1) 
    poster = movie_all[movie_all$id==v$cluster_id12, 'poster']
    id = v$cluster_id12
    output$cluster12_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh13,{
    v$cluster_id13 = sample(settle_cluster[[13]],1) 
    poster = movie_all[movie_all$id==v$cluster_id13, 'poster']
    id = v$cluster_id13
    output$cluster13_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"  users scored)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("Director:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("Actors",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh_cluster,{
    id_list = movie_cluster[,sample(1:50,1)]
    id_list = rev(id_list)
    
    v$cluster_might_id1 = id_list[1]
    output$cluster_might1 <- renderUI({
      tags$img(src = movie_all[movie_all$id== v$cluster_might_id1,'poster'])
    })
    observeEvent(input$cluster_select1,{
      id =  v$cluster_might_id1
      output$cluster_might_info1 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"  users scored)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("Director:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("Actors",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    v$cluster_might_id2 = id_list[2]
    output$cluster_might2 <- renderUI({
      tags$img(src = movie_all[movie_all$id== v$cluster_might_id2,'poster'])
    })
    observeEvent(input$cluster_select2,{
      id =  v$cluster_might_id2
      output$cluster_might_info2 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"  users scored)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("Director:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("Actors",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    v$cluster_might_id3 = id_list[3]
    output$cluster_might3 <- renderUI({
      tags$img(src = movie_all[movie_all$id== v$cluster_might_id3,'poster'])
    })
    observeEvent(input$cluster_select3,{
      id =  v$cluster_might_id3
      output$cluster_might_info3 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"  users scored)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("Director:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("Actors",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    v$cluster_might_id4 = id_list[4]
    output$cluster_might4 <- renderUI({
      tags$img(src = movie_all[movie_all$id== v$cluster_might_id4,'poster'])
    })
    observeEvent(input$cluster_select4,{
      id =  v$cluster_might_id4
      output$cluster_might_info4 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"  users scored)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("Director:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("Actors",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    v$cluster_might_id5 = id_list[5]
    output$cluster_might5 <- renderUI({
      tags$img(src = movie_all[movie_all$id== v$cluster_might_id5,'poster'])
    })
    observeEvent(input$cluster_select5,{
      id =  v$cluster_might_id5
      output$cluster_might_info5 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"  users scored)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("Director:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("Actors",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    v$cluster_might_id6 = id_list[6]
    output$cluster_might6 <- renderUI({
      tags$img(src = movie_all[movie_all$id== v$cluster_might_id6,'poster'])
    })
    observeEvent(input$cluster_select6,{
      id =  v$cluster_might_id6
      output$cluster_might_info6 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"    users scored)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("Director:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("Actors",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
  })
###Isolate_ones!!!
  
  #main
  isolate({
    observeEvent(input$save_inputs, {
      docu2 <- read.table("./data/docu.rdata")
      v$user_rate = as.numeric(gsub('(\\d)star','\\1',input$user_score))
      print(v$user_rate)
      docu2 = rbind(docu2, data.frame(score=v$user_rate, id = v$id))
      write.table(docu2, "./data/docu.rdata")
    })
    
  })
  
  #movie_might
  isolate({
    observeEvent(input$save_movie_might1, {
      docu2 <- read.table("./data/docu.rdata")
      v$movie_might_score1 = as.numeric(gsub('(\\d)star','\\1',input$movie_might_score1))
      print(v$movie_might_score1)
      docu2 = rbind(docu2, data.frame(score=v$movie_might_score1, id = v$movie_might_id1))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_movie_might2, {
      docu2 <- read.table("./data/docu.rdata")
      v$movie_might_score2 = as.numeric(gsub('(\\d)star','\\1',input$movie_might_score2))
      print(v$movie_might_score2)
      docu2 = rbind(docu2, data.frame(score=v$movie_might_score2, id = v$movie_might_id2))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_movie_might3, {
      docu2 <- read.table("./data/docu.rdata")
      v$movie_might_score3 = as.numeric(gsub('(\\d)star','\\1',input$movie_might_score3))
      print(v$movie_might_score3)
      docu2 = rbind(docu2, data.frame(score=v$movie_might_score3, id = v$movie_might_id3))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_movie_might4, {
      docu2 <- read.table("./data/docu.rdata")
      v$movie_might_score4 = as.numeric(gsub('(\\d)star','\\1',input$movie_might_score4))
      print(v$movie_might_score4)
      docu2 = rbind(docu2, data.frame(score=v$movie_might_score4, id = v$movie_might_id4))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_movie_might5, {
      docu2 <- read.table("./data/docu.rdata")
      v$movie_might_score5 = as.numeric(gsub('(\\d)star','\\1',input$movie_might_score5))
      print(v$movie_might_score5)
      docu2 = rbind(docu2, data.frame(score=v$movie_might_score5, id = v$movie_might_id5))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  #user_might
  isolate({
    observeEvent(input$save_user_might1, {
      docu2 <- read.table("./data/docu.rdata")
      v$user_might_score1 = as.numeric(gsub('(\\d)star','\\1',input$user_might_score1))
      print(v$user_might_score1)
      docu2 = rbind(docu2, data.frame(score=v$user_might_score1, id = v$user_might_id1))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_user_might2, {
      docu2 <- read.table("./data/docu.rdata")
      v$user_might_score2 = as.numeric(gsub('(\\d)star','\\1',input$user_might_score2))
      print(v$user_might_score2)
      docu2 = rbind(docu2, data.frame(score=v$user_might_score2, id = v$user_might_id2))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_user_might3, {
      docu2 <- read.table("./data/docu.rdata")
      v$user_might_score3 = as.numeric(gsub('(\\d)star','\\1',input$user_might_score3))
      print(v$user_might_score3)
      docu2 = rbind(docu2, data.frame(score=v$user_might_score3, id = v$user_might_id3))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_user_might4, {
      docu2 <- read.table("./data/docu.rdata")
      v$user_might_score4 = as.numeric(gsub('(\\d)star','\\1',input$user_might_score4))
      print(v$user_might_score4)
      docu2 = rbind(docu2, data.frame(score=v$user_might_score4, id = v$user_might_id4))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_user_might5, {
      docu2 <- read.table("./data/docu.rdata")
      v$user_might_score5 = as.numeric(gsub('(\\d)star','\\1',input$user_might_score5))
      print(v$user_might_score5)
      docu2 = rbind(docu2, data.frame(score=v$user_might_score5, id = v$user_might_id5))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  #cluster
  isolate({
    observeEvent(input$save_cluster1, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score1 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_1))
      print(v$cluster_score1)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score1, id = v$cluster_id1))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster2, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score2 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_2))
      print(v$cluster_score2)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score2, id = v$cluster_id2))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster3, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score3 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_3))
      print(v$cluster_score3)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score3, id = v$cluster_id3))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster4, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score4 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_4))
      print(v$cluster_score4)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score4, id = v$cluster_id4))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster5, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score5 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_5))
      print(v$cluster_score5)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score5, id = v$cluster_id5))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster6, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score6 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_6))
      print(v$cluster_score6)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score6, id = v$cluster_id6))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster7, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score7 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_7))
      print(v$cluster_score7)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score7, id = v$cluster_id7))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster8, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score8 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_8))
      print(v$cluster_score8)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score8, id = v$cluster_id8))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster9, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score9 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_9))
      print(v$cluster_score9)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score9, id = v$cluster_id9))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster10, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score10 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_10))
      print(v$cluster_score10)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score10, id = v$cluster_id10))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster11, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score11 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_11))
      print(v$cluster_score11)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score11, id = v$cluster_id11))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster12, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score12 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_12))
      print(v$cluster_score12)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score12, id = v$cluster_id12))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster13, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_score13 = as.numeric(gsub('(\\d)star','\\1',input$cluster_score_13))
      print(v$cluster_score13)
      docu2 = rbind(docu2, data.frame(score=v$cluster_score13, id = v$cluster_id13))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  #cluster_might
  
  isolate({
    observeEvent(input$save_cluster_might1, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_might_score1 = as.numeric(gsub('(\\d)star','\\1',input$cluster_might_score1))
      print(v$cluster_might_score1)
      docu2 = rbind(docu2, data.frame(score=v$cluster_might_score1, id = v$cluster_might_id1))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster_might2, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_might_score2 = as.numeric(gsub('(\\d)star','\\1',input$cluster_might_score2))
      print(v$cluster_might_score2)
      docu2 = rbind(docu2, data.frame(score=v$cluster_might_score2, id = v$cluster_might_id2))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster_might3, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_might_score3 = as.numeric(gsub('(\\d)star','\\1',input$cluster_might_score3))
      print(v$cluster_might_score3)
      docu2 = rbind(docu2, data.frame(score=v$cluster_might_score3, id = v$cluster_might_id3))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster_might4, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_might_score4 = as.numeric(gsub('(\\d)star','\\1',input$cluster_might_score4))
      print(v$cluster_might_score4)
      docu2 = rbind(docu2, data.frame(score=v$cluster_might_score4, id = v$cluster_might_id4))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster_might5, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_might_score5 = as.numeric(gsub('(\\d)star','\\1',input$cluster_might_score5))
      print(v$cluster_might_score5)
      docu2 = rbind(docu2, data.frame(score=v$cluster_might_score5, id = v$cluster_might_id5))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
  isolate({
    observeEvent(input$save_cluster_might6, {
      docu2 <- read.table("./data/docu.rdata")
      v$cluster_might_score6 = as.numeric(gsub('(\\d)star','\\1',input$cluster_might_score6))
      print(v$cluster_might_score6)
      docu2 = rbind(docu2, data.frame(score=v$cluster_might_score6, id = v$cluster_might_id6))
      write.table(docu2, "./data/docu.rdata")
    })
  })
  
}

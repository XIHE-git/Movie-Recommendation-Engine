
fluidPage(
  navbarPage(
    title="Welcome!",
    id = 'index',
    tabPanel("New_user", icon=icon("home")),
    tabPanel("Who_are_you"),
    tabPanel("Douban_Guess")
  ),
  ###main block
  
  ###Douban_Guess block
  conditionalPanel(
    condition = 'input.index == "Douban_Guess"',
    navlistPanel(
      well = FALSE,
      id = 'side',
      widths = c(3, 8),
      tabPanel("Information"),
      tabPanel("Comments"),
      tabPanel("Score distribution"),
      tabPanel("You might also be interested"),
      conditionalPanel(
        condition = 'input.side == "Information"',
        uiOutput(outputId = "sys_condition"),
        uiOutput(outputId = "movie_info_main"),
        uiOutput(outputId = "real_rate"),
        radioButtons('user_score', '', choices = c('5star','4star','3star','2star','1star'), 
                     inline = T, selected='5star'), 
        actionButton("refresh", "Refresh movie"),
        actionButton('save_inputs', 'Save inputs'),
        uiOutput(outputId = "outlier")
      ),
      
      conditionalPanel(
        condition = 'input.side == "Comments"',
        radioButtons('comment_star', '', choices = c('5star','4star','3star','2star','1star'), 
                     inline = T, selected='5star'),
        conditionalPanel(
          condition = 'input.comment_star == "5star"',
          tableOutput(outputId = "movie_inspect5")
        ),
        conditionalPanel(
          condition = 'input.comment_star == "4star"',
          tableOutput(outputId = "movie_inspect4")
        ),
        conditionalPanel(
          condition = 'input.comment_star == "3star"',
          tableOutput(outputId = "movie_inspect3")
        ),
        conditionalPanel(
          condition = 'input.comment_star == "2star"',
          tableOutput(outputId = "movie_inspect2")
        ),
        conditionalPanel(
          condition = 'input.comment_star == "1star"',
          tableOutput(outputId = "movie_inspect1")
        )
      ),
      
      conditionalPanel(
        condition = 'input.side == "Score distribution"',
        plotOutput('user_dist'),
        tableOutput(outputId = "outlier_said")
      ),
      
      conditionalPanel(
        condition = 'input.side == "You might also be interested"',
        radioButtons('by_way', '', choices = c("By movie information","By user score"), 
                     inline = T, selected='By movie information'),
        conditionalPanel(
          condition = 'input.by_way == "By movie information"',
          uiOutput(outputId = "movie_might1"),
          radioButtons('movie_select1', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.movie_select1=="Score it!"',
            radioButtons('movie_might_score1', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_movie_might1', 'Save')
          ),
          conditionalPanel(
            condition = 'input.movie_select1=="Information"',
            uiOutput(outputId = "movie_might_info1")
          ),
          
          uiOutput(outputId = "movie_might2"),
          radioButtons('movie_select2', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.movie_select2=="Score it!"',
            radioButtons('movie_might_score2', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_movie_might2', 'Save')
          ),
          conditionalPanel(
            condition = 'input.movie_select2=="Information"',
            uiOutput(outputId = "movie_might_info2")
          ),
          
          uiOutput(outputId = "movie_might3"),
          radioButtons('movie_select3', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.movie_select3=="Score it!"',
            radioButtons('movie_might_score3', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_movie_might3', 'Save')
          ),
          conditionalPanel(
            condition = 'input.movie_select3=="Information"',
            uiOutput(outputId = "movie_might_info3")
          ),
          
          uiOutput(outputId = "movie_might4"),
          radioButtons('movie_select4', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.movie_select4=="Score it!"',
            radioButtons('movie_might_score4', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_movie_might4', 'Save')
          ),
          conditionalPanel(
            condition = 'input.movie_select4=="Information"',
            uiOutput(outputId = "movie_might_info4")
          ),
          
          uiOutput(outputId = "movie_might5"),
          radioButtons('movie_select5', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.movie_select5=="Score it!"',
            radioButtons('movie_might_score5', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_movie_might5', 'Save')
          ),
          conditionalPanel(
            condition = 'input.movie_select5=="Information"',
            uiOutput(outputId = "movie_might_info5")
          )
        ),
        
        
        conditionalPanel(
          condition = 'input.by_way == "By user score"',
          uiOutput(outputId = "user_might1"),
          radioButtons('user_select1', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.user_select1=="Score it!"',
            radioButtons('user_might_score1', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_user_might1', 'Save')
          ),
          conditionalPanel(
            condition = 'input.user_select1=="Information"',
            uiOutput(outputId = "user_might_info1")
          ),
          
          uiOutput(outputId = "user_might2"),
          radioButtons('user_select2', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.user_select2=="Score it!"',
            radioButtons('user_might_score2', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_user_might2', 'Save')
          ),
          conditionalPanel(
            condition = 'input.user_select2=="Information"',
            uiOutput(outputId = "user_might_info2")
          ),
          
          uiOutput(outputId = "user_might3"),
          radioButtons('user_select3', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.user_select3=="Score it!"',
            radioButtons('user_might_score3', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_user_might3', 'Save')
          ),
          conditionalPanel(
            condition = 'input.user_select3=="Information"',
            uiOutput(outputId = "user_might_info3")
          ),
          
          uiOutput(outputId = "user_might4"),
          radioButtons('user_select4', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.user_select4=="Score it!"',
            radioButtons('user_might_score4', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_user_might4', 'Save')
          ),
          conditionalPanel(
            condition = 'input.user_select4=="Information"',
            uiOutput(outputId = "user_might_info4")
          ),
          
          uiOutput(outputId = "user_might5"),
          radioButtons('user_select5', '', choices = c("Score it!","Information"), 
                       inline = T, selected="Information"), 
          conditionalPanel(
            condition = 'input.user_select5=="Score it!"',
            radioButtons('user_might_score5', '', choices = c('5star','4star','3star','2star','1star'), 
                         inline = T, selected='5star'),
            actionButton('save_user_might5', 'Save')
          ),
          conditionalPanel(
            condition = 'input.user_select5=="Information"',
            uiOutput(outputId = "user_might_info5")
          )
        )
      )
    )
  ),
  
  ###user_analyse block(connect with server!)
  conditionalPanel(
    condition = 'input.index == "Who_are_you"',
    navlistPanel(
      well = FALSE,
      id = 'analyse',
      widths = c(3, 8),
      tabPanel("Score distribution"),
      tabPanel("Constructing...")
    ),
      
      conditionalPanel(
        condition = 'input.analyse == "Score distribution"',
        plotOutput(outputId = 'score_hist')
      )
      
  ),
  
  
  conditionalPanel(
    condition = 'input.index == "New_user"',
    navlistPanel(
      well = FALSE,
      id = 'cluster',
      widths = c(3, 8),
      tabPanel("Time"),
      tabPanel("Type"),
      tabPanel("AI Group"),
      
      conditionalPanel(
        condition = 'input.cluster == "Time"',
        radioButtons('cluster_time', '', choices = c('Before 1980','1981-2000','2001-now'), 
                     inline = T, selected='Before 1980'),
        
        conditionalPanel(
          condition = 'input.cluster_time == "Before 1980"',
          actionButton('refresh1', "Give me one！"),
          uiOutput(outputId = 'cluster1_info'),
          radioButtons('cluster_score_1', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster1', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_time == "1981-2000"',
          actionButton('refresh2', "Give me one！"),
          uiOutput(outputId = 'cluster2_info'),
          radioButtons('cluster_score_2', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster2', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_time == "2001-now"',
          actionButton('refresh3', "Give me one！"),
          uiOutput(outputId = 'cluster3_info'),
          radioButtons('cluster_score_3', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster3', 'Save')
        )
      ),
      
      conditionalPanel(
        condition = 'input.cluster == "Type"',
        radioButtons('cluster_type', '', choices = c('Biography','Action','Cartoon','Comedy','Fantasy',
                                                     'Thriller','Romance','Science fiction','Documentary','LGBT'), 
                     inline = T, selected='Biography'),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Biography"',
          actionButton('refresh4', "Give me one！"),
          uiOutput(outputId = 'cluster4_info'),
          radioButtons('cluster_score_4', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster4', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Action"',
          actionButton('refresh5', "Give me one！"),
          uiOutput(outputId = 'cluster5_info'),
          radioButtons('cluster_score_5', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster5', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Cartoon"',
          actionButton('refresh6', "Give me one！"),
          uiOutput(outputId = 'cluster6_info'),
          radioButtons('cluster_score_6', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster6', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Comedy"',
          actionButton('refresh7', "Give me one！"),
          uiOutput(outputId = 'cluster7_info'),
          radioButtons('cluster_score_7', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster7', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Fantasy"',
          actionButton('refresh8', "Give me one！"),
          uiOutput(outputId = 'cluster8_info'),
          radioButtons('cluster_score_8', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster8', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Thriller"',
          actionButton('refresh9', "Give me one！"),
          uiOutput(outputId = 'cluster9_info'),
          radioButtons('cluster_score_9', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster9', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Romance"',
          actionButton('refresh10', "Give me one！"),
          uiOutput(outputId = 'cluster10_info'),
          radioButtons('cluster_score_10', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster10', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Science fiction"',
          actionButton('refresh11', "Give me one！"),
          uiOutput(outputId = 'cluster11_info'),
          radioButtons('cluster_score_11', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster11', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "Documentary"',
          actionButton('refresh12', "Give me one！"),
          uiOutput(outputId = 'cluster12_info'),
          radioButtons('cluster_score_12', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster12', 'Save')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "LGBT"',
          actionButton('refresh13', "Give me one！"),
          uiOutput(outputId = 'cluster13_info'),
          radioButtons('cluster_score_13', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster13', 'Save')
        )
      ),
      
      conditionalPanel(
        condition = 'input.cluster == "AI Group"',
        actionButton('refresh_cluster', "Give me a group！"),
        
        uiOutput(outputId = "cluster_might1"),
        radioButtons('cluster_select1', '', choices = c("Score it!","Information"), 
                     inline = T, selected="Information"), 
        conditionalPanel(
          condition = 'input.cluster_select1=="Score it!"',
          radioButtons('cluster_might_score1', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster_might1', 'Save')
        ),
        conditionalPanel(
          condition = 'input.cluster_select1=="Information"',
          uiOutput(outputId = "cluster_might_info1")
        ),
        
        uiOutput(outputId = "cluster_might2"),
        radioButtons('cluster_select2', '', choices = c("Score it!","Information"), 
                     inline = T, selected="Information"), 
        conditionalPanel(
          condition = 'input.cluster_select2=="Score it!"',
          radioButtons('cluster_might_score2', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster_might2', 'Save')
        ),
        conditionalPanel(
          condition = 'input.cluster_select2=="Information"',
          uiOutput(outputId = "cluster_might_info2")
        ),
        
        uiOutput(outputId = "cluster_might3"),
        radioButtons('cluster_select3', '', choices = c("Score it!","Information"), 
                     inline = T, selected="Information"), 
        conditionalPanel(
          condition = 'input.cluster_select3=="Score it!"',
          radioButtons('cluster_might_score3', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster_might3', 'Save')
        ),
        conditionalPanel(
          condition = 'input.cluster_select3=="Information"',
          uiOutput(outputId = "cluster_might_info3")
        ),
        
        uiOutput(outputId = "cluster_might4"),
        radioButtons('cluster_select4', '', choices = c("Score it!","Information"), 
                     inline = T, selected="Information"), 
        conditionalPanel(
          condition = 'input.cluster_select4=="Score it!"',
          radioButtons('cluster_might_score4', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster_might4', 'Save')
        ),
        conditionalPanel(
          condition = 'input.cluster_select4=="Information"',
          uiOutput(outputId = "cluster_might_info4")
        ),
        
        uiOutput(outputId = "cluster_might5"),
        radioButtons('cluster_select5', '', choices = c("Score it!","Information"), 
                     inline = T, selected="Information"), 
        conditionalPanel(
          condition = 'input.cluster_select5=="Score it!"',
          radioButtons('cluster_might_score5', '', choices = c('5star','4star','3star','2star','1star'), 
                       inline = T, selected='5star'),
          actionButton('save_cluster_might5', 'Save')
        ),
        conditionalPanel(
          condition = 'input.cluster_select5=="Information"',
          uiOutput(outputId = "cluster_might_info5")
        )
      )
    )
  ),
  
  tags$head(
    tags$style(".tab-content .tab-content {border: 1px solid gray; min-height:200px;}")
  )
)
if (!require("pacman")) install.packages("pacman")
packages<-c("shiny","shinythemes","DT","DataExplorer",
            "glmnet","leaps","e1071","tibble",
            "rpart","randomForest","xgboost","Matrix","JOUSBoost","solitude",
            "class","arulesCBA","ROCR","tidyverse")
pacman::p_load(char=packages)

ui <- fluidPage(theme = shinythemes::shinytheme("cerulean"),
                navbarPage(p(strong("Data Science and Analysis")),
                           tabPanel("Upload Datasets",
                                    fileInput("upload", "Choose a file", buttonLabel = "Upload...",accept = ".csv",multiple = T),
                                    checkboxInput("header", "Header", T),
                                    radioButtons("sep", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ","),
                                    DT::dataTableOutput("dataset"),
                                    style="overflow-x: scroll"),
                           tabPanel("Exploratory Data Analysis",
                                    h1("Basic Stats"),
                                    plotOutput("PlotIntro"),
                                    h1("Missing Data Profile"),
                                    plotOutput("PlotMissing"),
                                    h1("Univariate Distributions"),
                                    h2("Bar Charts"),
                                    plotOutput("Bars"),
                                    h2("Histograms"),
                                    plotOutput("Histograms"),
                                    h1("Correlation Analysis"),
                                    plotOutput("Correlation"),
                                    style="overflow-y: scroll"),
                           tabPanel("Modelling",
                                    sidebarPanel(uiOutput("Target"),
                                                 selectInput("Models", "Choose a Model:",
                                                             list("Linear" = c("logistic","ridge","lasso","bestsubset","svm","oneclasssvm"),
                                                                  "Tree" = c("decisiontree","randomforest","xgboost","adaboost","isolationforest"),
                                                                  "Non-linear"=c("knn","naivebayes","rulebased")),
                                                             selected = "logistic"),
                                                 numericInput("Trainsplit","Enter training split percentage (0-1):",
                                                              min = 0,max = 1,value = 0.7,step = 0.1),
                                                 numericInput("Seed","Random Seed:",
                                                              min = 0,max = 10000000000,value = 1),
                                                 actionButton("Build","Build Model!",width = "100%",icon=icon("hammer"),class="btn btn-primary"),
                                                 h3("Hyperparameter Tuning"),
                                                 uiOutput("param1"),
                                                 uiOutput("param2"),
                                                 uiOutput("param3"),
                                                 uiOutput("param4"),
                                                 uiOutput("param5"),
                                                 uiOutput("param6"),
                                                 uiOutput("param7")),
                                    mainPanel(h1("Accuracy Metrics"),
                                              h2("Confusion Matrix"),
                                              plotOutput("ConfusionMatrix"),
                                              h2("ROC Curve"),
                                              plotOutput("ROC"))),
                           tabPanel("Model Leaderboard",
                                    DT::dataTableOutput("leaderboard")),
                           tabPanel("Make New Prediction",
                                    fileInput("newdata", "Choose a file", buttonLabel = "Upload...",accept = ".csv"),
                                    DT::dataTableOutput("newpreview"),
                                    downloadButton("downloadPreds", "Download Predictions"),
                                    actionButton("newpreds","Make New Predictions",icon=icon("search"),class="btn btn-primary"),
                                    style="overflow-x: scroll")
                )
)

server <- function(input, output, session) {
  
  #initialize leaderboard#
  
  bd<-data.frame(Model=character(),
                 Logloss=numeric(),
                 AUC=numeric(),
                 Accuracy=numeric(),
                 TPR=numeric(),
                 FPR=numeric())
  v<-reactiveValues(df=bd)
  updated<-reactive({
    input$Build
    isolate({
      v$df<-rbind.data.frame(v$df,calculate()$insert,make.row.names = F)
    })
  })
  
  #read input file as reactive data
  data<-reactive({
    req(input$upload)
    df <- read.csv(input$upload$datapath,
                   header = input$header,
                   sep = input$sep)
    return(df)
  })
  
  #preview input file
  output$dataset <- DT::renderDataTable({
    return(data())
  })
  
  #exploratory data analysis
  output$PlotIntro<-renderPlot({
    DataExplorer::plot_intro(data())
  })
  output$PlotMissing<-renderPlot({
    DataExplorer::plot_missing(data())
  })
  output$Bars<-renderPlot({
    DataExplorer::plot_bar(data())
  })
  output$Histograms<-renderPlot({
    DataExplorer::plot_histogram(data())
  })
  output$Correlation<-renderPlot({
    DataExplorer::plot_correlation(data())
  })
  
  #modelling
  output$Target <- renderUI({
    req(data())
    df <- data()
    selectInput("Target", "Select a variable as target:", choices= names(df))
  })
  output$param1<-renderUI({
    req(data())
    req(input$Trainsplit)
    df <- data()
    n <- nrow(df)
    RNGkind(sample.kind = "Rounding")
    set.seed(input$Seed)
    train <- sample(n,input$Trainsplit*n)
    if(input$Models=="logistic"){
      numericInput("significance","Significant Level (alpha):",
                   min = 0.01,max = 0.1,value = 0.05)
    }
    else if(input$Models=="ridge"){
      numericInput("lambdaA","Number of Lambda Values to Try (default: 100):",
                   min = 1,max = 1000,value = 100)
    }
    else if(input$Models=="lasso"){
      numericInput("lambdaB","Number of Lambda Values to Try (default: 100):",
                   min = 1,max = 1000,value = 100)
    }
    else if(input$Models=="bestsubset"){
      numericInput("nvmax","Maximum Size of Subsets:",
                   min = 1,max = ncol(df)-1,value = ncol(df)-1)
    }
    else if(input$Models=="svm"){
      selectInput("svmkernel","Select kernel:",
                  choices = c("linear","polynomial","radial","sigmoid"),selected = "linear")
    }
    else if(input$Models=="oneclasssvm"){
      selectInput("oneclasskernel","Select kernel:",
                  choices = c("linear","polynomial","radial","sigmoid"),selected = "linear")
    }
    else if(input$Models=="decisiontree"){
      numericInput("minsplit","Minimum Observations for a Split",
                   min = 1,max=100,value=20,step = 1)
    }
    else if(input$Models=="randomforest"){
      numericInput("ntrees","Number of Trees to Grow:",
                   min=100,max=1000,value=500)
    }
    else if(input$Models=="xgboost"){
      numericInput("nrounds","Maximum Number of Boosting Iterations:",
                   min=100,max=1000,value=500)
    }
    else if(input$Models=="adaboost"){
      numericInput("adanrounds","Maximum Number of Boosting Iterations:",
                   min=100,max=1000,value=500)
    }
    else if(input$Models=="isolationforest"){
      numericInput("ratio","Percentage of Anomalies",
                   min=round(sum(df[train,which(names(df) %in% input$Target)])/length(train),2),max=0.99,
                   value=round(sum(df[train,which(names(df) %in% input$Target)])/length(train),2),step = .01)
    }
    else if(input$Models=="knn"){
      numericInput("neighbours","Number of Nearest Neighbours, K:",
                   min=1, max=20,value = 20)
    }
    else if(input$Models=="rulebased"){
      numericInput("support","Support Level",
                   min=0.01,max=0.99,value=0.01)
    }
    
  })
  output$param2<-renderUI({
    req(input$Models)
    df <- data()
    if(input$Models=="bestsubset"){
      selectInput("bsmethod","Selection Method:",
                  choices = c("exhaustive","backward","forward"),selected = "exhaustive")
    }
    else if(input$Models=="decisiontree"){
      numericInput("maxdepth","Maximum Depth of Tree",
                   min = 1,max=30,value=30,step = 1)
    }
    else if(input$Models=="randomforest"){
      numericInput("mtry","Number of Variables to use each split:",
                   min=1, max=ncol(df)-1,value=floor(sqrt(ncol(df)-1)))
    }
    else if(input$Models=="xgboost"){
      numericInput("earlystop","Number of Early Stopping Rounds:",
                   min=0,max=10,value=5)
    }
    else if(input$Models=="adaboost"){
      numericInput("adamaxdepth","Maximum Depth of Tree",
                   min = 1,max=3,value=2,step = 1)
    }
    else if(input$Models=="knn"){
      radioButtons("knnsample","Sampling:",
                   choices = c("cross","bootstrap"),
                   selected = "cross")
    }
    else if(input$Models=="rulebased"){
      numericInput("confidence","Confidence Level",
                   min=0.01,max=0.99,value=0.8)
    }
  })
  output$param3<-renderUI({
    req(data())
    req(input$Models)
    if(input$Models=="bestsubset"){
      radioButtons("bsmetric","Metric:",
                   choices = c(BIC="bic",AIC="aic",AdjR2="adjr2"),selected = "bic")
    }
    else if(input$Models=="randomforest"){
      radioButtons("sampling","Bootstrap Sampling?",
                   choices = c("T","F"),selected = "T")
    }
    else if(input$Models=="xgboost"){
      numericInput("eta","Learning Rate:",
                   min=0.01,max=0.99,value = 0.3)
    }
  })
  output$param4<-renderUI({
    req(data())
    req(input$Models)
    if(input$Models=="xgboost"){
      numericInput("xgbmaxdepth","Maximum Depth of Trees:",
                   min=2,max=30,value=6)
    }
  })
  output$param5<-renderUI({
    req(data())
    req(input$Models)
    if(input$Models=="xgboost"){
      numericInput("xgboostalpha","L1 Regularization",
                   min=0,max=1,value = 0)
    }
  })
  output$param6<-renderUI({
    req(data())
    req(input$Models)
    if(input$Models=="xgboost"){
      numericInput("xgboostlambda","L2 Regularization",
                   min=0,max=1,value = 0)
    }
  })
  output$param7<-renderUI({
    req(data())
    req(input$Models)
    if(input$Models=="xgboost"){
      radioButtons("xgboostmetrics","Metrics:",
                   choices = c("Logloss"="logloss",
                               "Classification Error"="error",
                               "AUC"="auc"),
                   selected = "logloss")
    }
  })  
  
  calculate<-eventReactive(input$Build,{
    req(data())
    df <- data()
    n <- nrow(df)
    RNGkind(sample.kind = "Rounding")
    set.seed(input$Seed)
    train <- sample(n,input$Trainsplit*n)
    form <- as.formula(paste0(input$Target,"~."))
    if(input$Models=="logistic"){
      model <- glm(form,data = df[train,],family = binomial)
      summ<-summary(model)
      coeftable<-as.data.frame(summ$coefficients[-1,])
      coeftable<-coeftable[coeftable$`Pr(>|z|)`<=input$significance,]
      accepted<-c(rownames(coeftable),input$Target)
      filtered_df<-df[,which(names(df)%in%accepted)]
      newmodel<-glm(form,data = filtered_df[train,],family = binomial)
      preds <- predict(newmodel,filtered_df[-train,],type="response")
    }
    else if(input$Models=="ridge"){
      x<-model.matrix(form,df[train,])[,-1]
      y<-df[train,which(names(df)%in%input$Target)]
      new.x<-model.matrix(form,df[-train,])[,-1]
      model<-glmnet::glmnet(x,y,alpha = 0,family = 'binomial',nlambda = input$lambdaA)
      cv<-glmnet::cv.glmnet(x,y,alpha=0, family = 'binomial')
      bestlamb<-cv$lambda.min
      preds<-predict(model,s=bestlamb, newx = new.x,type = 'response')
    }
    else if(input$Models=="lasso"){
      x<-model.matrix(form,df[train,])[,-1]
      y<-df[train,which(names(df)%in%input$Target)]
      new.x<-model.matrix(form,df[-train,])[,-1]
      model<-glmnet::glmnet(x,y,alpha = 1,family = 'binomial',nlambda = input$lambdaB)
      cv<-glmnet::cv.glmnet(x,y,alpha=1, family = 'binomial')
      bestlamb<-cv$lambda.min
      preds<-predict(model,s=bestlamb, newx = new.x,type = 'response')
    }
    else if(input$Models=="bestsubset"){
      model <- leaps::regsubsets(form,df[train,],nvmax=input$nvmax,method=input$bsmethod)
      summ<-summary(model)
      if(input$bsmetric=="bic"){
        b<-which.min(summ$bic)
      }
      else if(input$bsmetric=="aic"){
        b<-which.min(summ$aic)
      }
      else if(input$bsmetric=="adjr2"){
        b<-which.max(summ$adjr2)
      }
      coefs<-coef(model,b)[-1]
      predictor<-paste(names(coefs),collapse = "+")
      new_form<-as.formula(paste0(input$Target,"~",predictor))
      newmodel <- glm(new_form,df[train,],family = binomial)
      preds <- predict(newmodel,df[-train,],type="response")
    }
    else if(input$Models=="svm"){
      model <- e1071::svm(form,df[train,],type="C-classification",kernel=input$svmkernel,probability=T)
      preds <- attributes(predict(model,df[-train,],probability=T))$probabilities[,2]
    }
    else if(input$Models=="oneclasssvm"){
      filtered_df <- df[df[,which(names(df)%in%input$Target)]==1,]
      newtrain<-sample(nrow(filtered_df),input$Trainsplit*nrow(filtered_df))
      model <-e1071::svm(form,filtered_df[newtrain,],type="one-classification",kernel=input$oneclasskernel)
      PredictedLabels <- as.numeric(predict(model,df[-train,]))
      cm<-tibble::as_tibble(table(actual=df[-train,which(names(df) %in% input$Target)],
                                  predicted=PredictedLabels))
      acc<-round(sum(cm[c(1,4),3])/sum(cm[,3]),5)
      tpr<-round(unlist(cm[4,3]/sum(cm[c(2,4),3])),5)
      fpr<-round(unlist(cm[3,3]/sum(cm[c(1,3),3])),5)
      insert<-data.frame(Model=input$Models,Logloss=NA,AUC=NA,Accuracy=acc,TPR=tpr,FPR=fpr)
      return(list(cm=cm,insert=insert))
    }
    else if(input$Models=="decisiontree"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      model <- rpart::rpart(form,data = df,subset = train,parms = rpart::rpart.control(minsplit = input$minsplit,maxdepth = input$maxdepth))
      pruned <- rpart::prune(model,model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
      preds <- predict(pruned,df[-train,])[,2]
    }
    else if(input$Models=="randomforest"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      if(input$sampling=="T"){
        model <- randomForest::randomForest(form,df,subset=train,ntree=input$ntrees,mtry=input$mtry)
      }
      else if(input$sampling=="F"){
        model <- randomForest::randomForest(form,df,subset=train,ntree=input$ntrees,mtry=input$mtry,replace=F)
      }
      preds <- predict(model,df[-train,],type="prob")[,2]
    }
    else if(input$Models=="xgboost"){
      output.vector=as.numeric(df[train,which(names(df) %in% input$Target)]==1)
      output.vector.test=as.numeric(df[-train,which(names(df) %in% input$Target)]==1)
      dtrain=xgboost::xgb.DMatrix(Matrix::sparse.model.matrix(form,df)[train,-1],label=output.vector)
      dtest=xgboost::xgb.DMatrix(Matrix::sparse.model.matrix(form,df)[-train,-1],label=output.vector.test)
      parameter=list(eta=input$eta,max_depth=input$xgbmaxdepth,alpha=input$xgboostalpha,lambda=xgboostlambda)
      watchlist=list(train=dtrain,test=dtest)
      model <- xgboost::xgb.train(data=dtrain,nrounds=input$nrounds,early_stopping_rounds=input$earlystop,
                                  watchlist=watchlist,params = parameter,objective="binary:logistic",
                                  eval.metric = input$xgboostmetrics,verbose=0)
      preds <- predict(model,dtest)
    }
    else if(input$Models=="adaboost"){
      x<-as.matrix(df[,-which(names(df) %in% input$Target)])
      y_train<-as.numeric(df[train,which(names(df) %in% input$Target)]==1)
      y_train<-ifelse(y_train==0,-1,y_train)
      model <-JOUSBoost::adaboost(x[train,],y_train,n_rounds = input$adanrounds,tree_depth = input$adamaxdepth)
      preds <- predict(model,x[-train,],type="prob")
    }
    else if(input$Models=="isolationforest"){
      model=solitude::isolationForest$new(sample_size = length(train))
      model$fit(dataset=df[train,-which(names(df) %in% input$Target)])
      preds<-unlist(model$predict(df[-train,-which(names(df) %in% input$Target)])[,3])
      rate<-1-input$ratio
      PredictedLabels<-ifelse(preds>quantile(preds,rate),1,0)
      cm <- tibble::as_tibble(table(actual=df[-train,which(names(df) %in% input$Target)],
                                    predicted=PredictedLabels))
      acc<-round(sum(cm[c(1,4),3])/sum(cm[,3]),5)
      tpr<-round(unlist(cm[4,3]/sum(cm[c(2,4),3])),5)
      fpr<-round(unlist(cm[3,3]/sum(cm[c(1,3),3])),5)
      insert<-data.frame(Model=input$Models,Logloss=NA,AUC=NA,Accuracy=acc,TPR=tpr,FPR=fpr)
      return(list(cm=cm,insert=insert))
    }
    else if(input$Models=="knn"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      tuner<-e1071::tune.knn(df[train,-which(names(df) %in% input$Target)],df[train,which(names(df) %in% input$Target)],k=1:input$neighbours,tunecontrol=e1071::tune.control(sampling=input$knnsample),cross=10)
      model<-class::knn(df[train,-which(names(df) %in% input$Target)],df[-train,-which(names(df) %in% input$Target)],df[train,which(names(df) %in% input$Target)],k=tuner$best.parameter,prob = T)
      preds<-attributes(model)$prob
    }
    else if(input$Models=="naivebayes"){
      model <- e1071::naiveBayes(form,df[train,])
      preds <- predict(model,df[-train,],type="raw")[,2]
    }
    else if(input$Models=="rulebased"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      model<-CBA(form,df[train,],parameter = list(support=input$support,confidence=input$confidence))
      PredictedLabels<-predict(model,df[-train,])
      cm <- tibble::as_tibble(table(actual=df[-train,which(names(df) %in% input$Target)],
                                    predicted=PredictedLabels))
      acc<-round(sum(cm[c(1,4),3])/sum(cm[,3]),5)
      tpr<-round(unlist(cm[4,3]/sum(cm[c(2,4),3])),5)
      fpr<-round(unlist(cm[3,3]/sum(cm[c(1,3),3])),5)
      insert<-data.frame(Model=input$Models,Logloss=NA,AUC=NA,Accuracy=acc,TPR=tpr,FPR=fpr)
      return(list(cm=cm,insert=insert))
    }
    p <- ROCR::prediction(preds,df[-train,which(names(df) %in% input$Target)])
    acc.perf = ROCR::performance(p, measure = "acc")
    ind = which.max(slot(acc.perf, "y.values")[[1]])
    cutoff = slot(acc.perf, "x.values")[[1]][ind]
    PredictedLabels<-ifelse(preds>cutoff,1,0)
    cm <- tibble::as_tibble(table(actual=df[-train,which(names(df) %in% input$Target)],
                                  predicted=PredictedLabels))
    logloss.perf=ROCR::performance(p,measure = "mxe")
    auc.perf=ROCR::performance(p,measure = "auc")
    roc.perf = ROCR::performance(p, measure = "tpr", x.measure = "fpr")
    logloss=round(slot(logloss.perf,"y.values")[[1]],5)
    auc=round(slot(auc.perf,"y.values")[[1]],5)
    acc<-round(sum(cm[c(1,4),3])/sum(cm[,3]),5)
    tpr<-round(unlist(cm[4,3]/sum(cm[c(2,4),3])),5)
    fpr<-round(unlist(cm[3,3]/sum(cm[c(1,3),3])),5)
    insert<-data.frame(Model=input$Models,Logloss=logloss,AUC=auc,Accuracy=acc,TPR=tpr,FPR=fpr)
    return(list(cm=cm,insert=insert,roc=roc.perf))
  })
  output$ConfusionMatrix<-renderPlot({
    cvms::plot_confusion_matrix(calculate()$cm,target_col = "actual",
                                prediction_col = "predicted",
                                counts_col = "n")
  })
  output$ROC<-renderPlot({
    createplot=try(plot(calculate()$roc,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1)),silent=T)
    if(class(createplot)!="try-error"){
      plot(calculate()$roc,colorize=TRUE,print.cutoffs.at=seq(0,1,by=0.1))
      abline(a=0, b= 1)
    }
  })
  
  #render the model leaderboard which updates based on models built
  output$leaderboard <- DT::renderDataTable({
    return(updated()%>%dplyr::arrange(desc(Accuracy)))
  })
  
  #make new predictions and preview new predictions
  calculatenewpreds<-eventReactive(input$newpreds,{
    req(data())
    req(input$newdata)
    df <- data()
    new_data<-read.csv(input$newdata$datapath,
                       header = input$header,
                       sep = input$sep)
    form <- as.formula(paste0(input$Target,"~."))
    if(input$Models=="logistic"){
      model <- glm(form,data = df,family = binomial)
      summ<-summary(model)
      coeftable<-as.data.frame(summ$coefficients[-1,])
      coeftable<-coeftable[coeftable$`Pr(>|z|)`<=input$significance,]
      accepted<-c(rownames(coeftable),input$Target)
      filtered_df<-df[,which(names(df)%in%accepted)]
      newmodel<-glm(form,data = filtered_df,family = binomial)
      preds <- predict(newmodel,new_data,type="response")
    }
    else if(input$Models=="ridge"){
      x<-model.matrix(form,df)[,-1]
      y<-df[,which(names(df)%in%input$Target)]
      new.x<-model.matrix(form,new_data)[,-1]
      model<-glmnet::glmnet(x,y,alpha = 0,family = 'binomial',nlambda = input$lambdaA)
      cv<-glmnet::cv.glmnet(x,y,alpha=0, family = 'binomial')
      bestlamb<-cv$lambda.min
      preds<-predict(model,s=bestlamb, newx = new.x,type = 'response')
    }
    else if(input$Models=="lasso"){
      x<-model.matrix(form,df)[,-1]
      y<-df[,which(names(df)%in%input$Target)]
      new.x<-model.matrix(form,new_data)[,-1]
      model<-glmnet::glmnet(x,y,alpha = 1,family = 'binomial',nlambda = input$lambdaB)
      cv<-glmnet::cv.glmnet(x,y,alpha=1, family = 'binomial')
      bestlamb<-cv$lambda.min
      preds<-predict(model,s=bestlamb, newx = new.x,type = 'response')
    }
    else if(input$Models=="bestsubset"){
      model <- leaps::regsubsets(form,df,nvmax=input$nvmax,method=input$bsmethod)
      summ<-summary(model)
      if(input$bsmetric=="bic"){
        b<-which.min(summ$bic)
      }
      else if(input$bsmetric=="aic"){
        b<-which.min(summ$aic)
      }
      else if(input$bsmetric=="adjr2"){
        b<-which.max(summ$adjr2)
      }
      coefs<-coef(model,b)[-1]
      predictor<-paste(names(coefs),collapse = "+")
      new_form<-as.formula(paste0(input$Target,"~",predictor))
      newmodel <- glm(new_form,df,family = binomial)
      preds <- predict(newmodel,new_data,type="response")
    }
    else if(input$Models=="decisiontree"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      model <- rpart::rpart(form,data = df,parms = rpart::rpart.control(minsplit = input$minsplit,maxdepth = input$maxdepth))
      pruned <- rpart::prune(model,model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
      preds <- predict(pruned,new_data)[,2]
    }
    else if(input$Models=="randomforest"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      if(input$sampling=="T"){
        model <- randomForest::randomForest(form,df,ntree=input$ntrees,mtry=input$mtry)
      }
      else if(input$sampling=="F"){
        model <- randomForest::randomForest(form,df,ntree=input$ntrees,mtry=input$mtry,replace=F)
      }
      preds <- predict(model,new_data,type="prob")[,2]
    }
    else if(input$Models=="xgboost"){
      output.vector=as.numeric(df[,which(names(df) %in% input$Target)]==1)
      dtrain=xgboost::xgb.DMatrix(Matrix::sparse.model.matrix(form,df)[,-1],label=output.vector)
      dtest=xgboost::xgb.DMatrix(Matrix::sparse.model.matrix(form,df)[,-1])
      parameter=list(eta=input$eta,max_depth=input$xgbmaxdepth,alpha=input$xgboostalpha,lambda=xgboostlambda)
      watchlist=list(train=dtrain,test=dtest)
      model <- xgboost::xgb.train(data=dtrain,nrounds=input$nrounds,early_stopping_rounds=input$earlystop,
                                  watchlist=watchlist,params = parameter,
                                  eval.metric=input$xgboostmetrics,objective="binary:logistic",verbose=0)
      preds <- predict(model,dtest)
    }
    else if(input$Models=="adaboost"){
      x<-as.matrix(df[,-which(names(df) %in% input$Target)])
      y<-as.numeric(df[,which(names(df) %in% input$Target)]==1)
      y<-ifelse(y==0,-1,y)
      new_data<-as.matrix(new_data[,-which(names(df) %in% input$Target)])
      model <-JOUSBoost::adaboost(x,y,n_rounds = input$adanrounds,tree_depth = input$adamaxdepth)
      preds <- predict(model,new_data,type="prob")
    }
    else if(input$Models=="isolationforest"){
      model=solitude::isolationForest$new(sample_size = nrow(df))
      model$fit(dataset=df[,-which(names(df) %in% input$Target)])
      preds<-unlist(model$predict(new_data[,-which(names(df) %in% input$Target)])[,3])
      rate<-1-input$ratio
      new_data$PredictedLabels<-ifelse(preds>quantile(preds,rate),1,0)
      return(new_data)
    }
    else if(input$Models=="svm"){
      model <- e1071::svm(form,df[train,],type="C-classification",kernel=input$svmkernel,probability=T)
      preds <- attributes(predict(model,df[-train,],probability=T))$probabilities[,2]
    }
    else if(input$Models=="oneclasssvm"){
      filtered_df <- df[df[,which(names(df)%in%input$Target)]==1,]
      model <-e1071::svm(form,filtered_df,type="one-classification",kernel=input$oneclasskernel)
      new_data$PredictedLabels <- as.numeric(predict(model,new_data))
      return(new_data)
    }
    else if(input$Models=="knn"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      tuner<-e1071::tune.knn(df[,-which(names(df) %in% input$Target)],df[,which(names(df) %in% input$Target)],k=1:input$neighbours,tunecontrol=e1071::tune.control(sampling=input$knnsample),cross=10)
      model<-class::knn(df[,-which(names(df) %in% input$Target)],new_data[,-which(names(df) %in% input$Target)],df[,which(names(df) %in% input$Target)],k=tuner$best.parameter,prob = T)
      preds<-attributes(model)$prob
    }
    else if(input$Models=="naivebayes"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      model <- e1071::naiveBayes(form,df)
      preds <- predict(model,new_data,"raw")[,2]
    }
    else if(input$Models=="rulebased"){
      df[,which(names(df) %in% input$Target)] <- as.factor(df[,which(names(df) %in% input$Target)])
      new_data[,which(names(df) %in% input$Target)] <- as.factor(new_data[,which(names(df) %in% input$Target)])
      model<-CBA(form,df,parameter = list(support=input$support,confidence=input$confidence))
      new_data$PredictedLabels<-predict(model,new_data)
      return(new_data)
    }
    p <- ROCR::prediction(preds,df[,which(names(df) %in% input$Target)])
    acc.perf = ROCR::performance(p, measure = "acc")
    ind = which.max(slot(acc.perf, "y.values")[[1]])
    cutoff = slot(acc.perf, "x.values")[[1]][ind]
    new_data$PredictedLabels<-ifelse(preds>cutoff,1,0)
    return(new_data)
  })
  output$newpreview <- DT::renderDataTable({
    req(calculatenewpreds())
    return(calculatenewpreds())
  })
  output$downloadPreds <- downloadHandler(
    filename = "new_predictions.csv",
    content = function(file) {
      write.csv(calculatenewpreds(), file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui,server)
  

library(rhdf5)
library(dplyr)
library(lda)
library(magrittr)
library(randomForest)
library(xgboost)
library(e1071)
setwd("~/Desktop/哥大/fall 2016/5243 ads/project4/Project4_data")

###load data
load("lyr.RData")
head(lyr)
Filenames = list.files(path = "~/Desktop/哥大/fall 2016/5243 ads/project4/Project4_data/data",all.files = TRUE, recursive = TRUE )
i=1
sound<-list()
Filenames2=grep('.h5', Filenames, value=TRUE)
setwd("~/Desktop/哥大/fall 2016/5243 ads/project4/Project4_data/data")
for (i in 1:length(Filenames2))
{
  sound[[i]]<-h5read(Filenames2[i], "/analysis")
  i=i+1
}

###Topic Modeling
lyr_data= lyr[,-1]
vocab= colnames(lyr)[-1]
n.train=2350
word.list= list()
for(i in 1:n.train){
  word.list[[i]]= rep(colnames(lyr_data), lyr_data[i,])
}

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index), as.integer(rep(1, length(index))))
}

doc <- lapply(word.list, get.terms)

################ fit the LDA
topic_number <- 20
iteration_number <- 100
alpha <- 0.02
eta <- 0.02
library(lda)
set.seed(327)
#t1 <- Sys.time()
vocab2= colnames(lyr)
words.lda <- lda.collapsed.gibbs.sampler(documents = doc, K = topic_number, vocab = vocab2, 
                                         num.iterations = iteration_number, alpha = alpha, 
                                         eta = eta, initial = NULL, burnin = 0,
                                         compute.log.likelihood = TRUE)
#t2 <- Sys.time()
theta <- t(apply(words.lda$document_sums + alpha, 2, function(x) x/sum(x)))  # dim: 2350*20
#the possibility of falling into a certain topic
#topic_indicator <- apply(theta,1,order) %>%t()# %>% is_greater_than(15) %>% as.numeric() %>% matrix(n.train,20)
y= theta
y <- ifelse(y<0.1,0,1)
y_train= ifelse(theta<0.1,0,1)
words.prob <- t(apply(t(words.lda$topics) + eta, 2, function(x) x/sum(x)))[,-1]
###8 loudness max
features8=matrix(nrow=2350,ncol=90)
k=1
index8=array(dim=9)
for (k in 1:length(sound)){
  length8_k=length(sound[[k]]$segments_loudness_max)
  a=1
  b=1
  for (a in 2:10){index8[a-1]=floor(length8_k/a)
  a=a+1}
  if (length8_k>=90){
    for (b in 1:9) {features8[k,(10*(b-1)+1):(10*b)]=sound[[k]]$segments_loudness_max[(index8[b]-4):(index8[b]+5)]
    b=b+1}
  }
  
  else if (length8_k<90 && 90%%length8_k!=0){
    features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(90/length8_k)),sound[[k]]$segments_loudness_max[1:(90-floor(90/length8_k)*length8_k)])
    
  }
  else if (length8_k<90 && 90%%length8_k!=0){
    features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(90/length8_k)))
  }
  
  k=k+1
}

###9 segments_loudness_max_time
features9=matrix(nrow=2350,ncol=90)
p=1
index9=array(dim=9)
for (k in 1:length(sound)){
  length9_p=length(sound[[p]]$segments_loudness_max_time)
  a=1
  b=1
  for (a in 2:10){index9[a-1]=floor(length9_p/a)
  a=a+1}
  if (length9_p>=90){
    for (b in 1:9) {features9[p,(10*(b-1)+1):(10*b)]=sound[[p]]$segments_loudness_max_time[(index9[b]-4):(index9[b]+5)]
    b=b+1}
  }
  
  else if (length9_p<90 && 90%%length9_p!=0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(90/length9_p)),sound[[p]]$segments_loudness_max_time[1:(90-floor(90/length9_p)*length9_p)])
    
  }
  else if (length9_p<90 && 90%%length9_p!=0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(90/length9_p)))
  }
  
  p=p+1
}


###11 segments_pitches
features11=matrix(nrow=2350,ncol=1080)
m=1
index11=array(dim=9)
for (m in 1:length(sound)){
  length11_m=ncol(sound[[m]]$segments_pitches)
  a=1
  b=1
  for (a in 2:10){ index11[a-1]=floor(length11_m/a)
  a=a+1}
  if (length11_m>=90){
    for (b in 1:9) {features11[m,(120*(b-1)+1):(120*b)]=as.vector(sound[[m]]$segments_pitches[,(index11[b]-4):(index11[b]+5)])
    b=b+1
    }
  }
  
  else if (length11_m<90 && 90%%length11_m!=0){
    features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(90/length11_m)),as.vector(sound[[m]]$segments_pitches[,1:(90-floor(90/length11_m)*length11_m)]))
    
  }
  else if (length11_m<90 && 90%%length11_m==0){
    features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(90/length11_m)))
  }
  
  m=m+1
}

###13 segments_timbre
features13=matrix(nrow=2350,ncol=1080)
index13=array(dim=9)
q=1
for (m in 1:length(sound)){
  length13_q=ncol(sound[[q]]$segments_timbre)
  a=1
  b=1
  for (a in 2:10){index13[a-1]=floor(length13_q/a)
  a=a+1}
  if (length13_q>=90){
    for (b in 1:9) {features13[q,(120*(b-1)+1):(120*b)]=as.vector(sound[[q]]$segments_timbre[,(index13[b]-4):(index13[b]+5)])
    b=b+1
    }
  }
  
  else if (length13_q<90 && 90%%length13_q!=0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(90/length13_q)),as.vector(sound[[q]]$segments_timbre[,1:(90-floor(90/length13_q)*length13_q)]))
    
  }
  else if (length13_q<90 && 90%%length13_q==0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(90/length13_q)))
  }
  
  q=q+1
}

features_num<-cbind(features8,features9,features11,features13)
features= as.data.frame(features_num)
features[is.na(features)]=0
features_train= features
######Get test features
Filenames_test = list.files(path = "~/Desktop/哥大/fall 2016/5243 ads/project4/Project4_data/TestSongFile100",all.files = TRUE, recursive = TRUE )
i_test=1
sound_test<-list()
Filenames2_test=grep('.h5', Filenames_test, value=TRUE)
setwd("~/Desktop/哥大/fall 2016/5243 ads/project4/Project4_data/TestSongFile100")
for (i_test in 1:length(Filenames2_test))
{
  sound_test[[i_test]]<-h5read(Filenames2_test[i_test], "/analysis")
  i_test=i_test+1
}

sound=sound_test

###8 loudness max
features8=matrix(nrow=100,ncol=90)
k=1
index8=array(dim=9)
for (k in 1:length(sound)){
  length8_k=length(sound[[k]]$segments_loudness_max)
  a=1
  b=1
  for (a in 2:10){index8[a-1]=floor(length8_k/a)
  a=a+1}
  if (length8_k>=90){
    for (b in 1:9) {features8[k,(10*(b-1)+1):(10*b)]=sound[[k]]$segments_loudness_max[(index8[b]-4):(index8[b]+5)]
    b=b+1}
  }
  
  else if (length8_k<90 && 90%%length8_k!=0){
    features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(90/length8_k)),sound[[k]]$segments_loudness_max[1:(90-floor(90/length8_k)*length8_k)])
    
  }
  else if (length8_k<90 && 90%%length8_k!=0){
    features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(90/length8_k)))
  }
  
  k=k+1
}

###9 segments_loudness_max_time
features9=matrix(nrow=100,ncol=90)
p=1
index9=array(dim=9)
for (k in 1:length(sound)){
  length9_p=length(sound[[p]]$segments_loudness_max_time)
  a=1
  b=1
  for (a in 2:10){index9[a-1]=floor(length9_p/a)
  a=a+1}
  if (length9_p>=90){
    for (b in 1:9) {features9[p,(10*(b-1)+1):(10*b)]=sound[[p]]$segments_loudness_max_time[(index9[b]-4):(index9[b]+5)]
    b=b+1}
  }
  
  else if (length9_p<90 && 90%%length9_p!=0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(90/length9_p)),sound[[p]]$segments_loudness_max_time[1:(90-floor(90/length9_p)*length9_p)])
    
  }
  else if (length9_p<90 && 90%%length9_p!=0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(90/length9_p)))
  }
  
  p=p+1
}


###11 segments_pitches
features11=matrix(nrow=100,ncol=1080)
m=1
index11=array(dim=9)
for (m in 1:length(sound)){
  length11_m=ncol(sound[[m]]$segments_pitches)
  a=1
  b=1
  for (a in 2:10){ index11[a-1]=floor(length11_m/a)
  a=a+1}
  if (length11_m>=90){
    for (b in 1:9) {features11[m,(120*(b-1)+1):(120*b)]=as.vector(sound[[m]]$segments_pitches[,(index11[b]-4):(index11[b]+5)])
    b=b+1
    }
  }
  
  else if (length11_m<90 && 90%%length11_m!=0){
    features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(90/length11_m)),as.vector(sound[[m]]$segments_pitches[,1:(90-floor(90/length11_m)*length11_m)]))
    
  }
  else if (length11_m<90 && 90%%length11_m==0){
    features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(90/length11_m)))
  }
  
  m=m+1
}

###13 segments_timbre
features13=matrix(nrow=100,ncol=1080)
index13=array(dim=9)
q=1
for (m in 1:length(sound)){
  length13_q=ncol(sound[[q]]$segments_timbre)
  a=1
  b=1
  for (a in 2:10){index13[a-1]=floor(length13_q/a)
  a=a+1}
  if (length13_q>=90){
    for (b in 1:9) {features13[q,(120*(b-1)+1):(120*b)]=as.vector(sound[[q]]$segments_timbre[,(index13[b]-4):(index13[b]+5)])
    b=b+1
    }
  }
  
  else if (length13_q<90 && 90%%length13_q!=0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(90/length13_q)),as.vector(sound[[q]]$segments_timbre[,1:(90-floor(90/length13_q)*length13_q)]))
    
  }
  else if (length13_q<90 && 90%%length13_q==0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(90/length13_q)))
  }
  
  q=q+1
}
features_num_test<-cbind(features8,features9,features11,features13)
features_test= features_num_test
param <- list(objective = "binary:logistic", max_depth = 7,eta = 0.13, gamma = 0.1)
y_pre= vector()
i=1
for(i in 1:20){
  dtrain <- xgb.DMatrix(as.matrix(features_train),label = y_train[,i])
  xg.fit <- xgboost(data=dtrain, params=param, nrounds=20, nthread=6)
  pre= predict(xg.fit, as.matrix(features_test))
  y_pre= cbind(y_pre, pre)
}
# Fit predict probability of 5000 word in the test songs
rank.words_test= matrix(0, ncol=5000)
i=1
for(i in 1:100){
  rank.words_test= rbind(rank.words_test,colSums(y_pre[i,]*words.prob))
}
rank.words_test= rank.words_test[-1,]
# Add prior probability
prior= colSums(lyr_data)/sum(lyr_data)
prior_mat= matrix(rep(prior,(100)),nrow= (100),byrow=T)
dim(prior_mat)
rank.words= prior_mat* rank.words_test
rank_col=colnames(rank.words)
result2<-apply(rank.words,1,function(x) match(x, sort(x,decreasing = T)))
result3<-t(result2)
colnames(result3)=rank_col
result4=cbind(Filenames2_test,result3)
write.csv(result4,"result.csv")
temp_sum=0
i=1
lyr_test= lyr[,-1]
lyr_test= lyr_test !=0
result= apply(lyr_test, 1, function(x) which(x !=0))
for(i in 1:100){
  temp= 5001-rank(rank.words_test[i,])
  temp_sum= temp_sum+sum(temp[result[[i]]])/length(result[[i]])
}
temp_sum/100

#####check
result3[2,which(result3[2,]==23)]######find the second song 23 rank
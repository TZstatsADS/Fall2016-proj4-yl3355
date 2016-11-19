######ADS PROJ 4 LYRICS
######Youzhu Liu
####classification of music:naive bayes/lasso/kmeans/cluster/know the features of new song and catergorithm )\
###topic k
####tidy_text_r/word2vecg/ldavis

#source("http://bioconductor.org/biocLite.R")
#biocLite("rhdf5")
library(rhdf5)
library(dplyr)
library(lda)
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
lyr_data=lyr[,-1]

vocab= colnames(lyr)[-1]
n.train=2350
word.list= list()
t1= Sys.time()
for(i in 1:n.train){
  word.list[[i]]= rep(colnames(lyr_data), lyr_data[i,])
}
t2= Sys.time()
t2-t1

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
words.prob <- t(apply(t(words.lda$topics) + eta, 2, function(x) x/sum(x)))[,-1]

###Feature sampling 
###4 6 8 9 10 11 12 13 
###get min value 
j=1
min_bs=length(sound[[1]]$beats_start)
for (j in 1:length(sound)){
  min_bs=min(min_bs,length(sound[[j]]$beats_start))
  j=j+1
}


####start type 求间隔的平均，间隔的方差，间隔的max min
####8 9 11 13
###8 loudness max
features8=matrix(nrow=2350,ncol=90)
k=1
a=1
for (a in 1:10){index8[a]=floor(length8_k/a)}

for (k in 1:length(sound)){
  length8_k=length(sound[[k]]$segments_loudness_max)
   
  if (length8_k>=30 && length8_k%%2==0){
    features8[k,]=c(sound[[k]]$segments_loudness_max[1:10],sound[[k]]$segments_loudness_max[(length8_k/2-4):(length8_k/2+5)],sound[[k]]$segments_loudness_max[(length8_k-9):length8_k])
  }
   else if (length8_k>=30 && length8_k%%2!=0) {
     features8[k,]=c(sound[[k]]$segments_loudness_max[1:10],sound[[k]]$segments_loudness_max[((length8_k-1)/2-4):((length8_k-1)/2+5)],sound[[k]]$segments_loudness_max[(length8_k-9):length8_k])
   }
     else if (length8_k<30 && 30%%length8_k!=0){
       features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(30/length8_k)),sound[[k]]$segments_loudness_max[1:(30-floor(30/length8_k)*length8_k)])
       
     }
        else if (length8_k<30 && 30%%length8_k==0){
          features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(30/length8_k)))
        }
       
  k=k+1
}

###9 segments_loudness_max_time
features9=matrix(nrow=2350,ncol=30)
p=1
for (p in 1:length(sound)){
  length9_p=length(sound[[p]]$segments_loudness_max_time)
  if (length9_p>=30 && length9_p%%2==0){
    features9[p,]=c(sound[[p]]$segments_loudness_max_time[1:10],sound[[p]]$segments_loudness_max_time[(length9_p/2-4):(length9_p/2+5)],sound[[p]]$segments_loudness_max_time[(length9_p-9):length9_p])
  }
  else if (length9_p>=30 && length9_p%%2!=0) {
    features9[p,]=c(sound[[p]]$segments_loudness_max_time[1:10],sound[[p]]$segments_loudness_max_time[((length9_p-1)/2-4):((length9_p-1)/2+5)],sound[[p]]$segments_loudness_max_time[(length9_p-9):length9_p])
  }
  else if (length9_p<30 && 30%%length9_p!=0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(30/length9_p)),sound[[p]]$segments_loudness_max_time[1:(30-floor(30/length9_p)*length9_p)])
    
  }
  else if (length9_p<30 && 30%%length9_p==0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(30/length9_p)))
  }
  
  p=p+1
}


###11 segments_pitches
features11=matrix(nrow=2350,ncol=360)
m=1
for (m in 1:length(sound)){
  length11_m=ncol(sound[[m]]$segments_pitches)
  if (length11_m>=30 && length11_m%%2==0){
    features11[m,]=c(as.vector(sound[[m]]$segments_pitches[,1:10]),as.vector(sound[[m]]$segments_pitches[,(length11_m/2-4):(length11_m/2+5)]),as.vector(sound[[m]]$segments_pitches[,(length11_m-9):length11_m]))
  }
   else if (length11_m>=30 && length11_m%%2!=0) {
     features11[m,]=c(as.vector(sound[[m]]$segments_pitches[,1:10]),as.vector(sound[[m]]$segments_pitches[,((length11_m-1)/2-4):((length11_m-1)/2+5)]),as.vector(sound[[m]]$segments_pitches[,(length11_m-9):length11_m]))
   }
      else if (length11_m<30 && 30%%length11_m!=0){
        features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(30/length11_m)),as.vector(sound[[m]]$segments_pitches[,1:(30-floor(30/length11_m)*length11_m)]))
        
      }
         else if (length11_m<30 && 30%%length11_m==0){
           features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(30/length11_m)))
         }
  
  m=m+1
}

###13 segments_timbre
features13=matrix(nrow=2350,ncol=360)
q=1
for (q in 1:length(sound)){
  length13_q=ncol(sound[[q]]$segments_timbre)
  if (length13_q>=30 && length13_q%%2==0){
    features13[q,]=c(as.vector(sound[[q]]$segments_timbre[,1:10]),as.vector(sound[[q]]$segments_timbre[,(length13_q/2-4):(length13_q/2+5)]),as.vector(sound[[q]]$segments_timbre[,(length13_q-9):length13_q]))
  }
  else if (length13_q>=30 && length13_q%%2!=0) {
    features13[q,]=c(as.vector(sound[[q]]$segments_timbre[,1:10]),as.vector(sound[[q]]$segments_timbre[,((length13_q-1)/2-4):((length13_q-1)/2+5)]),as.vector(sound[[q]]$segments_timbre[,(length13_q-9):length13_q]))
  }
  else if (length13_q<30 && 30%%length13_q!=0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(30/length13_q)),as.vector(sound[[q]]$segments_timbre[,1:(30-floor(30/length13_q)*length13_q)]))
    
  }
  else if (length13_q<30 && 30%%length13_q==0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(30/length13_q)))
  }
  
  q=q+1
}

features_num<-cbind(features8,features9,features11,features13)


#####10% 20% 
###model fitting

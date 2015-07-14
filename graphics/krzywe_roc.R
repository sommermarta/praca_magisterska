library("ggplot2")

u <- read.table("http://www.ipipan.eu/~teisseyrep/TEACHING/DM/DANE/urine.txt",header=TRUE)

# standardowa krzywa roc, dwie klasy

siatka <- seq(0,1,length.out=100)
l <- glm(presence~.,data=u,family="binomial")
tpr1 <- numeric(100)
fpr1 <- numeric(100)
k <- 1

for(i in siatka){
  
  l_pred <- ifelse(predict(l,newdata=u,type="response")>i,"yes","no")
  t <- table(u$presence,l_pred)
  if(all(l_pred=="no")) t <- cbind(t,c(0,0))
  if(all(l_pred=="yes")) t <- cbind(c(0,0),t)
  
  tpr1[k] <- t[2,2]/(t[2,2]+t[2,1])
  fpr1[k] <- 1 - t[1,1]/(t[1,1]+t[1,2])
  k <- k+1
  
}

dane <- data.frame(fpr=sort(fpr1), tpr=sort(tpr1))

tpr2 <- rep(dane$tpr, each=2)
tpr2 <- tpr2[-length(tpr2)]

fpr2 <- rep(dane$fpr, each=2)
fpr2 <- fpr2[-1]

dane <- data.frame(fpr=sort(fpr2), tpr=sort(tpr2))
dane <- rbind(dane, c(1,0))

ggplot(dane, aes(x=fpr, y=tpr))+
  geom_step(size=1)+
  geom_polygon(fill="red", alpha=0.5)+
  theme_bw()+
  xlab("FPR")+
  ylab("TPR")+
  theme(axis.text = element_text(size=14, family="serif"),
        axis.title = element_text(size=20, family="serif"))

# ta druga krzywa roc, dwie klasy

siatka <- seq(0,1,length.out=100)
l <- glm(presence~.,data=u,family="binomial")
tpr1 <- numeric(100)
fpr1 <- numeric(100)
k <- 1

for(i in siatka){
  
  l_pred <- ifelse(predict(l,newdata=u,type="response")>i,"yes","no")
  t <- table(u$presence,l_pred)
  if(all(l_pred=="no")) t <- cbind(t,c(0,0))
  if(all(l_pred=="yes")) t <- cbind(c(0,0),t)
  
  tpr1[k] <- t[2,2]/(t[2,2]+t[2,1])
  fpr1[k] <- t[1,1]/(t[1,1]+t[1,2])
  k <- k+1
  
}

dane <- data.frame(fpr=sort(fpr1), tpr=sort(tpr1, decreasing=TRUE))

tpr2 <- rep(dane$tpr, each=2)
tpr2 <- tpr2[-1]

fpr2 <- rep(dane$fpr, each=2)
fpr2 <- fpr2[-length(fpr2)]

dane <- data.frame(fpr=sort(fpr2), tpr=sort(tpr2, decreasing=TRUE))
dane <- rbind(c(0,0),dane)

ggplot(dane, aes(x=fpr, y=tpr))+
  geom_step(size=1)+
  geom_polygon(fill="red", alpha=0.5)+
  theme_bw()+
  xlab("TNR")+
  ylab("TPR")+
  theme(axis.text = element_text(size=14, family="serif"),
        axis.title = element_text(size=20, family="serif"))




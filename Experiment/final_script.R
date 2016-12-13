library(MASS)
library(dplyr)

# Start the clock!
ptm <- proc.time()
i=0

#Generating the list of variables
varList=list()
varPairs <- NULL

#Generating Data
mySig<-matrix(.5,nrow=20,ncol=20)
diag(mySig)=1

y<-data.frame(mvrnorm(n=40, mu=rep(50,times=20), Sigma=mySig))
print(paste0("Initial missing: ",max(apply(y,2,function(c) sum(is.na(c))))))

#Imposing Missing values
y[y>50]<-NA
print(paste0("Imposed missing MAX: ",max(apply(y,2,function(c) sum(is.na(c))))))
naCount<-apply(y,2,function(c) sum(is.na(c)))
naCount<- as.data.frame(naCount)
naCount<- add_rownames(naCount,"Var1")

#Function for na counts
calc_na_counts <- function(c) { return(sum(is.na(y[,c])))}


#Genearting pair of variables by taking 2 ata a time
varList<- colnames(y)
varPairs <- t(combn(varList,2))
varPairs<- varPairs[1:100,]


#remove the first variable
while(length(varPairs)>0){
  
  
  s<- table(unlist(varPairs))
  s<- as.data.frame(s)
  # s<- arrange(s,desc(Freq))
  t<- s[which(s$Freq==max(s$Freq)),]
  
  
  
  #If we have same max values for 2 variables then check no of NA's
     if(diff(t$Freq)==0 && nrow(t)>1){
      
      print(paste0("Max values are equal"))
      print(paste0("Check the max values for NA count"))
      m<-max(sapply(as.character(t$Var1),FUN=calc_na_counts))
      a<-naCount[which(naCount$naCount == m),]
      a<-a[which(a$Var1 %in% as.character(t$Var1)),]
      
      if(diff(a$naCount)==0 && nrow(a)>1){
        
        print(paste0("a value equal"))
        a<- a[1,]
        a<- as.character(a$Var1)
        s<- s[-which(s$Var1 == a),]
        varPairs<- subset(varPairs,varPairs[,1]!=a)
        
      } else if(nrow(a)<2){
        print(paste0("a value not equal"))
        a<- as.character(a$Var1)
        s<- s[-which(s$Var1 == a),]
        varPairs<- subset(varPairs,varPairs[,1]!=a)
        
      }
      
      
      }else
      print(paste0("else entered"))
      t<- as.character(t$Var1)
      s<- s[-which(s$Var1 == t),]
      varPairs<- subset(varPairs,varPairs[,1]!=t)
      
    
    
}
  
  




# Stop the clock
proc.time() - ptm
fileName="~/stop words testing/Learning algo/testhindi.txt"
conn=file(fileName,open="r")
linn=readLines(conn)
#linn<-gsub("[","/[",linn)
close(conn)

fileName="~/stop words testing/Learning algo/DomainWithOR.txt"
conn1=file(fileName,open="r")
domains=readLines(conn1)
hindistop<-read.table("~/stop words testing/hindistopwords.txt",fill=TRUE,quote = "")

# ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~  
domains<-gsub("\\[","\\\\[",domains)
domains<-gsub("\\]","\\\\]",domains)
domains<-gsub("\\{","\\\\{",domains)
domains<-gsub("\\}","\\\\}",domains)
domains<-gsub("\\.","\\\\.",domains)
domains<-gsub("\\?","\\\\?",domains)
domains<-gsub("\\!","\\\\!",domains)
#domains<-gsub("\\"","\\\\"",domains)
domains<-gsub("\\$","\\\\$",domains)
domains<-gsub("\\&","\\\\&",domains)
domains<-gsub("\\'","\\\\'",domains)
domains<-gsub("\\(","\\\\(",domains)
domains<-gsub("\\)","\\\\)",domains)
domains<-gsub("\\*","\\\\*",domains)
domains<-gsub("\\+","\\\\+",domains)
domains<-gsub("\\-","\\\\-",domains)
domains<-gsub("\\/","\\\\/",domains)
domains<-gsub("\\:","\\\\:",domains)
domains<-gsub("\\;","\\\\;",domains)
domains<-gsub("<","<",domains)
domains<-gsub("\\=","\\\\=",domains)
domains<-gsub(">",">",domains)
domains<-gsub("\\@","\\\\@",domains)
#domains<-gsub("\\\","\\\\\\",domains)
domains<-gsub("\\^","\\\\^",domains)
domains<-gsub("\\`","\\\\`",domains)
domains<-gsub("\\_","\\\\_",domains)
domains<-gsub("\\~","\\\\~",domains)
close(conn1)
file.create("~/stop words testing//Learning algo/Domainstesthindi.txt")
file.create("~/stop words testing//Learning algo/temp.txt")

for (i in 1:length(linn))
{
  hindicorpus<-Corpus(VectorSource(linn[i]))
  # hindicorpus<-tm_map(hindicorpus,content_transformer(tolower))  
  hindicorpus<-tm_map(hindicorpus,removeWords,as.matrix(hindistop))  
  hindicorpus<-tm_map(hindicorpus,removeNumbers)
  hindidtm<-TermDocumentMatrix(hindicorpus,list(wordLengths=c(0,Inf)))
  hindidtm<-rowSums(as.matrix(hindidtm))    
  abc<-rownames(as.data.frame(hindidtm))
  abc<-toString(abc,width = NULL)
  abc<-gsub(",","",abc)
  cat(abc,file="~/stop words testing/Learning algo/temp.txt",append = TRUE,sep = "\n") 
  
  for(j in 1: length(domains)) 
  {
    
    abc<-gsub(domains[j],as.character(paste(" ",j," ")),abc,ignore.case = TRUE)
  }
  fileName="~/stop words testing/Learning algo/Domainstesthindi.txt"
  
  abc<-gsub("[A-z]|[[:punct:]]","",abc)
  
  t<-gsub(" ","",abc)
  if(t=="")
  {
    abc<- -1
  }
  cat(abc,file=fileName,append = TRUE,sep = "\n") 
}
v<-max(count.fields(file=fileName))
replacedomain<-read.table(file =fileName,header = FALSE ,fill=TRUE,col.names=1:v,stringsAsFactors=FALSE)
p=nrow(replacedomain)
cluster<-matrix(data=0,ncol = 1) 
l1<-0
for(k in 1 : p)               #Loop variable k and p cant be updated at any cost
{ l1<-l1+1
  tempcluster<-matrix(data=0,ncol =1)
  abc<-replacedomain[k,]
  abc<-unique(unlist(abc),incomparables = FALSE)
  x=1
  t=k
  for(l in k : p)
  { 
    countpqr<-0
    pqr<-replacedomain[t,]
    pqr<-unique(unlist(pqr),incomparables = FALSE)
    
    for(b in 1 : length(pqr))
    {
      if(!is.na(pqr[b]))
        if(pqr[b]!="" )        
          countpqr<-countpqr+1  #no of kam ke words in pqr 
    }
    count<-0    
    tempcount<-0
    
    for(a in 1 : length(abc))
    {  
      if(!is.na(abc[a]))
        if(abc[a]!="")
        {count=count+1;
         for(b in 1 : length(pqr))
         {
           
           if(abc[a]==pqr[b]&&pqr[b]!=""&&!is.na(pqr[b]))         
           {tempcount<-tempcount+1
           break
           }
        
         
         }
         
         
        }
      
    }
    
      
#    cat(abc,append = TRUE,file="~/stop words testing/Learning algo/Debug.txt")
#      cat(pqr,append = TRUE,file="~/stop words testing/Learning algo/Debug.txt")
#      cat(paste("k=",k,"l=",l,rownames(replacedomain[k,]),rownames(replacedomain[l,])),append = TRUE,file="~/stop words testing/Learning algo/Debug.txt",sep="\n")
#     
    if(tempcount==count&&countpqr==count)
    {
      if(x>dim(tempcluster)[2])
      {
        tempcluster<-cbind(tempcluster,c(0))
      }
      tempcluster[1,x]<-rownames(replacedomain[t,]);
       x=x+1
    
      
      if(k!=l)
      { 
        replacedomain<-replacedomain[-1*t,] #DEL ETE DUPLICATE ROWS      
       t=t-1
        p=nrow(replacedomain)
      }
      
    }  
    t=t+1
  }

 if(!-1%in%abc) #dont append clusture of blank lines 
{if(dim(tempcluster)[2]!=dim(cluster)[2]) #check for if number of column in cluster are equal if less then add
{ c<-dim(cluster)[2]
  ctemp<-dim(tempcluster)[2]
  if(ctemp>c)
  for(t in 1:(ctemp-c))
  {cluster<-cbind(cluster,c(0))

  }
   if(c>ctemp)
  for(h in 1:(c-ctemp))
    tempcluster<-cbind(tempcluster,c(0))
}

cluster<-rbind(cluster,tempcluster)
}
if(p-k==0){
 cluster<-cluster[-1,]
  break
  
}
  }


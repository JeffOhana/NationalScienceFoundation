#the smallWorldNewman function generates a weighted Newman small world network
#with n nodes connected to k closest neighbors with weight 1 and an additional
#k random connections per node with random weight (0,1) from a uniform dist

#the function takes 3 parameters: n=number of nodes, k=number of neighbors 
#(k/2 neighbors on each side, k is required to be even),f=file to which the 
#created small world matrix is saved- default is "matrix.txt"

#MISSING A CHECK that the matrix is sufficiently large for the given k
#MISSING A CHECK in the randConn function to prevent infinite loop if a given
#node already has all possible connections

smallWorldNewman <- function(n,k,f="matrix.txt"){
  library(MASS)
  library(Matrix)
  if (k%%2!=0){ #check that k is even
    stop("need an even number of k neighbors")
  }
  #generate regular part of the graph
  if (k==2){ #if k=2 use smWor2 function below
    x<-smWor2(n)
  }
  else{ #if k>2 use smWor4 function below
    x<-smWor4(n,k)
  }
  #add random connections
  x<-randConn(n,k,x)
  x<-forceSymmetric(x)
  write.matrix(x,f,sep = " ")
}

#make neighbor connections
smWor2<-function(n){
  x<-matrix(rep(0,n*n),n)
  for (i in 1:(n-1)){
    x[i,i+1]<-1 #add weight 1 to each edge of neighboring nodes
  }
  x[1,n]<-1 #connect last and first node
  return(x)
}
smWor4 <- function(n,k){
  x<-matrix(rep(0,n*n),n)
  for (i in 1:(n-(k/2))){ #assign weight 1 to neighbors nodes 1 through n-(k/2)
    for (j in 1:(k/2)){
      x[i,i+j]<-1
    }
  }
  for (i in 0:((k/2)-1)){ #assign weight 1 to edges to connect end and beginning
    for (j in 1:((k/2)-i)){
      x[1+i,n-j+1]<-1
    }
  }
  for (i in (n-(k/2)+1):(n-1)){ #assign weight 1 to neighbors the first for loop
    for (j in 1:(n-i)){         #missed- nodes n-(k/2)+1 through n-1
      x[i,i+j]<-1
    }
  }
  return(x)
}

#add random connections
randConn <- function(n,k,x){
  for (i in 1:n){
    for (j in 1:k){
      rand<-runif(1) #generate random weight (0,1)
      repeat{
        node<-sample(1:n,1) #choose node at random
        #check that edge doesn't already exist and that won't create a self loop
        if (x[i,node]==0 && x[node,i]==0 && i!=node){
          #assign random weight
          x[i,node]<-rand
          x[node,i]<-rand
          break
        }
      }
    }
  }
  return(x)
}
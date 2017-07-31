DATA=read.table("E1_2006.ped",row.names=2)
BABY=read.table("baby.txt",row.names=2)
MOM=read.csv("parents.txt", header=T)
predicted_pop=c()
#false=0
K=dim(DATA)
pos=0
neg=0
WIN=250
step=250
count=0
count1=0
d=0
index=0
smallest = 1
loc_smallest = 0
pop_smallest = ""
pop_vector=c()
#for(pop in 1:K[1]){
X = BABY[1,]
#X=DATA[1,]
#for(start in 6:(length(X)-WIN)){#over windiws
for(start in seq(from=6, to=(length(X)-WIN), by=step)){
  d_vector=c()
  #over row
  for(i in 1:K[1]){#over database individuals
    #i = 2
    count=WIN #count = total no. of SNPs in window
    count1=0 #clear value for mismatches
    for(j in start:(start+WIN)){#within the window
      if(as.character(X[j-1][[1]])!=as.character(DATA[i,j])){
        count1=count1+1 #increase no. of matches if data points aren't equal
      }
    }
    
    d=count1/count
    d_vector=c(d_vector,d)
  }
  
  smallest = min(d_vector)
  print(smallest)
  
  for(x in 1:dim(DATA)[1]){
    if(identical(d_vector[x],smallest)){
      loc_smallest = x
      pop_smallest = as.character(DATA[x,1][[1]])
      break;
    } 
  }
  
  pop_vector=c(pop_vector,pop_smallest)
  
  #if((index*WIN) > length(X)){
  
  #  break
  
  #}
  
  
  
}#end go over reference

#write.csv(pop_vector,"predicted.txt")

#PREDICT=read.csv("predicted.txt", header=T)

for(ind in 1:length(BABY)){ #5 is where data first starts. 

 if(identical(pop_vector[round((ind/WIN)-.5)],as.character(MOM[ind,2]))){
   pos=pos+1
 } else {
   neg=neg+1
 }
}

#
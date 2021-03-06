DATA=read.table("1.ped",row.names=2)
#BABY=read.table("baby.txt",header=TRUE)
BABY=as.data.frame("baby.txt")
K=dim(DATA)
WIN=10
step=10
count=0
count1=0
d=0
index=0
smallest = 1
loc_smallest = 0
pop_smallest = ""
pop_vector=c()
#for(pop in 1:K[1]){
  d_vector=c()
  X = BABY[1,]
  #for(start in 6:(length(X)-WIN)){#over windiws
  for(start in seq(from=6, to=(length(X)-WIN), by=step)){
    d_vector=c()
    #over row
    for(i in 1:K[1]){#over database individuals
      #i = 2
      count=WIN #count = total no. of SNPs in window
      count1=0 #clear value for mismatches
      for(j in start:(start+WIN)){#within the window
        if(X[j][[1]]!= DATA[i,j]){
          count1=count1+1 #increase no. of mismatches if data points aren't equal
        }
      }
      d=count1/count
      d_vector=c(d_vector,d)
    }
    smallest = min(d_vector)
    for(x in 1:length(d_vector)){
      if(d_vector[x] == smallest){
        loc_smallest = x
        pop_smallest = as.character(DATA[x,1][[1]])
        break; 
      } 
    }
    pop_vector=c(pop_vector,pop_smallest)
    if((index*WIN) > length(X)){
      break
    }
  }#end go over reference

DATA=read.table("1.ped",row.names=2)
K=dim(DATA)
baby = c()
Break = c()
used_column = c(rep("not used",K[1]))
dc=cbind(DATA,used_column)
#while(TRUE){
  all_used = FALSE
  p1 = round(runif(1, 1,K[1]))
  p2 = round(runif(1, 1,K[1]))
  while(identical(dc[p1,K[2]],"used")){
    p1 = runif(1, 1:K[1])
  }
  while(identical(dc[p2,K[2]],"used")){
    p2 = runif(1, 1:K[1])
  }
  while(p1 == p2){
    p2 = runif(1, 1:K[1])
  }
  #replace(dc,dc[p1,K[2]],"used")
  vv[p1]='used'
  dc$used_column=vv
  dc[p1,K[2]]
  vv[p2]='used'
  dc$used_column=vv
  dc[p2,K[2]]
  person1 <- c(dc[p1, K[2]])
  person2 <- c(dc[p2, K[2]])
  Break = sort(round(runif(100, 6, K[2]))) 
  baby = person1[1: (Break[1] - 1)]
  for(x in 1: (length(Break) - 1)){
    if(x %% 2 == 0){
      baby <- c(baby, person1[Break[x]: Break[x+1]])
    }else{
      baby <- c(baby, person2[Break[x]: Break[x+1]])
    }
  }
  for(index in 1:K[1]){
    if(identical(dc[index, K[2]],"not_used")){
      break
    } else {
      all_used = TRUE
    }
  }
  #if(all_used == TRUE){
   # break
  #}
#}
  
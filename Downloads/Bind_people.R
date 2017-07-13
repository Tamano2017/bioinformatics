DATA=read.table("1.ped",row.names=2)
BABY = c()
Break = c()
labeled_baby=c()
vv=c(rep("not used",K[1]))
used_column = c(rep("not used",K[1]))
dc=cbind(DATA,used_column)
K=dim(dc)
BABY_matrix=c()
index=0
parents= c()
pop1_SNP=0
pop2_SNP=0
all_used = FALSE
#while(all_used == FALSE){
  p1 = #round(runif(1, 1,K[1]))
  p2 = #round(runif(1, 1,K[1]))
  #while(identical(dc[p1,K[2]],"used")){
  #  p1 = runif(1, 1:K[1])
  #}
  #while(identical(dc[p2,K[2]],"used")){
  #  p2 = runif(1, 1:K[1])
  #}
  #while(p1 == p2){
  #  p2 = runif(1, 1:K[1])
  #}
  #vv[p1]='used'
  #dc$used_column=vv
  #dc[p1,K[2]]
  #vv[p2]='used'
  #dc$used_column=vv
  #dc[p2,K[2]]
  person1 <- dc['4257735763_A',] #dc[p1,1:(K[2]-1)]
  person2 <- dc['4257735504_A',] #dc[p2,1:(K[2]-1)]
  Break = sort(round(runif(100, 7, K[2]))) 
  BABY=person1# c(BABY,person1[6:(Break[1] - 1)])
  parents= rep(dc['4257735763_A',1],length(BABY))
  for(x in 1: (length(Break) - 2)){
    if(x %% 2 == 0){
      BABY[Break[x]:Break[x+1]]=person2[Break[x]: Break[x+1]]
      parents[Break[x]:Break[x+1]]=dc['4257735504_A',1]
    }
  }
  #BABY_matrix=rbind(BABY_matrix,BABY)
  for(p in 1:length(parents)){
    if(parents[p]==dc['4257735763_A',1]){pop1_SNP=pop1_SNP+1}
    if(parents[p]==dc['4257735504_A',1]){pop2_SNP=pop2_SNP+1}
  }
  labeled_baby <- c(parents,BABY)
  index=index+2
  if(index == K[1]){
    all_used = TRUE
  }
#}
  
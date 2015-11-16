library(mclust)

data(Baudry_etal_2010_JCGS_examples)
mc = clustCombi(ex4.2)
post = mc$MclustOutput$z

library(mixpack)
L8 = list(diag(NCOL(post)),post) 
L7 = mergeStep_const_entropy(L8[[2]])
L6 = mergeStep_const_entropy(L7[[2]])
L5 = mergeStep_const_entropy(L6[[2]])
L4 = mergeStep_const_entropy(L5[[2]])
L3 = mergeStep_const_entropy(L4[[2]])
L2 = mergeStep_const_entropy(L3[[2]])

##Hi han diferencies

L8[[1]] %*% L7[[1]]%*% L6[[1]] %*% L5[[1]] %*% L4[[1]] %*% L3[[1]] %*% L2[[1]]
table(mc$classification[[8]], mc$classification[[2]])

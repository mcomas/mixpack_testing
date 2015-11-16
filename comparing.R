library(mclust)

data(Baudry_etal_2010_JCGS_examples)
mc = clustCombi(ex4.2)
post = mc$MclustOutput$z

library(mixpack)
L8 = list(diag(NCOL(post)),post) 
L7 = mergeStep(L8[[2]], 'const', 'entropy')
L6 = mergeStep(L7[[2]], 'const', 'entropy')
L5 = mergeStep(L6[[2]], 'const', 'entropy')
L4 = mergeStep(L5[[2]], 'const', 'entropy')
L3 = mergeStep(L4[[2]], 'const', 'entropy')
L2 = mergeStep(L3[[2]], 'const', 'entropy')

##Hi han diferencies

L8[[1]] %*% L7[[1]]%*% L6[[1]] %*% L5[[1]] %*% L4[[1]] %*% L3[[1]] %*% L2[[1]]
table(mc$classification[[8]], mc$classification[[2]])

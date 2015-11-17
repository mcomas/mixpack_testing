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


xlog = function(x) ifelse(x==0, 0, x * log(x))
l_lambda = list(
  'entr' = function(v_tau, a, b) xlog(v_tau[a] + v_tau[b]) - xlog(v_tau[a]) - xlog(v_tau[b]),
  'demp' = function(v_tau, a, b) if(which.max(v_tau) == b) 1 else 0,
  'demp.mod' = function(v_tau, a, b) v_tau[b] * (v_tau[a] + v_tau[b])^-1,
  'coda' = function(v_tau, a, b) log(v_tau[b] / v_tau[a]),
  'coda.norm' = function(v_tau, a, b) -log(v_tau[b] / v_tau[a])^2,
  'prop' = function(v_tau, a, b) v_tau[b] )

# Weitghing functions
l_omega = list(
  'cnst' = function(v_tau, a) 1,
  'prop' = function(v_tau, a) v_tau[a],
  'dich' = function(v_tau, a) if(which.max(v_tau) == a) 1 else 0
)

fun1 = function(post){
  mixpack::get_hierarchical_partition(post, l_omega[['dich']], l_lambda[['entr']])
}
fun2 = function(post){
  mixpack::get_hierarchical_partition_fast(post, 'dich', 'entropy')
}


system.time(hp1 <- fun1(post))
system.time(hp2 <- fun2(post))

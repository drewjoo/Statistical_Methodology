# Permutation Test

# CIFAR10과 TinyImageNet의 Cosine Similarity 
x = c(0.9836458, 0.98679346, 0.97652966, 0.9875398, 0.96775347, 0.98156935, 0.9777201, 0.97770274, 0.9868976, 0.97849494,
      0.96807307, 0.9782228, 0.97763, 0.97853947, 0.9845965, 0.9834155, 0.97115946, 0.98483765, 0.97235817, 0.9765884)

# WM811K와 Mixed WM811K의 Cosine Similarity 
y = c(0.7436851, 0.7385785, 0.7498541, 0.7495987, 0.74609363, 0.74981433, 0.7506356, 0.7440469, 0.75910604, 0.764551,
      0.7503272, 0.75200033, 0.7489456, 0.7673101, 0.74372, 0.7473218, 0.74083483, 0.7557458, 0.7614375, 0.74430144)

z = c(x, y)

# Method 1. Using t-test 
N<-length(z);m<-length(x)
B<-1000;tperm = numeric(B)

tobs<-t.test(x,y)$statistic

for(i in 1:B){
  idx<-sample(1:N, size = m, replace=F)
  tperm[i] = t.test(z[idx],z[-idx])$statistic
}

mean(abs(tperm)>tobs) # p-value = 0, 즉 두 진단간 평균차이는 유의하다.
hist(tperm, freq=F, xlab="T(P_Value = 0)", xlim = c(-5,105))
points(tobs,0,col="red", pch=16,cex=1.2)


# Method 2. 
diff = mean(x) - mean(y)
diff # 0.228608
B = 1000;diff_perm = numeric(B)

for(i in 1:B){
  idx<-sample(1:N, size = m, replace=F)
  diff_perm[i] = mean(z[idx])-mean(z[-idx])
}
hist(diff_perm, xlab="T(P_Value = 0)", xlim = c(-0.3,0.3))
points(diff,0,col="red", pch=16,cex=1.2)
mean(abs(diff_perm)>diff) # p-value = 0, 즉 두 진단간 평균차이는 유의하다.


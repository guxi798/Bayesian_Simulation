shape0 = 1
rate0 = 1
alpha = rgamma(1, shape0, 0.5)
lamda = rgamma(1, shape0, rate0)

y = c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 1, 1, 5, 65)

alpha0 = alpha; #alpha = alpha0
lamda0 = lamda; #lamda = lamda0
alpha.store = alpha
lamda.store = lamda

n = 10000
for(i in 1:n){
	#sample lamda ~ Gamma(shape, rate/(1+rate*yi^alpha))
	#rate = rate/(1+rate*sum(y^alpha))
	shape = shape0+1
	rate = rate0+sum(y^alpha)
	lamda = rgamma(1, shape, rate)
	lamda.store = c(lamda.store, lamda)
	
	#sample alpha
	alpha.n = rnorm(1, mean(alpha.store), 0.5)

	p.old = 1
	for(yj in y){
		p.old = p.old*(lamda*alpha*yj^(alpha-1)*exp(-lamda*yj^alpha))
	}
	p.new = 1
	for(yj in y){
		p.new = p.new*(lamda*alpha.n*yj^(alpha.n-1)*exp(-lamda*yj^alpha.n))
	}
	q.old = dnorm(alpha, mean(alpha.store), 0.5)
	q.new = dnorm(alpha.n, mean(alpha.store), 0.5)
	if(q.new>0 & q.old>0){
		ratio = q.new/q.old
	}else{
		ratio = 1
	}
	if(ratio >= 1){
		alpha = alpha.n
	}else{
		u = runif(1)
		if(ratio > u){
			alpha = alpha.n
		}
	}
	alpha.store = c(alpha.store, alpha)
}
par(mar=c(6,5,4,2))
plot(alpha.store, main="alpha", xlab="Iterations", ylab="Value", type="l", sub=paste("alpha0=",alpha0,", lamda0=",lamda0, sep=""), cex.main=2, cex.lab=1, cex.sub=1.2)
plot(lamda.store, main="lamda", xlab="Iterations", ylab="Value", type="l", sub=paste("alpha0=",alpha0,", lamda0=",lamda0, sep=""))

y.m = median(y)
p.m = lamda*alpha*y.m^(alpha-1)*exp(-lamda*y.m^alpha)

y.t = 24
p = lamda*alpha*y.t^(alpha-1)*exp(-lamda*y.t^alpha)

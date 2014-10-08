library(mnormt)

shape = 2
rate = 2
#alpha = rgamma(1, shape, rate)
#lamda = rgamma(1, shape, rate)
y = c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 1, 1, 5, 65)

alpha0 = 10
lamda0 = 10 
alpha = alpha0
lamda = lamda0
alpha.store = alpha
lamda.store = lamda
alpha.v2 = 0.5
lamda.v2 = 0.5
cov = 0

n = 5000
for(i in 1:n){
	

	p.old = 1
	for(yj in y){
		p.old = p.old*lamda*alpha*yj^(alpha-1)*exp(-lamda*yj^alpha)
	}
	
	mu = c(mean(alpha.store), mean(lamda.store))
	
	sigma = matrix(c(1, 0, 0, 1), ncol=2, byrow=TRUE)

	prop = rmnorm(1, mu, sigma)
	#prop = mvrnorm(1, mu, sigma)
	alpha.n = prop[1] 
	lamda.n = prop[2] #new proposed values

	p.new = 1
	for(yj in y){
		p.new = p.new*lamda.n*alpha.n*yj^(alpha.n-1)*exp(-lamda.n*yj^alpha.n)
	}

	q.old = dmnorm(c(alpha,lamda), mu, sigma)
	q.new = dmnorm(c(alpha.n,lamda.n), mu, sigma)

	ratio = (p.new*q.new)/(p.old*q.old)
	ratio = ifelse(is.na(ratio), 1, ratio)
	if(ratio >= 1){
		alpha = alpha.n
		lamda = lamda.n
		#alpha.store = c(alpha.store, alpha)
		#lamda.store = c(lamda.store, lamda)
		alpha.v2 = var(alpha.store)
		lamda.v2 = var(lamda.store)
		cov = cov(alpha.store,lamda.store)
	}else{
		u = runif(1)
		if(ratio > u){
			alpha = alpha.n
			lamda = lamda.n
			#alpha.store = c(alpha.store, alpha)
			#lamda.store = c(lamda.store, lamda)
			alpha.v2 = var(alpha.store)
			lamda.v2 = var(lamda.store)
			cov = cov(alpha.store,lamda.store)
		}
	}
	alpha.store = c(alpha.store, alpha)
	lamda.store = c(lamda.store, lamda)

}
par(mar=c(6,5,4,2))
plot(alpha.store, main="alpha", xlab="Iterations", ylab="Value", type="l", sub=paste("alpha0=",alpha0,", lamda0=",lamda0, sep=""), cex.main=2, cex.lab=1, cex.sub=1.2)
plot(lamda.store, main="lamda", xlab="Iterations", ylab="Value", type="l", sub=paste("alpha0=",alpha0,", lamda0=",lamda0, sep=""))

y.m = median(y)
p.m = lamda*alpha*y.m^(alpha-1)*exp(-lamda*y.m^alpha)

y.t = 24
p = lamda*alpha*y.t^(alpha-1)*exp(-lamda*y.t^alpha)





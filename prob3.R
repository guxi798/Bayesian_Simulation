library(mnormt)

shape = 0.5
rate = 0.5
alpha = rgamma(1, shape, rate)
lamda = rgamma(1, shape, rate)
y = c(65, 156, 100, 134, 16, 108, 121, 4, 39, 143, 56, 26, 1, 1, 5, 65)

alpha.store = alpha
lamda.store = lamda
alpha.v2 = 0.5
lamda.v2 = 0.5
cov = 0

n = 5000
for(i in 1:n){
	

	p.old = 1
	for(yj in y){
		p.old = lamda*alpha*yj^(alpha-1)*exp(-lamda*yj^alpha)
	}
	
	mu = c(mean(alpha.store), mean(lamda.store))
	
	sigma = matrix(c(1, 0, 0, 1), ncol=2, byrow=TRUE)

	prop = rmnorm(1, mu, sigma)
	#prop = mvrnorm(1, mu, sigma)
	alpha.n = prop[1] 
	lamda.n = prop[2] #new proposed values

	p.new = 1
	for(yj in y){
		p.new = lamda.n*alpha.n*yj^(alpha.n-1)*exp(-lamda.n*yj^alpha.n)
	}

	q.old = dmnorm(c(alpha,lamda), mu, sigma)
	q.new = dmnorm(c(alpha.n,lamda.n), mu, sigma)

	ratio = (p.new*q.new)/(p.old*q.old)
	ratio = ifelse(is.na(ratio), 1, ratio)
	if(ratio >= 1){
		alpha = alpha.n
		lamda = lamda.n
		alpha.store = c(alpha.store, alpha)
		lamda.store = c(lamda.store, lamda)
		alpha.v2 = var(alpha.store)
		lamda.v2 = var(lamda.store)
		cov = cov(alpha.store,lamda.store)
	}else{
		u = runif(1)
		if(ratio > u){
			alpha = alpha.n
			lamda = lamda.n
			alpha.store = c(alpha.store, alpha)
			lamda.store = c(lamda.store, lamda)
			alpha.v2 = var(alpha.store)
			lamda.v2 = var(lamda.store)
			cov = cov(alpha.store,lamda.store)
		}
	}
}
plot(alpha.store, main="alpha", xlab="Iterations", ylab="Value", type="l")
plot(lamda.store, main="lamda", xlab="Iterations", ylab="Value")








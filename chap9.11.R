j = c(1, 2, 3, 4, 5) #time in week
x = matrix(c(151, 199, 246, 283, 320,
               145, 199, 249, 293, 354,
               147, 214, 263, 312, 328,
               155, 200, 237, 272, 297,
               135, 188, 230, 280, 323,
               159, 210, 252, 298, 331,
               141, 189, 231, 275, 305,
               159, 201, 248, 297, 338,
               177, 236, 285, 350, 376,
               134, 182, 220, 260, 296,
               160, 208, 261, 313, 352,
               143, 188, 220, 273, 314,
               154, 200, 244, 289, 325,
               171, 221, 270, 326, 358,
               163, 216, 242, 281, 312,
               160, 207, 248, 288, 324,
               142, 187, 234, 280, 316,
               156, 203, 243, 283, 317,
               157, 212, 259, 307, 336,
               152, 203, 246, 286, 321,
               154, 205, 253, 298, 334,
               139, 190, 225, 267, 302,
               146, 191, 229, 272, 302,
               157, 211, 250, 285, 323,
               132, 185, 237, 286, 331,
               160, 207, 257, 303, 345,
               169, 216, 261, 295, 333,
               157, 205, 248, 289, 316,
               137, 180, 219, 258, 291,
               153, 200, 244, 286, 324),
            ncol = 5, byrow = TRUE)
nrow = dim(x)[1]
ncol = dim(x)[2]

alpha0 = 0 #prior for a0
beta0 = 0 #prior for b0
a.v2 = 10
b.v2 = 1 # a.v2 and b.v2 are the diagonal of sigma matrix
v2 = 1 # overal variance

n = 10
alpha.store=NULL
beta.store=NULL
for(p in 1:n){
	#set.seed(123)
	alpha = NULL
	beta = NULL #store sampled data
	alpha.store = c(alpha.store, alpha0)
	beta.store = c(beta.store, beta0)

	for(q in 1:nrow){
		a.var = ncol/v2 + 1/a.v2
		a.mean = (sum(x[q,])/v2 + alpha0/a.v2)/a.var
		alpha = c(alpha, rnorm(1, mean = a.mean, sd = sqrt(a.var)))

		b.var = var(j)/v2 + 1/b.v2
		b.mean = (sum(x[q,]*(j-mean(j)))/v2 + beta0/b.v2)/b.var
		beta = c(beta, rnorm(1, mean = b.mean, sd = sqrt(b.var)))
	}
	alpha.mean = mean(alpha)
	beta.mean = mean(beta)

	alpha0 = rnorm(1, mean = alpha.mean, sd = sqrt(a.v2/ncol))
	beta0 = rnorm(1, mean = beta.mean, sd = sqrt(b.v2/ncol))

	a.v2 = 1/(beta0*rgamma(1,alpha0)) #Inverse Gamma(alpha0, beta0)
	b.v2 = 1/(beta0*rgamma(1,alpha0))
	v2 = 1/(beta0*rgamma(1,alpha0))
}

alpha0 #106.5924
beta0	#131.3745











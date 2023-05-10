### 1)Newton method ########
tol<-0.001 
initial<- 0 
f<-function(x){ 230*x^4+18*x^3+9*x^2-221*x-9} 
f(4)
fPrime <- function(x) {920*x^3+54*x^2+18*x-221} 
root <- function(f, fPrime, initial, tol) { 
  x = initial 
  while (abs(f(x)) > tol) { 
    x = x - f(x)/fPrime(x) 
  } 
  x 
} 
root(f, fPrime, initial, tol)
###Taylor Series Using General Form 
#our function=1/1-x round zero
install.packages("pracma") 
library(pracma) 
equation1 <- function(x) {1/(1-x)}
p <- taylor(equation1, x0 = 0, n = 3)
p

###Graphing Taylor Polynomial 
x <- seq(-1.0, 0, length.out=100) 
f <- function(x) (1/(1-x)) 
p <- taylor(f, 0, 3) 

yf <- f(x) 
yp <- polyval(p, x) 
x
plot(x, yf, type = "l", main =' Taylor Series Approximation of 1/(1-x)',col 
     = "blue", lwd = 3) 
lines(x, yp, col = "red") 
legend('topleft', inset=.05, legend= c("Taylor Series", "f(x)=1/(1 - x)") 
       , lwd=c(2.5,2.5), col=c('red', 'blue'), bty='n', cex=.75) 

### 3) Bisection method --
a<-1 
b<-2 
f<-function(x){x^3+4*x^2-10} 
f(a)
f(b)  #conditions are satisfied
#then calculate the midpoint
c<-(a+b)/2 
while(f(c)!=0 && b-a >.02) 
{ 
  if(f(c)==0) 
  { 
    c 
  }
  if(f(c)<0) 
  { 
    a<-c 
  } 
  else 
  { 
    b=c 
  } 
  { 
    c<-(a+b)/2;c 
  } 
} 
c  
### 4)Newton method ########
#a
tol<-0.0001 
initial<- -2 
f<-function(x){ x^3+3*x-1} 
f(4)
fPrime <- function(x) {3*x^2+3} 
root <- function(f, fPrime, initial, tol) { 
  x = initial 
  while (abs(f(x)) > tol) { 
    x = x - f(x)/fPrime(x) 
  } 
  x 
} 
root(f, fPrime, initial, tol)

newton.method(FUN = function(x) x^3 + 3*x -1, init = -2, rg 
              = c(-3, -2), tol = 0.0001)
#b
tol<-0.0001 
initial<- 0.5 
f<-function(x){ sin(x) - exp(-x)} 
f(4)
fPrime <- function(x) {cos(x) + exp(-x)} 
root <- function(f, fPrime, initial, tol) { 
  x = initial 
  while (abs(f(x)) > tol) { 
    x = x - f(x)/fPrime(x) 
  } 
  x 
}
root(f, fPrime, initial, tol)

newton.method(FUN = function(x) sin(x) - exp(-x), init = 0.5, rg 
              = c(0, 1), tol = 0.0001)
### 5) Fixed iteration method #########
tol<-0.0001 
initial<- 1 
g<-function(x){(1/12)*(1+x^3)} 
root <- function(g, initial, tol) { 
  x = initial 
  while (abs(x-g(x)) > tol) { 
    x = g(x) 
  } 
  x 
} 

root(g, initial, tol)

tol<-0.0001 
initial<- 1 
install.packages("spuRs") 
library(spuRs) 
fixedpoint(g, 0.5, tol = 0.0001, max.iter = 100)
#6) in trapezoidal we need to define the function,a,b,h=b-a/n
f=function(x){y=x/((x^2+4))  
return(y)} 

trapezoid= function(f, a, b, n) { 
  
  h <- 0.25
  
  j <- 1:(n - 1) 
  
  xj <- a + j * h 
  
  value <- (h / 2) * (f(a) + 2 * sum(f(xj)) + f(b)) 
  
  return(value) 
} 
trapezoid(f,1,3,8)
simpson <- function(f, a, b, n) { 
  h <- 0.25
  x <- seq(a, b, h) 
  
  s <- f(x[1]) + f(x[n+1]) + 4*sum(f(x[seq(2,n,2)])) + 2 *sum(f(x[seq(3,n-1, 2)])) 
  
  s <- s*h/3 
  return(s) 
} 

simpson(f,1,3,8)


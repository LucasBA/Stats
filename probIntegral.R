f_of_x <- function(x){ exp(sqrt(sin(x)))}
epsilon = .Machine$double.eps*100000000
n = 100000000
i = 1.5
while (i*epsilon<pi){
  dy = (f_of_x(i+epsilon)-f_of_x(i))/epsilon
  if (dy < 0){
    i = i-epsilon
    break
  }
  i= i +epsilon
}
maxima = f_of_x(i)
x_obs <- runif(n, min = 0, max = pi)
y_obs <- runif(n, min = 0, max = maxima)

hit <- (f_of_x(x_obs)-y_obs)>=0
rectangle = (pi*(maxima))
I = mean(hit)*rectangle

# R Lab from Chapter 2
#setwd("C:/Users/eric/Desktop/R Projects/Intro_To_Stats")


# Normalized values and statistics
x <- rnorm(50)
y <- x + rnorm(50, mean=50, sd=.1)

# correlation
cor(x,y)

# Standard Deviation
sd(x)

# Variance = sd^2
var(x)

# Standard Error
se <- function(x) sd(x)/sqrt(length(x))

# to perform linear convolution
convolve(x, y, type = "open")

# Fast Fourier Transform
fft(x)

# Outer Product: The product of two vectors is a matrix
outer(x,y)

# Graphics
plot(x,y)

# Plotting in 3-D
x = seq(-pi,pi, length=50)
y = x
f=outer(x,y,function (x,y)cos(y)/(1+x^2))

# Contour: takes in x, y, and z axis and plots in 3-D with height in contours
contour(x,y,f)
contour (x,y,f,nlevels =45, add=T)

# t() takes the transpose of a matrix
fa=(f-t(f))/2

contour (x,y,fa,nlevels =15)

# To get a heat map
image(x,y,fa)

# To give 3-D perspective
persp(x,y,fa)
persp(x,y,fa ,theta =30)
persp(x,y,fa ,theta =30, phi =20)
persp(x,y,fa ,theta =30, phi =70)
persp(x,y,fa ,theta =30, phi =40)



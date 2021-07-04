# This code generates random numbers with desired statistical properties,
# first by using Box- Muller method and then by # using Polar-Marsaglia method. 
# The getUnifDist() method uses LGM to generate n Uniformly distributed random numbers on [0,1]


getUnifDist <- function(seed, n) {
  x0 <- seed
  a <- 7^5
  m <- 2^31 -1
  lgm <- vector()
  lgm[1] <-  x0
  for (i in 2:n) {
    lgm[i] <- (a*lgm[i-1]) %% m
  }
  
  unif_dist <- vector()
  for (i in 1:n) {
    unif_dist[i] <- (lgm[i]+0.5)/m
  }
  
  return(unif_dist)
}



#Box- Muller Method to generate 5,000 Normally distributed random numbers 
#with mean 0 and variance 1.

box_start <- Sys.time()
u1 <- getUnifDist(as.numeric(as.POSIXct(Sys.time())),2500)  
u2 <- getUnifDist(as.numeric(as.POSIXct(Sys.time())),2500)

box_muller <- vector()
z1_vect <- vector()
z2_vect <- vector()
pi = 22/7
for (i in 1:2500) {
  z1_vect [i] <- sqrt (-2*log(u1[i]))*cos(2*pi*u2[i])
  z2_vect [i] <- sqrt (-2*log(u1[i]))*sin(2*pi*u2[i])
} 
z <- vector()
z <- c(z1_vect,z2_vect)
box_end <- Sys.time()

hist(z,main = "Distribution of Random Numbers using Box- Muller Method")
print(paste("Mean: ",mean (z), " SD: ", sd(z)))


#Polar-Marsaglia method to Generate 5,000 Normally distributed random numbers
#with mean 0 and variance 1.
polar_start <- Sys.time()
counter <-  0
seedval1 <- 123
seedval2 <- 789
z1_vect <- vector()
z2_vect <- vector()
while(counter < 2500) {
  u1 <- as.double (getUnifDist(seedval1,10)  )
  u2 <- as.double (getUnifDist(seedval2,10)  )
  
  seedval1 <- seedval1 + 1
  seedval2 <- seedval2 + 1
  
  v1 <-  2*u1[4] - 1
  v2 <-  2* u2[5] - 1
  w = v1^2 + v2^2
  
  if (w <= 1) {
    #print (paste("w <=1 with counter: ",counter))
    z1 = v1*sqrt ((-2*log(w))/w)
    z2 = v2*sqrt ((-2*log(w))/w)
    z1_vect <- append(z1_vect,z1)
    z2_vect <- append(z2_vect,z2)
    counter <-  counter +  1
    
  }
}
z <- vector()
z <-  c(z1_vect,z2_vect)
print(paste("Mean: ",mean (z), " SD: ", sd(z)))
polar_end <- Sys.time()
hist(z,main = "Distribution of Random Numbers using Polar-Marsaglia Method")
print (paste("Time taken by Box-Muller Method: ", round (box_end- box_start, digits = 4),"s"))
print (paste("Time taken by Polar-Marsaglia Method: ", round (polar_end- polar_start, digits = 4),"s"))

#Empirically we can verify that the Box-Muller Method is more efficient. 


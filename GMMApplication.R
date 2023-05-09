#I begin with cleaning the data

#Finding out NAs and imputing them with averages
FinalData2 <- FinalData[!(is.na(FinalData$Cons2019)), ]
FinalData2 <- FinalData2[!(is.na(FinalData2$Cons2020)), ]
FinalData2 <- FinalData2[!(is.na(FinalData2$Cons2021)), ]
FinalData2 <- FinalData2[!(is.na(FinalData2$Invm2019)), ]
FinalData2 <- FinalData2[!(is.na(FinalData2$Invm2020)), ]
FinalData2 <- FinalData2[!(is.na(FinalData2$Invm2021)), ]

#Defining the income variable

FinalData2$Income2019 <- FinalData2$Cons2019 + FinalData2$Invm2019
FinalData2$Income2020 <- FinalData2$Cons2020 + FinalData2$Invm2020
FinalData2$Income2021 <- FinalData2$Cons2021 + FinalData2$Invm2021

# Moment restrictions
g1 <- function(theta, FinalData2) {
  m1 = (FinalData2$Cons2021 - theta[1] * FinalData2$Income2020)
  m2 = (FinalData2$Cons2020 - theta[1] * FinalData2$Income2019)
  m3 = FinalData2$Invm2021 - theta[2] * (FinalData2$Cons2021 - FinalData2$Cons2020)
  m4 = FinalData2$Invm2020 - theta[2] * (FinalData2$Cons2020 - FinalData2$Cons2019)
  f = cbind(m1,m2,m3, m4)
  return(f)
}

library(gmm)

# Running GMM
gmm_mod = gmm(
  # Moment restriction equations
  g = g1,
  # Matrix of data
  x = FinalData2,
  # Starting location for minimization algorithm
  t0 = c(0,0), # Required when g argument is a function
  type = "twoStep"
)

# Reporting results
summary(gmm_mod)


# Running GMM (iterative)
gmm_mod2 = gmm(
  # Moment restriction equations
  g = g1,
  # Matrix of data
  x = FinalData2,
  # Starting location for minimization algorithm
  t0 = c(0,0), # Required when g argument is a function
  type = "iterative",
  itermax = 1000
)

# Reporting results
summary(gmm_mod2)

# Running GMM (CUE)
gmm_mod3 = gmm(
  # Moment restriction equations
  g = g1,
  # Matrix of data
  x = FinalData2,
  # Starting location for minimization algorithm
  t0 = c(0,0), # Required when g argument is a function
  type = "cue"
)

# Reporting results
summary(gmm_mod3)

#New non-linear model
########################################################################

FinalData2$lambda2021 <- rep(0, 182)
FinalData2$lambda2021[FinalData2$Cons2021 - FinalData2$Cons2020 >= 0] <- 1

FinalData2$lambda2020 <- rep(0, 182)
FinalData2$lambda2020[FinalData2$Cons2020 - FinalData2$Cons2019 >= 0] <- 1

# Moment restrictions
g1 <- function(theta, FinalData2) {
  m1 = (FinalData2$Cons2021 - theta[1] * FinalData2$Income2020)
  m2 = (FinalData2$Cons2020 - theta[1] * FinalData2$Income2019)
  m3 = FinalData2$Invm2021 - theta[2] * (FinalData2$Cons2021 - FinalData2$Cons2020) - theta[3] * FinalData2$lambda2021 * (FinalData2$Cons2021 - FinalData2$Cons2020)
  m4 = FinalData2$Invm2020 - theta[2] * (FinalData2$Cons2020 - FinalData2$Cons2019) - theta[3] * FinalData2$lambda2020 * (FinalData2$Cons2020 - FinalData2$Cons2019)
  f = cbind(m1,m2,m3, m4)
  return(f)
}

# Running GMM
gmm_mod = gmm(
  # Moment restriction equations
  g = g1,
  # Matrix of data
  x = FinalData2,
  # Starting location for minimization algorithm
  t0 = c(0, 0, 0), # Required when g argument is a function
  type = "twoStep"
)

# Reporting results
summary(gmm_mod)


# Running GMM (iterative)
gmm_mod2 = gmm(
  # Moment restriction equations
  g = g1,
  # Matrix of data
  x = FinalData2,
  # Starting location for minimization algorithm
  t0 = c(0,0,0), # Required when g argument is a function
  type = "iterative",
  itermax = 5000
)

# Reporting results
summary(gmm_mod2)

# Running GMM (CUE)
gmm_mod3 = gmm(
  # Moment restriction equations
  g = g1,
  # Matrix of data
  x = FinalData2,
  # Starting location for minimization algorithm
  t0 = c(0,0,0), # Required when g argument is a function
  type = "cue"
)

# Reporting results
summary(gmm_mod3)

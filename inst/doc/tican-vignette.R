## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,      # inches
  fig.height = 5,     # inches
  dpi = 150,
  out.width = "100%", # scale image to column width
  fig.align = "center"
)

## ----setup--------------------------------------------------------------------
library(tican)

## ----include=FALSE------------------------------------------------------------
set.seed(123)
time <- seq(0, 50, by = 0.5)

# simulating data using gamma
gamma_tic <- function(time, amplitude, arrival_time, alpha, beta, plateau) {
  intensity <- numeric(length(time))
  for (i in seq_along(time)) {
    t <- time[i] - arrival_time
    if (t <= 0) {
      intensity[i] <- plateau
    } else {
      gamma_component <- amplitude * (t^alpha) * exp(-t/beta)
      intensity[i] <- plateau + gamma_component
    }
  }
  return(intensity)
}

# simulating data for 'regionA'
regionA_intensity <- gamma_tic(
  time = time,
  amplitude = 20,
  arrival_time = 5,
  alpha = 2,
  beta = 3,
  plateau = 0
) + rnorm(length(time), 0, 2.5)

# simulating data for 'regionB'
regionB_intensity <- gamma_tic(
  time = time,
  amplitude = 25,
  arrival_time = 3,
  alpha = 1.5,
  beta = 2,
  plateau = 0
) + rnorm(length(time), 0, 1)


example_data <- data.frame(
  time = time,
  regionA_intensity = regionA_intensity,
  regionB_intensity = regionB_intensity
)

## -----------------------------------------------------------------------------

# Showing structure of example dataframe

head(example_data,5)


## -----------------------------------------------------------------------------

# Analysing using defaults

result <- tic_analyse(example_data,"time","regionA_intensity")

print(result)


## -----------------------------------------------------------------------------
result <- tic_analyse(example_data,"time","regionA_intensity",
                           peakproportion = 0.9, #to calculate time to 90 percent peak
                           AUCmax = 30)

print(result)

## -----------------------------------------------------------------------------

result <- tic_analyse(example_data,"time","regionA_intensity",
                           calc_wir = TRUE,
                           calc_wor = TRUE
                      )

print(result)


## -----------------------------------------------------------------------------

result <- tic_analyse(example_data,"time","regionA_intensity",
                           loess.span = 0.15, # altering from default of 0.1
                           degree = 1) # adding a loess() argument

print(result)


## -----------------------------------------------------------------------------
results <- data.frame() #making empty dataframe to hold results

for(region in c("regionA_intensity","regionB_intensity")){
  resulttemp <- tic_analyse(example_data,"time",region) #storing results
  resulttemp$Region <- region # adding column for region
  results <- rbind(results, resulttemp) # combining results for different regions
}

print(results)


## -----------------------------------------------------------------------------

example_data2 <- example_data #creating a second dataframe

results <- data.frame() #making empty dataframe to hold results

for(df in c("example_data","example_data2")){
  resulttemp <- tic_analyse(get(df), # get() to get the dataframe object
                                 "time","regionA_intensity") 
  
  resulttemp$data <- df # adding column for which dataframe results are from
  results <- rbind(results, resulttemp) # combining results for different dataframes
}

print(results)



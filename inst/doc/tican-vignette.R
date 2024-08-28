## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(tican)

## -----------------------------------------------------------------------------

# Simulating example data
set.seed(123)
example_data <- data.frame(time = seq(0, 82, by = 0.25))
random_vals <- sample(1:10, nrow(example_data), replace = TRUE)
example_data$regionA_intensity <- log(example_data$time + 1) * 50 -
  example_data$time * 2 + random_vals
example_data$regionB_intensity <- log(example_data$time + 7, base = 10) *
  80 - example_data$time * 1.5 + random_vals

# Showing dataframe structure

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
                           loess.span = 0.5, # altering from default of 0.1
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



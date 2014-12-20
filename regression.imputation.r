## This function accepts a data frame and a computed regression model. 
## It will return the data frame with imputed values on the response variable.
## mod = regression model (simple or multiple)
## df = data frame
regression.imputation <- function(mod, df)
{

  # Stop the function if all values in the r.v are not missing
  if (all(!is.na(df[,names(mod$model)[1]])))
    stop("Nothing to impute!")

  # Copy the imputed data frame from the original data frame
  imputed.df <- df

  # Subset the columns by identifying the name of the model
  mod.df <- df[,names(df) %in% names(mod$model)]

  # Subset rows that contain missing data
  missing.df <- df[rowSums(is.na(mod.df)) > 0,]

  # Calculate the mean squared error
  mse <- mean(mod$residuals^2)

  # Count the number of missing data
  n <- sum(is.na(missing.df))  

  # Impute values with random generated error term
  imputed.values <- predict(mod, missing.df) + 
    rnorm(n, mean = 0, sd = sqrt(mse))

  # Subset the response variable into an un-imputed vector
  imputed.vec <- imputed.df[, names(mod$model[1])]

  # Impute the missing values in a vector
  imputed.vec[is.na(imputed.vec)] <- imputed.values

  # Store imputed values in original data frame
  imputed.df[,  names(mod$model[1])] <- imputed.vec

  # Return data frame with imputed values
  return(imputed.df)

}
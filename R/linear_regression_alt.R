#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A list containing coefficients, final_slope, and final_intercept
#'
#' @import dplyr
#'
#' @export

slr_gd <- function(dat, response, explanatory) {
  # Filters numeric variables
  vars_numeric <- sapply(dat, is.numeric)
  data_numeric <- dat[, vars_numeric]

  # Extract the response/explanatory variables
  y <- data_numeric[, response]
  X <- data_numeric[, explanatory]

  # Standardized vals
  X_mean <- mean(X)
  X_sd <- sd(X)
  X <- (X - X_mean) / X_sd

  # Initializes intercept/slope/learning rate/max_iterations variables
  intercept <- 0
  slope <- 0
  learning_rate <- 0.01
  max_iterations <- 10000

  # Create dataframe that's going to be returned
  coefficients <- data.frame(iteration = integer(max_iterations),
                             slope = double(max_iterations),
                             intercept = double(max_iterations)
                             )

  gradient_slope <- c()

  # Loop for # iterations
  for (iteration in 1:max_iterations) {
    # predictions
    y_pred <- intercept + slope * X

    # gradients
    gradient_slope[iteration] <- (-2 / length(y)) * sum(X * (y - y_pred))  # Corrected
    gradient_intercept <- (-2 / length(y)) * sum(y - y_pred)  # Corrected

    # Update
    slope_new <- slope - learning_rate * gradient_slope[iteration]
    intercept_new <- intercept - learning_rate * gradient_intercept

    # Update slope and intercept
    slope <- slope_new
    intercept <- intercept_new

    # Stores into dataframe
    coefficients[iteration, ] <- c(iteration,
                                   slope,
                                   intercept)

    if(gradient_slope[iteration] <= .00001){
      break
    }
  }

  # Set the final intercept and slope
  final_slope <- slope
  final_intercept <- intercept

  # Unstandardizes
  final_slope <- final_slope / X_sd
  final_intercept <- (final_intercept) + mean(y) / 2

  # Return list
  return(list(coefficients = coefficients,
              final_slope = final_slope,
              final_intercept = final_intercept,
              gradient_slope = gradient_slope))

  #Refrenced Chat GPT
}





#' Implements linear regression with many predictors by gradient descent
#'
#' This function computes coefficients for multiple regression by gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export

mlr_gd <- function(dat, response) {

  # Extract the response/explanatory variables
  y <- dat[[response]]
  X <- as.matrix(dat[, !colnames(dat) %in% response])

  # Standardizes vals
  X_mean <- colMeans(X)
  X_sd <- apply(X, 2, sd)
  X <- scale(X)

  # Initialize coefficients/learning rate/max_iterations
  coefficients <- rep(0, ncol(X))
  learning_rate <- 0.01
  max_iterations <- 10000
  gradients <- c()

  # Loop for # iterations
  for (iteration in 1:max_iterations) {
    # Compute predictions
    y_pred <- X %*% coefficients

    # Compute gradients
    gradients[iteration] <- (2 / length(y)) * t(X) %*% (y_pred - y)

    # Update coefficients
    coefficients_new <- coefficients - learning_rate * gradients[iteration]

    #Store
    coefficients <- coefficients_new

    if(gradients[iteration] <= .00001){
      break
    }
  }

  # Unstandardizes
  coefficients <- coefficients / X_sd
  intercept <- mean(y) - sum(X_mean * coefficients)

  # Return list
  return(list(coefficients = c(intercept,
                               coefficients),
              final_coefficients = coefficients,
              final_intercept = intercept,
              gradients = gradients))

  #Refrenced Chat GPT
}



#coefficients <- coefficients / X_sd
#final_intercept <- (final_intercept) + mean(y) / 2

#' Implements linear regression with many predictors by matrix decomposition
#'
#' This function computes coefficients for multiple regression by QR matrix decomposition
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export

mlr_qr <- function(dat, response) {



  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

    # Filter numeric variables
    vars_numeric <- sapply(dat, is.numeric)
    data_numeric <- dat[, vars_numeric]

    # Extract response/explanatory variables
    y <- data_numeric[, response]
    X <- data_numeric[, !colnames(data_numeric) %in% response]

    # Intercept Col
    X <- cbind(1, X)

    # QR decomposition
    qr_decomp <- qr(X)

    # Computes coefficients
    coefficients <- qr.coef(qr_decomp, y)

    # Makes dataframe that we are returning
    coefficients_store <- data.frame(coefficients = coefficients)
    results <- coefficients_store


  return(results)
  #Refrenced Chat GPT
  }

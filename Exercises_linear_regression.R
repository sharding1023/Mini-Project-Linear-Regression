library(ggplot2)

## Exercise: least squares regression
## 

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?


## My Answer: 
states.data <- readRDS("dataSets/states.rds") 
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])

##   1. Examine/plot the data before fitting the model
summary(states.data$metro)
summary(states.data$energy)
ggplot(states.data, aes(x = metro, y = energy)) + 
      geom_point()

##   2. Print and interpret the model `summary'
energy.mod <- lm(energy ~ metro,
              data=states.data) 
summary(energy.mod)

##   3. `plot' the model to look for deviations from modeling assumptions
plot(energy.mod)

##  I'm including /green/ (Per capita greenhouse gas, tons) as an additional predictor

 ## 1. 
    summary(states.data$metro)
    summary(states.data$energy)    
    summary(states.data$green)
    ggplot(states.data, aes(x = metro, y = energy)) + 
      geom_point()
    ggplot(states.data, aes(x = green, y = energy)) + 
      geom_point()
 ## 2. 
    energy.mod2 <- lm(energy ~ metro + green,
                     data=states.data) 
    summary(energy.mod2)
    
  ## 3. 
    plot(energy.mod2)
    
 ## This model is better because the R-squared value increased from 0.1154 to 0.5939. 
    
    
    
    
    ##_______________________________________________________________
## Exercise: interactions and factors

##   Use the states data set.

##   1. Add on to the regression equation that you created in exercise 1 by
##      generating an interaction term and testing the interaction.

##   2. Try adding region to the model. Are there significant differences
##      across the four regions?

## My Answer: 
    ##   1.     
    model <- lm(energy ~ metro*density, data=states.data) 
    coeff(summary(model))
    
    ##   2. 
    states.data$region <- factor(states.data$region)
    sat.region <- lm(energy ~ region,
                     data=states.data) 
    #Show the results
    coef(summary(sat.region)) # show regression coefficients table
    anova(sat.region) # show ANOVA table
    
    ## Since the ANOVA p-value is only 0.07737, there is not evidence of a significant difference 
    ## in per capita energy consumed across the four regions. 
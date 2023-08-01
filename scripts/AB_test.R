# AB testing

source("../random_bits/tidy_loader.R")
load_required_packages()

dat = read.csv("./data/website_results.csv",
               header = T)
str(dat)
# 'data.frame':	1451 obs. of  4 variables:
# $ variant       : chr  "A" "A" "A" "A" ...
# $ converted     : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
# $ length_of_stay: int  0 0 0 0 0 0 0 0 0 0 ...
# $ revenue       : num  0 0 0 0 0 0 0 0 0 0 ...

print(unique(dat$variant))
# [1] "A" "B"
# var A is the control group
# var B is the experimental group

# Null: both var A and B have equal prob of conversion or driving customer 
# booking.

# Alt: both var A and B have different probs of conversion or driving customer
# booking and there is a difference between both. var B is better than var A
# when it comes to driving customer bookings. 

## Filter conversions for each variant and computer their corresponding rates

conv_rate_A = dat %>%
  filter(variant == "A") %>%
  summarize(conversions_A = sum(converted),
            visitors_A = n()) %>%
  transmute(conv_rate_A = conversions_A / visitors_A)

print(conv_rate_A$conv_rate_A)
# [1] 0.02773925

conv_rate_B = dat %>%
  filter(variant == "B") %>%
  summarize(conversions_B = sum(converted),
            visitors_B = n()) %>%
  transmute(conv_rate_B = conversions_B / visitors_B)

print(conv_rate_B$conv_rate_B)
# [1] 0.05068493

## Calculate the "uplift" (percentage of increase)

uplift = (conv_rate_B - conv_rate_A) / conv_rate_A * 100
print(paste(round(uplift, 2), "%", sep = ""))
# [1] "82.72%"
# B is better than A by 83% approx.

## Compute the pooled probability, standard error, margin of error, and
# difference in proportion (point estimate) for both variants

# Calculate conversions_A and visitors_A for variant_A
result_A = dat %>%
  filter(variant == "A") %>%
  summarize(conversions_A = sum(converted),
            visitors_A = n())

# Extract the individual objects from the result
conversions_A = result_A$conversions_A
visitors_A = result_A$visitors_A

# then for variant_B
result_B = dat %>%
  filter(variant == "B") %>%
  summarize(conversions_B = sum(converted),
            visitors_B = n())

# Extract the individual objects from the result
conversions_B = result_B$conversions_B
visitors_B = result_B$visitors_B

# Calculate Pooled sample proportion for variants A & B
p_pool = (conversions_A + conversions_B) / (visitors_A + visitors_B)

# Compute Standard error for variants A & B (SE_pool)
SE_pool = sqrt(p_pool * (1 - p_pool) * ((1 / visitors_A) + (1 / visitors_B)))

# Compute the margin of error for the pool
MOE = SE_pool * qnorm(0.975)

# Point Estimate or Difference in proportion
d_hat = conv_rate_B - conv_rate_A

# Print the results
print(p_pool) # Pooled sample proportion
# [1] 0.03928325
print(SE_pool) # Standard error
# [1] 0.01020014
print(MOE) # Margin of error
# [1] 0.0199919

## Compute the Z-score to determine the p-value

z_score = d_hat/SE_pool
print(z_score$conv_rate_B)
# [1] 2.249546

## Compute the p-value
p_value = 2 * (1 - pnorm(abs(z_score$conv_rate_B)))
print(p_value)
# [1] 0.02447777

## Compute the confidence interval for the pool

ci = c(d_hat - MOE, d_hat + MOE)
print(ci) 
# $conv_rate_B
# [1] 0.002953777
# $conv_rate_B
# [1] 0.04293758

ci_lower = d_hat - MOE
ci_upper = d_hat + MOE

## compute the confidence interval for variants A separately
X_hat_A = conversions_A / visitors_A
se_hat_A = sqrt(X_hat_A * (1 - X_hat_A) / visitors_A)
ci_A = c(X_hat_A - qnorm(0.975) * se_hat_A, X_hat_A
          + qnorm(0.975) * se_hat_A)
print(ci_A) 
ci_A_lower = as.numeric(X_hat_A - qnorm(0.975) * se_hat_A)
ci_A_upper = as.numeric(X_hat_A + qnorm(0.975) * se_hat_A)
# [1] 0.01575201 0.03972649

## compute the confidence interval for variants B separately								
X_hat_B = conversions_B / visitors_B
se_hat_B = sqrt(X_hat_B * (1 - X_hat_B) / visitors_B)
ci_B = c(X_hat_B - qnorm(0.975) * se_hat_B,
          X_hat_B + qnorm(0.975) * se_hat_B)
print(ci_B) 
# [1] 0.03477269 0.06659717
ci_B_lower = as.numeric(X_hat_B - qnorm(0.975) * se_hat_B)
ci_B_upper = as.numeric(X_hat_B + qnorm(0.975) * se_hat_B)

# Visualise the results on a table
# Create the vis_result_pool data frame
vis_result_pool <- tibble(
  Metric = c(
    'Estimated Difference',
    'Relative Uplift(%)',
    'pooled sample proportion',
    'Standard Error of Difference',
    'z_score',
    'p-value',
    'Margin of Error',
    'CI-lower',
    'CI-upper'
  ),
  Value = c(
    as.numeric(conv_rate_B - conv_rate_A),
    as.numeric(uplift),
    as.numeric(p_pool),
    as.numeric(SE_pool),
    as.numeric(z_score),
    as.numeric(p_value),
    as.numeric(MOE),
    as.numeric(ci[1]),
    as.numeric(ci[2])
  )
)

# Print the tidy result
print(vis_result_pool)
# Metric                          Value
# <chr>                           <dbl>
# 1 Estimated Difference          0.0229 
# 2 Relative Uplift(%)            82.7    
# 3 pooled sample proportion      0.0393 
# 4 Standard Error of Difference  0.0102 
# 5 z_score                       2.25   
# 6 p-value                       0.0245 
# 7 Margin of Error               0.0200 
# 8 CI-lower                      0.00295
# 9 CI-upper                      0.0429 

# We can see that variant A has 20 conversions in 721 visits (2.77% conversion
# rate).
# Variant B has 37 conversions in 730 visits (5.07% conversion rate).
# The relative uplift of 82.72% based on the respective conversion rates 
# indicates that variant B is a better alternative by the value of the relative
# uplift.
# This result has strong statistical significance with a p-value of 0.02448.
# The recommendation is to accept variant B for deployment.

# Save the results as .Rds files
saveRDS(vis_result_pool, file = "./outputs/vis_result_pool.Rds")
saveRDS(conv_rate_A, file = "./outputs/conv_rate_A.Rds")
saveRDS(conv_rate_B, file = "./outputs/conv_rate_B.Rds")
saveRDS(ci_A_lower, file = "./outputs/ci_A_lower.Rds")
saveRDS(ci_A_upper, file = "./outputs/ci_A_upper.Rds")
saveRDS(ci_B_lower, file = "./outputs/ci_B_lower.Rds")
saveRDS(ci_B_upper, file = "./outputs/ci_B_upper.Rds")
saveRDS(SE_pool, file = "./outputs/SE_pool.Rds")


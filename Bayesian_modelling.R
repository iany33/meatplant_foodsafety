
pacman::p_load(brms,
               Matrix,
               tidyverse,  
               janitor,
               tidybayes, 
               bayesplot,
               marginaleffects,
               modelr,
               patchwork)


# Start with logistic model of overall audit pass rating outcome
# Start with model containing only operation type and number of employees, with varying effect for plant

get_prior(Rating2 ~ Operation_type.x + Num_employees_s + year + week_day + (1 | Plant_ID),
           data = audits, family = bernoulli)

priors <- c(prior(normal(0,1), class = "b"),
            prior(exponential(1), class = "sd"))


m1_prior <- brm(Rating2 ~ Operation_type.x + Num_employees_s + year + week_day + (1 | Plant_ID),
                data = audits, family = bernoulli, prior = priors,
                    iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
                    sample_prior = TRUE, backend = "cmdstanr", 
                    stan_model_args = list(stanc_options = list("O1")))

summary(m1_prior)
plot(m1_prior)
pp_check(m1_prior)
pp_check(m1_prior, type = "stat", stat = "mean", prefix = "ppd")
pp_check(m1_prior, ndraws=100, prefix = "ppd")
conditional_effects(m1_prior)

# Sample from posterior 

m1 <- brm(Rating2 ~ Operation_type.x + Num_employees_s + year + week_day + (1 | Plant_ID),
                data = audits, family = bernoulli, prior = priors,
                iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
                backend = "cmdstanr", 
                stan_model_args = list(stanc_options = list("O1")))

summary(m1)
plot(m1)
pp_check(m1, ndraws=100)
pp_check(m1, type = "stat", stat = "mean")
conditional_effects(m1)

# Add additional predictors

m2 <- brm(Rating2 ~ Operation_type.x + Num_employees_s + year + week_day + 
            Seasonal_operations + Municipal_water + Sausages + Blood_products +
            Dried_meats + Fermented_meats + Jerky + Wet_cured + Smoked + (1 | Plant_ID),
          data = audits, family = bernoulli, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(m2)
plot(m2)
pp_check(m2, ndraws=100)
pp_check(m2, type = "stat", stat = "mean")
mcmc_acf(m2, pars = vars(c(contains("employees"), contains("type"))), lags = 10)
mcmc_acf(m2, pars = vars(contains("Yes")), lags = 10)
mcmc_pairs(m2, pars = vars(contains("Yes")), diag_fun = "den", off_diag_fun = "hex")

conditional_effects(m2)

plot_predictions(m2, condition = "Num_employees_s")

### Examine fail/infraction rate outcome in separate model

get_prior(n_fail | rate(items_assessed) ~ Operation_type.x + Num_employees_s + 
            year + week_day + (1 | Plant_ID),
          data = audits, family = poisson)

priors <- c(prior(normal(0,1), class = "b"),
            prior(exponential(1), class = "sd"))

r1_prior <- brm(n_fail | rate(items_assessed) ~ Operation_type.x + Num_employees_s + 
                  year + week_day + (1 | Plant_ID),
                data = audits, family = poisson, prior = priors,
                iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
                sample_prior = TRUE, backend = "cmdstanr", 
                stan_model_args = list(stanc_options = list("O1")))

summary(r1_prior)
plot(r1_prior)
pp_check(r1_prior)
pp_check(r1_prior, type = "stat", stat = "mean", prefix = "ppd")
pp_check(r1_prior, ndraws=100, prefix = "ppd")
conditional_effects(r1_prior)

# Sample from posterior 

r1 <- brm(n_fail | rate(items_assessed) ~ Operation_type.x + Num_employees_s + 
            year + week_day + (1 | Plant_ID),
          data = audits, family = poisson, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(r1)
plot(r1)
pp_check(r1, type = "stat", stat = "mean")
pp_check(r1, ndraws=100)
conditional_effects(r1)

# Add additional predictors

r2 <- brm(n_fail | rate(items_assessed) ~ Operation_type.x + Num_employees_s + 
            year + week_day + Seasonal_operations + Municipal_water + Sausages + 
            Blood_products +Dried_meats + Fermented_meats + Jerky + Wet_cured + 
            Smoked + (1 | Plant_ID),
          data = audits, family = poisson, prior = priors,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(r2)
plot(r2)
pp_check(r2, ndraws=100)
pp_check(r2, type = "stat", stat = "mean")
mcmc_acf(r2, pars = vars(c(contains("employees"), contains("type"))), lags = 10)
mcmc_acf(r2, pars = vars(contains("Yes")), lags = 10)
mcmc_pairs(r2, pars = vars(contains("Yes")), diag_fun = "den", off_diag_fun = "hex")

conditional_effects(r2)

# Compare to negative binomial model to account for extra variation

get_prior(n_fail | rate(items_assessed) ~ Operation_type.x + Num_employees_s + 
            year + week_day + Seasonal_operations + Municipal_water + Sausages + 
            Blood_products +Dried_meats + Fermented_meats + Jerky + Wet_cured + 
            Smoked + (1 | Plant_ID),
          data = audits, family = negbinomial)

priors2 <- c(prior(normal(0,1), class = "b"),
            prior(exponential(1), class = "sd"),
            prior(exponential(1), class = "shape"))

r3 <- brm(n_fail | rate(items_assessed) ~ Operation_type.x + Num_employees_s + 
            year + week_day + Seasonal_operations + Municipal_water + Sausages + 
            Blood_products +Dried_meats + Fermented_meats + Jerky + Wet_cured + 
            Smoked + (1 | Plant_ID),
          data = audits, family = negbinomial, prior = priors2,
          iter = 2000, chains = 4, cores = 4, warmup = 1000, seed = 123, 
          backend = "cmdstanr", 
          stan_model_args = list(stanc_options = list("O1")))

summary(r3)
plot(r3)
pp_check(r3, ndraws=100)
pp_check(r3, type = "stat", stat = "mean")
mcmc_acf(r2, pars = vars(c(contains("employees"), contains("type"))), lags = 10)
mcmc_acf(r3, pars = vars(contains("Yes")), lags = 10)
mcmc_pairs(r3, pars = vars(contains("Yes")), diag_fun = "den", off_diag_fun = "hex")

loo(r2, r3)
# Neg binomial model has better fit

conditional_effects(r3)


### Now produce marginal effects of key parameters

audits |> rstatix::get_summary_stats(Num_employees, Num_employees_s)
quantile(audits$Num_employees_s, probs = c(0, 0.25, .50, 0.75, .95, 0.99), na.rm = TRUE)


get_variables(m2)


# Extract samples from posterior and plot draws at specific values of predictors
# Start with logistic model -> number of employees

avg_comparisons(m2)

nd <- audits |> data_grid(Num_employees_s = seq(-0.5, 9, by = 0.5),
                          Operation_type.x, year = 2019, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked) |> 
  distinct(Num_employees_s, .keep_all = TRUE)

nd <- nd |> mutate(Num_employees = round(Num_employees_s*sd(audits$Num_employees, na.rm = TRUE) + 
                                           mean(audits$Num_employees, na.rm = TRUE)))

pred <- predictions(m2, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

ggplot(pred, aes(x = Num_employees, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "Number of Employees",
       y = "Predicted Probability of Passing an Audit",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom")

avg_slopes(m2, re_formula = NA, type = "response", variables = "Num_employees_s", 
          newdata = nd)

# Operation type

nd <- audits |> data_grid(Num_employees_s = mean(Num_employees_s, na.rm=TRUE), 
                          Operation_type.x, year = 2019, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked) |> 
  distinct(Operation_type.x, .keep_all = TRUE)

pred <- predictions(m2, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

p1 <- ggplot(pred, aes(x = draw, fill = Operation_type.x)) +
  scale_fill_brewer(palette ="Dark2", name = "Operation Type") +
  stat_halfeye(slab_alpha = .5)  +
    labs(x = "Predicted Probability of Passing an Audit", y = "Probability Density",
       subtitle = "Posterior Predictions") +
  theme_minimal() +
  theme(legend.position = "bottom")  

avg_comparisons(m2, re_formula = NA, type = "response", variables = "Operation_type.x", 
            newdata = nd)

mfx <- comparisons(m2, re_formula = NA, type = "response", variables = "Operation_type.x", 
                   newdata = nd) |> posteriordraws()

p2 <- ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5, fill = "#7570B3")  +
  labs(x = "Effect of Operation Type on Passing Probability", y = "",
       subtitle = "Contrast (FSMP vs. Abattoir)") +
  theme_minimal() +
  theme(legend.position = "bottom") 

p1 + p2

# Municipal water

nd <- audits |> data_grid(Num_employees_s = mean(Num_employees_s, na.rm=TRUE), 
                          Operation_type.x, year, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked) |> 
  distinct(Municipal_water,  year, .keep_all = TRUE)

pred <- predictions(m2, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

p1 <- ggplot(pred, aes(x = draw, fill = Municipal_water)) +
  scale_fill_brewer(palette ="Dark2", name = "Municipal water") +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted Probability of Passing an Audit", y = "Probability Density",
       subtitle = "Posterior Predictions") +
  theme_minimal() +
  theme(legend.position = "bottom")  

avg_comparisons(m2, re_formula = NA, type = "response", variables = "Municipal_water", 
            newdata = nd) 

mfx <- comparisons(m2, re_formula = NA, type = "response", variables = "Municipal_water", 
                   newdata = nd) |> posteriordraws()

p2 <- ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5, fill = "#7570B3")  +
  labs(x = "Effect of Municipal Water on Passing Probability", y = "",
       subtitle = "Contrast (Yes vs. No)") +
  theme_minimal() +
  theme(legend.position = "bottom") 

p1 + p2

# Jerky production

nd <- audits |> data_grid(Num_employees_s = mean(Num_employees_s, na.rm=TRUE), 
                          Operation_type.x, year = 2019, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked) |> 
  distinct(Jerky, .keep_all = TRUE)

pred <- predictions(m2, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

p1 <- ggplot(pred, aes(x = draw, fill = Jerky)) +
  scale_fill_brewer(palette ="Dark2", name = "Jerky Production") +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted Audit Item Fail Rate", y = "Probability Density",
       subtitle = "Posterior Predictions") +
  theme_minimal() +
  theme(legend.position = "bottom")  

avg_comparisons(m2, re_formula = NA, type = "response", variables = "Jerky", 
                newdata = nd) 

mfx <- comparisons(m2, re_formula = NA, type = "response", variables = "Jerky", 
                   newdata = nd) |> posteriordraws()

p2 <- ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5, fill = "#7570B3")  +
  labs(x = "Effect of Jerky Production on Audit Item Fail Rate", y = "",
       subtitle = "Contrast (Yes vs. No)") +
  theme_minimal() +
  theme(legend.position = "bottom") 

p1 + p2


### Effects for fail/infraction rate model

avg_comparisons(r2)

# Number of employees

nd <- audits |> data_grid(Num_employees_s = seq(-0.5, 9, by = 0.5),
                          Operation_type.x, year = 2019, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked, items_assessed = 1) |> 
  distinct(Num_employees_s, .keep_all = TRUE)

nd <- nd |> mutate(Num_employees = round(Num_employees_s*sd(audits$Num_employees, na.rm = TRUE) + 
                                           mean(audits$Num_employees, na.rm = TRUE)))

pred <- predictions(r3, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

ggplot(pred, aes(x = Num_employees, y = draw)) +
  stat_lineribbon() +
  scale_fill_brewer(palette = "Purples") +
  labs(x = "Number of Employees",
       y = "Predicted Audit Item Fail Rate",
       fill = "") +
  theme_classic() + 
  theme(legend.position = "bottom")

avg_slopes(r2, re_formula = NA, type = "response", variables = "Num_employees_s", 
           newdata = nd)

# Operation type

nd <- audits |> data_grid(Num_employees_s = mean(Num_employees_s, na.rm=TRUE), 
                          Operation_type.x, year = 2019, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked, items_assessed = 1) |> 
  distinct(Operation_type.x, .keep_all = TRUE)

pred <- predictions(r3, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

p1 <- ggplot(pred, aes(x = draw, fill = Operation_type.x)) +
  scale_fill_brewer(palette ="Dark2", name = "Operation Type") +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted Audit Item Fail Rate", y = "Probability Density",
       subtitle = "Posterior Predictions") +
  theme_minimal() +
  theme(legend.position = "bottom")  

avg_comparisons(r3, re_formula = NA, type = "response", variables = "Operation_type.x", 
            newdata = nd) 

mfx <- comparisons(r3, re_formula = NA, type = "response", variables = "Operation_type.x", 
                   newdata = nd) |> posteriordraws()

p2 <- ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5, fill = "#7570B3")  +
  labs(x = "Effect of Operation Type on Audit Item Fail Rate", y = "",
       subtitle = "Contrast (FSMP vs. Abattoir)") +
  theme_minimal() +
  theme(legend.position = "bottom") 

p1 + p2

# Jerky production

nd <- audits |> data_grid(Num_employees_s = mean(Num_employees_s, na.rm=TRUE), 
                          Operation_type.x, year = 2019, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked, items_assessed = 1) |> 
  distinct(Jerky, .keep_all = TRUE)

pred <- predictions(r3, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

p1 <- ggplot(pred, aes(x = draw, fill = Jerky)) +
  scale_fill_brewer(palette ="Dark2", name = "Jerky Production") +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted Audit Item Fail Rate", y = "Probability Density",
       subtitle = "Posterior Predictions") +
  theme_minimal() +
  theme(legend.position = "bottom")  

avg_comparisons(r3, re_formula = NA, type = "response", variables = "Jerky", 
            newdata = nd) 

mfx <- comparisons(r3, re_formula = NA, type = "response", variables = "Jerky", 
                   newdata = nd) |> posteriordraws()

p2 <- ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5, fill = "#7570B3")  +
  labs(x = "Effect of Jerky Production on Audit Item Fail Rate", y = "",
       subtitle = "Contrast (Yes vs. No)") +
  theme_minimal() +
  theme(legend.position = "bottom") 

p1 + p2

# Municipal water

nd <- audits |> data_grid(Num_employees_s = mean(Num_employees_s, na.rm=TRUE), 
                          Operation_type.x, year = 2019, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked, items_assessed = 1) |> 
  distinct(Municipal_water, .keep_all = TRUE)

pred <- predictions(r3, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

p1 <- ggplot(pred, aes(x = draw, fill = Municipal_water)) +
  scale_fill_brewer(palette ="Dark2", name = "Municipal Water") +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted Audit Item Fail Rate", y = "Probability Density",
       subtitle = "Posterior Predictions", fill = "Municipal water") +
  theme_minimal() +
  theme(legend.position = "bottom")  

avg_comparisons(r3, re_formula = NA, type = "response", variables = "Municipal_water", 
            newdata = nd) 

mfx <- comparisons(r3, re_formula = NA, type = "response", variables = "Municipal_water", 
                   newdata = nd) |> posteriordraws()

p2 <- ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5, fill = "#7570B3")  +
  labs(x = "Effect of Municipal Water Source on Audit Item Fail Rate", y = "",
       subtitle = "Contrast (Yes vs. No)") +
  theme_minimal() +
  theme(legend.position = "bottom") 

p1 + p2

# Smoked products

nd <- audits |> data_grid(Num_employees_s = mean(Num_employees_s, na.rm=TRUE), 
                          Operation_type.x, year = 2019, week_day,
                          Seasonal_operations, Municipal_water, Sausages,
                          Blood_products, Dried_meats, Fermented_meats, Jerky, 
                          Wet_cured, Smoked, items_assessed = 1) |> 
  distinct(Smoked, .keep_all = TRUE)

pred <- predictions(r3, re_formula = NA, type = "response",
                    newdata = nd) |> 
  posteriordraws()

p1 <- ggplot(pred, aes(x = draw, fill = Smoked)) +
  scale_fill_brewer(palette ="Dark2", name = "Smoked Products") +
  stat_halfeye(slab_alpha = .5)  +
  labs(x = "Predicted Audit Item Fail Rate", y = "Probability Density",
       subtitle = "Posterior Predictions", fill = "Smoked Products") +
  theme_minimal() +
  theme(legend.position = "bottom")  

avg_comparisons(r3, re_formula = NA, type = "response", variables = "Smoked", 
                newdata = nd) 

mfx <- comparisons(r3, re_formula = NA, type = "response", variables = "Smoked", 
                   newdata = nd) |> posteriordraws()

p2 <- ggplot(mfx, aes(x = draw, fill = contrast)) +
  stat_halfeye(slab_alpha = .5, fill = "#7570B3")  +
  labs(x = "Effect of Smoked Products on Audit Item Fail Rate", y = "",
       subtitle = "Contrast (Yes vs. No)") +
  theme_minimal() +
  theme(legend.position = "bottom") 

p1 + p2





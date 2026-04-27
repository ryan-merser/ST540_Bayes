# Seed vs Rank

library(tidyverse)
library(runjags)
library(rjags)
library(lme4)

set.seed(123)

years <- 2019:2023
base_url <- "https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_"

atp_raw <- map_dfr(years, function(yr) {
  url <- paste0(base_url, yr, ".csv")
  read_csv(url, show_col_types = FALSE)
})
#---------------------------Draw only needed columns-------------------------------
# Keep only the columns we need, filter to top players and 3 surfaces
atp_clean_seed <- atp_raw |>
  select(winner_name, loser_name, surface, tourney_date, winner_seed, loser_seed) |> filter(surface %in% c("Clay", "Hard", "Grass")) |>
  drop_na()

dim(atp_clean_seed)

atp_clean_rank <- atp_raw |>
  select(winner_name, loser_name, surface, tourney_date, winner_rank, loser_rank) |>
  filter(surface %in% c("Clay", "Hard", "Grass")) |>
  drop_na()

dim(atp_clean_rank)

# Notice the difference in dimensions

#-----------------------------------------------------------------------------
# Creating rank difference (loser - winner)
temp1 = atp_clean_rank |>
  mutate(rank_diff = loser_rank - winner_rank, .keep = "unused")

temp2 = atp_clean_seed |>
  mutate(seed_diff = loser_seed - winner_seed, .keep = "unused")

# Pivoting longer to make every player have a row
long_df1 = temp1 |>
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) |>
  mutate(outcome = if_else(role == "winner_name", 1, 0)) |>  # 1 if winner, 0 if loser
  mutate(rank_diff = if_else(outcome == 1, rank_diff, -rank_diff)) |> # Rank_diff now opponent - player
  select(player, outcome, surface, tourney_date, rank_diff) # Selecting only variables of interest

long_df2 = temp2 |>
  pivot_longer(
    cols = c(winner_name, loser_name),
    names_to = "role",
    values_to = "player"
  ) |>
  mutate(outcome = if_else(role == "winner_name", 1, 0)) |>  # 1 if winner, 0 if loser
  mutate(rank_diff = if_else(outcome == 1, seed_diff, -seed_diff)) |> # Rank_diff now opponent - player
  select(player, outcome, surface, tourney_date, seed_diff) # Selecting only variables of interest
#--------------------------------------------------------------------------
# Converting the character values to unique numeric integers for jags to use
jags_df1 = long_df1 |>
  mutate(across(c(player, surface), as.factor)) |> # removed outcome factor (leave as original 0/1 encoder)
  mutate(across(c(player, surface), as.numeric)) |> 
  select(-tourney_date)

# Players who have played >= 50 games
top_players1 = jags_df1 |>
  group_by(player) |>
  summarize(count = n()) |>
  filter(count >= 50) |>
  pull(player)

top_df1 = jags_df1 |> 
  filter(player %in% top_players1) |>
  mutate(player = as.numeric(as.factor(player))) # re-index


# Players who have played 10 <= games < 50
mid_players1 = jags_df1 |>
  group_by(player) |>
  summarize(count = n()) |>
  filter(count >= 10 & count <50) |>
  pull(player)

mid_df1 = jags_df1 |> 
  filter(player %in% mid_players1) |>
  mutate(player = as.numeric(as.factor(player)))
#-------------------------------------------------------------------------------

jags_df2 = long_df2 |>
  mutate(across(c(player, surface), as.factor)) |> # removed outcome factor (leave as original 0/1 encoder)
  mutate(across(c(player, surface), as.numeric)) |> 
  select(-tourney_date)

# Players who have played >= 50 games
top_players2 = jags_df2 |>
  group_by(player) |>
  summarize(count = n()) |>
  filter(count >= 50) |>
  pull(player)

top_df2 = jags_df2 |> 
  filter(player %in% top_players2) |>
  mutate(player = as.numeric(as.factor(player))) # re-index


# Players who have played 10 <= games < 50
mid_players2 = jags_df2 |>
  group_by(player) |>
  summarize(count = n()) |>
  filter(count >= 10 & count <50) |>
  pull(player)

mid_df2 = jags_df2 |> 
  filter(player %in% mid_players2) |>
  mutate(player = as.numeric(as.factor(player)))

#========================Distributional Differences============================

# Seed
ggplot(top_df2, aes(seed_diff, col="top")) + 
  geom_density() + 
  facet_grid(~surface) +
  geom_density(data=mid_df2, aes(col="mid"))
# Similar distributions in seed

# Rank
ggplot(top_df1, aes(rank_diff, col="top")) + 
  geom_density() + 
  facet_grid(~surface) +
  geom_density(data=mid_df1, aes(col="mid"))

# Looking closer
ggplot(top_df1, aes(rank_diff, col="top")) + 
  geom_density() + 
  facet_grid(~surface) +
  geom_density(data=mid_df1, aes(col="mid")) + 
  xlim(-250, 250)
# Systematic difference in rank
# Assumption that they both come from the same population is violated
# We might be able to use rank if we limit the extreme values

#--------------Fixing Rank?------------------------------

# Tabular summaries
summary(top_df1)[, c(1, 4)]
quantile(top_df1$rank_diff, c(.025, .975))
table(top_df1 |> select(outcome, surface))

summary(mid_df1)[, c(1, 4)]
quantile(mid_df1$rank_diff, c(.025, .975))
table(mid_df1 |> select(outcome, surface))

tmpt = top_df1 |>
  filter(rank_diff >= quantile(top_df1$rank_diff, .025) & rank_diff <= quantile(top_df1$rank_diff, .975))
tmpm = mid_df1 |>
  filter(rank_diff >= quantile(mid_df1$rank_diff, .025) & rank_diff <= quantile(mid_df1$rank_diff, .975))

# Looking at how the trimmed values compare. 
# Still the systemic differences
# We can't combine bc the top_df overwhelms the mid_df
ggplot(tmpt, aes(rank_diff, col="top")) + 
  geom_density() + 
  facet_grid(~surface) +
  geom_density(data=tmpm, aes(col="mid")) + 
  geom_density(data = jags_df1, aes(col = "all")) +
  xlim(-350, 350)

# We would probably need to limit the mid_df to only be within the 95% range of the top_df values in order to make good predictions, 
# but our normality assumption is violated
tmpm = mid_df1 |>
  filter(rank_diff >= quantile(top_df1$rank_diff, .025) & rank_diff <= quantile(top_df1$rank_diff, .975))
ggplot(tmpt, aes(rank_diff, col="top")) + 
  geom_density() + 
  facet_grid(~surface) +
  geom_density(data=tmpm, aes(col="mid")) 

#----------Tabular summaries for seed --------------------
summary(top_df2)[, c(1, 4)]
table(top_df2 |> select(outcome, surface))

summary(mid_df2)[, c(1, 4)]
table(mid_df2 |> select(outcome, surface))

# Issues with using seed over rank
  # 1) A lot less data
  # 2) Limits to only seeded matches, which limits the scope of the investigation

#--------Interaction plots-------------------------------------------------

interaction1 <- long_df1 |>
  select(-tourney_date, -rank_diff) |>
  group_by(surface, player) |>
  summarize(outcome = mean(outcome), .groups = "drop")

interaction2 <- long_df2 |>
  select(-tourney_date, -seed_diff) |>
  group_by(surface, player) |>
  summarize(outcome = mean(outcome), .groups = "drop")

ggplot(interaction1, aes(x = surface, y = outcome, group = player, color = player)) +
  geom_line() +
  geom_point() + 
  theme(legend.position = "none")

ggplot(interaction2, aes(x = surface, y = outcome, group = player, color = player)) +
  geom_line() +
  geom_point() + 
  theme(legend.position = "none")

# Same plots, but with different amounts of data. We could potentially group the players up based on these interactions
# It looks like the players who do well on grass do not do well on clay and hard and vice versa

# There seems to be evidence of interaction, but it could be noise

#==============================================================================
# Preliminary Frequentist calcualtions to see if the model is ok

#--------------------Rank---------------------------------------------
temp = top_df1 |> mutate(across(c(player, outcome, surface), as.factor))
fit1 = glmer(outcome ~ 0+rank_diff + surface + (1 | player) + (1|(surface:player)), data = temp, family = "binomial")
summary(fit1)

fit2 = glmer(outcome ~ 0+rank_diff + surface + (1 | player), data = temp, family = "binomial")
summary(fit2)
# Model is worse with no interaction (Based on AIC)

preds = predict(fit1, type = "response")
preds = ifelse(preds > .5, 1, 0)
table(top_df1$outcome, preds)
Accuracy = sum(diag(table(top_df1$outcome, preds)))/sum(table(top_df1$outcome, preds))
Accuracy #64% accuracy on training set


#-------------------------Seed---------------------------------------------
temp = top_df2 |> mutate(across(c(player, outcome, surface), as.factor))
fit3 = glmer(outcome ~ 0+seed_diff + surface + (1 | player) + (1|(surface:player)), data = temp, family = "binomial")
summary(fit3)

fit4 = glmer(outcome ~ 0+seed_diff + surface + (1 | player), data = temp, family = "binomial")
summary(fit4)
# Model performance is similar with no interaction (Based on AIC)
# I would still include it in this model bc it is less data, so less compuationally difficult

preds = predict(fit3, type = "response")
preds = ifelse(preds > .5, 1, 0)
table(top_df2$outcome, preds)
Accuracy = sum(diag(table(top_df2$outcome, preds)))/sum(table(top_df2$outcome, preds))
Accuracy #62% accuracy on training set

#==========================Top PlayerJAGS=======================================
data1 = list(
  N = nrow(top_df1), 
  Y = top_df1$outcome,
  player = top_df1$player,
  surface = top_df1$surface, 
  X = top_df1$rank_diff |> scale() |> as.numeric(),
  n_players = length(unique(top_df1$player)), 
  n_surfaces = length(unique(top_df1$surface))
)

data2 = list(
  N = nrow(top_df2), 
  Y = top_df2$outcome,
  player = top_df2$player,
  surface = top_df2$surface, 
  X = top_df2$seed_diff |> scale() |> as.numeric(),
  n_players = length(unique(top_df2$player)), 
  n_surfaces = length(unique(top_df2$surface))
)

model_string_interaction = "model{

  #Likelihood
  for(i in 1:N){
    Y[i] ~ dbern(p[i])
    logit(p[i]) <-  alpha[surface[i]] + beta*X[i] + 
        A[player[i]] + B[player[i], surface[i]] 
  }
  
  for(i in 1:n_players){
  
    # Player effect
    A[i] ~ dnorm(0, tau_A)
    
    for(j in 1:n_surfaces){
    
    # Interaction effect
      B[i, j] ~ dnorm(0, tau_inter)
    }
  }
  
  # Surface effect
  for(j in 1:n_surfaces){
    alpha[j] ~ dnorm(0, .25)     ## SD = 2
  }
  
  beta ~ dnorm(0, .25)
  
  sigma_A ~ dt(0., 1., 4)T(0., )  ## Half-T with scale=1 and 4 df, changed to prevent overflow
  sigma_inter ~ dt(0., 1., 4)T(0., )
  tau_A <- 1/sigma_A^2
  tau_inter <- 1/sigma_inter^2
}"

interaction_fit1 = run.jags(model = model_string_interaction, data=data1, 
                           monitor=c("alpha", "beta", "sigma_A", "sigma_inter"),
                           n.chains=4, burnin=10000, sample=20000, modules="glm", 
                           method="parallel")
interaction_fit1
samps1 = as.mcmc.list(interaction_fit1)
summary(samps1)
DIC_inter1 = extract(interaction_fit1, what = "dic")
DIC_inter1

interaction_fit2 = run.jags(model = model_string_interaction, data=data2, 
                            monitor=c("alpha", "beta", "sigma_A", "sigma_inter"),
                            n.chains=4, burnin=10000, sample=20000, modules="glm", 
                            method="parallel")
interaction_fit2
samps2 = as.mcmc.list(interaction_fit2)
summary(samps2)
DIC_inter2 = extract(interaction_fit2, what = "dic")
DIC_inter2
#-------------------------------------------------------------------------------
model_string_add = "model{

  #Likelihood
  for(i in 1:N){
    Y[i] ~ dbern(p[i])
    logit(p[i]) <-  alpha[surface[i]] + beta*X[i] + A[player[i]] 
  }
  
  for(i in 1:n_players){
  
    # Player effect
    A[i] ~ dnorm(0, tau_A)
    
  }
  
  # Surface effect
  for(j in 1:n_surfaces){
    alpha[j] ~ dnorm(0, .25)     ## SD = 2
  }
  
  beta ~ dnorm(0, .25)
  
  sigma_A ~ dt(0., 1., 4)T(0., )  ## Half-T with scale=1 and 4 df, changed to prevent overflow
  sigma_inter ~ dt(0., 1., 4)T(0., )
  tau_A <- 1/sigma_A^2
}"

add_fit1 = run.jags(model = model_string_add, data=data1, 
                            monitor=c("alpha", "beta", "sigma_A", "sigma_inter"),
                            n.chains=4, burnin=10000, sample=20000, modules="glm", 
                            method="parallel")
add_fit1
samps3 = as.mcmc.list(add_fit1)
summary(samps3)
DIC_add1 = extract(add_fit1, what = "dic")
DIC_add1

add_fit2 = run.jags(model = model_string_add, data=data2, 
                            monitor=c("alpha", "beta", "sigma_A", "sigma_inter"),
                            n.chains=4, burnin=10000, sample=20000, modules="glm", 
                            method="parallel")
add_fit2
samps4 = as.mcmc.list(add_fit2)
summary(samps4)
DIC_add2 = extract(add_fit2, what = "dic")
DIC_add2


#========================Bringing over Priors======================================

# Winning models
results1 = rbind(samps1[[1]], samps1[[2]], samps1[[3]], samps1[[4]])
results2 = rbind(samps3[[1]], samps3[[2]], samps3[[3]], samps3[[4]])

#Hyperparameters for the sigmas
nlogL.gamma = function(theta, x){
  # Negative log-likelihood (that we want to minimize)
  -sum(dgamma(x, shape=exp(theta[1]), rate=exp(theta[2]), log=TRUE))
}

sig_a1 = results1[, 4]
fit_a1 = optim( c(0, 0), nlogL.gamma, x=1/sig_a1^2, method="BFGS" )
sig_int1 = results1[, 5]
fit_i1 = optim(c(0, 0), nlogL.gamma, x = 1/sig_i1^2, method = "BFGS")


sig_a2 = results2[, 4]
fit_a2 = optim( c(0, 0), nlogL.gamma, x=1/sig_a2^2, method="BFGS" )
sig_int2 = results2[, 5]

# Without trimming, the plot does not work
lower <- quantile(sig_int2, 0.025, na.rm = TRUE)

sig_i_trimmed <- sig_int2[sig_int2 >= lower]
fit_i2 = optim(c(0, 0), nlogL.gamma, x = 1/sig_i_trimmed^2, method = "BFGS")

# Showing that the distribution matches the posterior we produced
plot(density(1/sig_a1^2), main = "Rank 1/Sigma_A^2")
lines(density(rgamma(10000, shape=exp(fit_a1$par[1]), rate=exp(fit_a1$par[2]))), col="red")

plot(density(1/sig_i1^2), main = "1/Sigma_Interaction^2", xlim = c(0, 500))
lines(density(rgamma(10000, shape=exp(fit_i1$par[1]),
                     rate=exp(fit_i1$par[2]))), col="red")

plot(density(1/sig_a2^2), main = "Rank 1/Sigma_A^2")
lines(density(rgamma(10000, shape=exp(fit_a2$par[1]), rate=exp(fit_a2$par[2]))), col="red")

plot(density(1/sig_i_trimmed^2), main = "1/Sigma_Interaction^2", xlim = c(0, 500))
lines(density(rgamma(10000, shape=exp(fit_i2$par[1]),
                     rate=exp(fit_i2$par[2]))), col="red")

hyper_sigs1 = c(sig_aa, sig_ab, sig_ia, sig_ib)
hyper_sigs1 = c(exp(fit_a1$par[1]), exp(fit_a1$par[2]), 
                exp(fit_i1$par[1]), exp(fit_i1$par[2]))
hyper_sigs2 = c(exp(fit_a2$par[1]), exp(fit_a2$par[2]), 
                exp(fit_i2$par[1]), exp(fit_i2$par[2]))
hyper_norms1 = summary(samps1)$statistics[1:4, 1:2]
hyper_norms2 = summary(samps3)$statistics[1:4, 1:2]

#==================Fitting Mid Players=========================================
model_string_use = "model{

  #Likelihood
  for(i in 1:N){
    Y[i] ~ dbern(p[i])
    logit(p[i]) <-  alpha[surface[i]] + beta*X[i] + 
        A[player[i]] + B[player[i], surface[i]] 
  }
  
  for(i in 1:n_players){
  
    # Player effect
    A[i] ~ dnorm(0, tau_A)
    
    for(j in 1:n_surfaces){
    
    # Interaction effect
      B[i, j] ~ dnorm(0, tau_inter)
    }
  }
  
  # Surface effect
  alpha[1] ~ dnorm(hyper_norms[1, 1], 1/hyper_norms[1, 2]^2)
  alpha[2] ~ dnorm(hyper_norms[2, 1], 1/hyper_norms[2, 2]^2)
  alpha[3] ~ dnorm(hyper_norms[3, 1], 1/hyper_norms[2, 2]^2)
  
  beta ~ dnorm(hyper_norms[4, 1], 1/hyper_norms[4, 2]^2)
  
  tau_A ~ dgamma(hyper_sigs[1], hyper_sigs[2])
  tau_inter ~ dgamma(hyper_sigs[3], hyper_sigs[4])
}"

# Can only 
data3 = list(
  N = nrow(mid_df), 
  Y = mid_df$outcome,
  player = mid_df$player,
  surface = mid_df$surface, 
  X = mid_df$rank_diff |> scale() |> as.numeric(),
  n_players = length(unique(mid_df$player)), 
  n_surfaces = length(unique(mid_df$surface)), 
  hyper_sigs = hyper_sigs, 
  hyper_norms = hyper_norms
)

data4 = list(
  N = nrow(mid_df), 
  Y = mid_df$outcome,
  player = mid_df$player,
  surface = mid_df$surface, 
  X = mid_df$rank_diff |> scale() |> as.numeric(),
  n_players = length(unique(mid_df$player)), 
  n_surfaces = length(unique(mid_df$surface)), 
  hyper_sigs = hyper_sigs, 
  hyper_norms = hyper_norms
)

mid_fit = run.jags(model = model_string_use, data=data, 
                   monitor=c("beta", "A", "B", "alpha"),
                   n.chains=4, burnin=10000, sample=20000, modules="glm", 
                   method="parallel")
summary = summary(mid_fit)
max(summary[, 11])
min(summary[, 11])
summary(summary)

mid_samps = as.mcmc.list(mid_fit)

rez = rbind(mid_samps[[1]], mid_samps[[2]], mid_samps[[3]], mid_samps[[4]])
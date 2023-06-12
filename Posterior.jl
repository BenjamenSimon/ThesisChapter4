
include("Likelihood.jl")

###########################
### Prior Distributions ###
###########################

function Infection_prior(epi_params_dists, epi_params::Vector{Float64})

  β_c_prior::Float64 = logpdf(epi_params_dists[1], epi_params[1])
  β_b_prior::Float64 = logpdf(epi_params_dists[2], epi_params[2])
  γ_prior::Float64 = logpdf(epi_params_dists[3], epi_params[3])
  F_prior::Float64 = logpdf(epi_params_dists[4], epi_params[4])
  # EPSILON?

  log_prior::Float64 = β_c_prior + β_b_prior + γ_prior + F_prior

  return(log_prior::Float64)
end

function Detection_prior(epi_params_dists, epi_params::Vector{Float64})

  ρ_prior::Float64 = logpdf(epi_params_dists[6], epi_params[6])
  ρ_E_prior::Float64 = logpdf(epi_params_dists[7], epi_params[7])

  log_prior::Float64 = ρ_prior + ρ_E_prior

  return(log_prior::Float64)
end

function Badger_birth_death_prior(epi_params_dists, epi_params::Vector{Float64})

  θbb_prior::Float64 = logpdf(epi_params_dists[8], epi_params[8])
  θbd_prior::Float64 = logpdf(epi_params_dists[9], epi_params[9])

  log_prior::Float64 = θbb_prior + θbd_prior

  return(log_prior::Float64)
end


#########################################
### Parameter Posterior Distributions ###
#########################################

# NOTE: These functions will only calculate the appropriate posterior
# One needs to do that for each the cur and prime on every draw
# because we are always looking at a unique selection to save cost

function Infection_params_posterior(llh_array_prime, p_env_llh_array_prime, scope::Scope, log_prior_dists, epi_params_prime)

  log_prior_prime::Float64 = Infection_prior(log_prior_dists, epi_params_prime)

  post_prime::Float64 = calc_llh_h_and_p(scope, llh_array_prime, p_env_llh_array_prime) + log_prior_prime

  return(post_prime::Float64)
end

function Detection_params_posterior(llh_array_prime, p_env_llh_array_prime, scope::Scope, log_prior_dists, epi_params_prime)

  log_prior_prime::Float64 = Detection_prior(log_prior_dists, epi_params_prime)

  post_prime::Float64 = calc_llh_h(scope, llh_array_prime) + log_prior_prime

  return(post_prime::Float64)
end

function Badger_birth_death_params_posterior(llh_array_prime, p_env_llh_array_prime, scope::Scope, log_prior_dists, epi_params_prime)

  log_prior_prime::Float64 = Badger_birth_death_prior(log_prior_dists, epi_params_prime)

  post_prime::Float64 = calc_llh_h_and_p(scope, llh_array_prime, p_env_llh_array_prime) + log_prior_prime

  return(post_prime::Float64)
end


#################################################
### Data Augmentation Posterior Distributions ###
#################################################

function generic_posterior_SE_dataaug(llh_array_prime, p_env_llh_array_prime, scope::Scope, epi_params_prime)

  # Used for MoveSE, AddRemSE

  post_prime = calc_llh_h(scope, llh_array_prime)

  return(post_prime)
end

function generic_posterior_dataaug(llh_array_prime, p_env_llh_array_prime, scope::Scope, epi_params_prime)

  # Used for MoveEI, AddRemEI, AddRemDet, AddRemDeath, AddRemMoves, AddRemPenv, AddRemBadgerBirths, AddRemBadgerDeaths

  post_prime = calc_llh_h_and_p(scope, llh_array_prime, p_env_llh_array_prime)

  return(post_prime)
end

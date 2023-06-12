
#######################
### DATA STRUCTURES ###
#######################

function create_results_arrays(N_its, epi_params_true)

  number_of_parameters = size(epi_params_true, 1)

  res = fill(-99., N_its, number_of_parameters)
  # :β_c, :β_b, :γ, :F, :ϵ, :ρ, :ρ_E, :θ_bb, :θ_bd

  other_res = fill(-99., N_its, 16)
  # :is_accepted_inf, :log_α_ratio_inf, :post_inf, :post_prime_inf, :reason_inf
  # :is_accepted_det, :log_α_ratio_det, :post_det, :post_prime_det, :reason_det
  # :is_accepted_badger, :log_α_ratio_badger, :post_badger, :post_prime_badger, :reason_badger
  # :sample

  aug_res = fill(-99., N_its, 71)
  # [is_accepted, log_α_ratio, post, post_prime, reason] for
  # Move SE, Move EI, Add/Rem SE, Add/Rem EI
  # Add/Rem Det, Add/Rem Death, Add/Rem penv, Add/Rem Moves
  # Move badger SE, Move badger EI, Add/Rem badger SE, Add/Rem badger EI
  # Add/Rem badger Death, Add/Rem badger Birth
  # then sample

  tuning_res = fill(-99., N_its, 7)
  # :λ_inf, :λ_det, :m_inf, :m_det, :λ_bad, :m_bad, :sample

  update_tracker = fill(-99., N_its, 139)
  # move_SE_tracker, move_EI_tracker, AddRem_SE_tracker, AddRem_EI_tracker,
  # AddRem_Dets_tracker, AddRem_Deaths_tracker, AddRem_penv_tracker, AddRem_Moves_tracker
  # move_badger_SE_tracker, move_badger_EI_tracker, AddRem_badger_SE_tracker, AddRem_badger_EI_tracker,
  # AddRem_badger_Deaths_tracker, AddRem_badger_Births_tracker

  return(res, other_res, aug_res, tuning_res, update_tracker)
end

function rename_results_arrays(res, other_res, aug_res, tuning_res, update_tracker)

  res = DataFrame(res, :auto)
  rename!(res, [:β_c, :β_b, :γ, :F, :ϵ, :ρ, :ρ_E, :θ_bb, :θ_bd])

  other_res = DataFrame(other_res, :auto)
  rename!(other_res, [:is_accepted_inf, :log_α_ratio_inf, :post_inf, :post_prime_inf, :reason_inf,
                      :is_accepted_det, :log_α_ratio_det, :post_det, :post_prime_det, :reason_det,
                      :is_accepted_bad, :log_α_ratio_bad, :post_bad, :post_prime_bad, :reason_bad,
                      :sample])

  aug_res = DataFrame(aug_res, :auto)
  rename!(aug_res, [:is_accepted_move_SE, :log_α_ratio_move_SE, :post_move_SE, :post_prime_move_SE, :reason_move_SE,
                    :is_accepted_move_EI, :log_α_ratio_move_EI, :post_move_EI, :post_prime_move_EI, :reason_move_EI,
                    :is_accepted_AddRem_SE, :log_α_ratio_AddRem_SE, :post_AddRem_SE, :post_prime_AddRem_SE, :reason_AddRem_SE,
                    :is_accepted_AddRem_EI, :log_α_ratio_AddRem_EI, :post_AddRem_EI, :post_prime_AddRem_EI, :reason_AddRem_EI,
                    :is_accepted_AddRem_Det, :log_α_ratio_AddRem_Det, :post_AddRem_Det, :post_prime_AddRem_Det, :reason_AddRem_Det,
                    :is_accepted_AddRem_Death, :log_α_ratio_AddRem_Death, :post_AddRem_Death, :post_prime_AddRem_Death, :reason_AddRem_Death,
                    :is_accepted_AddRem_penv, :log_α_ratio_AddRem_penv, :post_AddRem_penv, :post_prime_AddRem_penv, :reason_AddRem_penv,
                    :is_accepted_AddRem_moves, :log_α_ratio_AddRem_moves, :post_AddRem_moves, :post_prime_AddRem_moves, :reason_AddRem_moves,
                    :is_accepted_move_badger_SE, :log_α_ratio_move_badger_SE, :post_move_badger_SE, :post_prime_move_badger_SE, :reason_move_badger_SE,
                    :is_accepted_move_badger_EI, :log_α_ratio_move_badger_EI, :post_move_badger_EI, :post_prime_move_badger_EI, :reason_move_badger_EI,
                    :is_accepted_AddRem_badger_SE, :log_α_ratio_AddRem_badger_SE, :post_AddRem_badger_SE, :post_prime_AddRem_badger_SE, :reason_AddRem_badger_SE,
                    :is_accepted_AddRem_badger_EI, :log_α_ratio_AddRem_badger_EI, :post_AddRem_badger_EI, :post_prime_AddRem_badger_EI, :reason_AddRem_badger_EI,
                    :is_accepted_AddRem_badger_Death, :log_α_ratio_AddRem_badger_Death, :post_AddRem_badger_Death, :post_prime_AddRem_badger_Death, :reason_AddRem_badger_Death,
                    :is_accepted_AddRem_badger_Birth, :log_α_ratio_AddRem_badger_Birth, :post_AddRem_badger_Birth, :post_prime_AddRem_badger_Birth, :reason_AddRem_badger_Birth,
                    :sample])

  tuning_res = DataFrame(tuning_res, :auto)
  rename!(tuning_res, [:λ_inf, :m_inf, :λ_det, :m_det, :λ_bad, :m_bad, :sample])


  update_tracker = DataFrame(update_tracker, :auto)
  rename!(update_tracker, [:mSE_position, :mSE_t, :mSE_is_accepted, :mSE_reason, :mSE_Δt, :mSE_num_moved,
                           :mEI_position, :mEI_t, :mEI_is_accepted, :mEI_reason, :mEI_Δt, :mEI_num_moved,
                           :arSE_position, :arSE_t, :arSE_is_accepted, :arSE_reason, :arSE_Δ, :arSE_SE_before, :arSE_SE_after, :arSE_cS, :arSE_prob,
                           :arEI_position, :arEI_t, :arEI_is_accepted, :arEI_reason, :arEI_Δ, :arEI_EI_before, :arEI_EI_after, :arEI_cE, :arEI_prob,
                           :arDet_position, :arDet_t, :arDet_is_accepted, :arDet_reason, :arDet_ΔE, :arDet_ΔI,
                                                      :arDet_Edet_before, :arDet_Idet_before, :arDet_Edet_after, :arDet_Idet_after,
                                                      :arDet_cE, :arDet_cI,
                           :arDeaths_position, :arDeaths_t, :arDeaths_is_accepted, :arDeaths_reason,
                                                      :arDeaths_ΔS, :arDeaths_ΔE, :arDeaths_ΔI,
                                                      :arDeaths_Sdths_before, :arDeaths_Edths_before, :arDeaths_Idths_before,
                                                      :arDeaths_Sdths_after, :arDeaths_Edths_after, :arDeaths_Idths_after,
                                                      :arDeaths_cS, :arDeaths_cE, :arDeaths_cI,
                           :arpenv_position, :arpenv_t, :arpenv_is_accepted, :arpenv_reason, :arpenv_Δr, :arpenv_Δn,
                                                      :arpenv_r_pres_before, :arpenv_n_pres_before, :arpenv_r_pres_after, :arpenv_n_pres_after,
                                                      :arpenv_p_env_prev, :arpenv_pI,
                           :arMoves_position, :arMoves_t, :arMoves_is_accepted, :arMoves_reason,
                                                      :arMoves_Soff_before, :arMoves_Eoff_before, :arMoves_Ioff_before,
                                                      :arMoves_Soff_after, :arMoves_Eoff_after, :arMoves_Ioff_after,
                                                      :arMoves_cS, :arMoves_cE, :arMoves_cI,
                           :mSE_badger_position, :mSE_badger_t, :mSE_badger_is_accepted, :mSE_badger_reason, :mSE_badger_Δt, :mSE_badger_num_moved,
                           :mEI_badger_position, :mEI_badger_t, :mEI_badger_is_accepted, :mEI_badger_reason, :mEI_badger_Δt, :mEI_badger_num_moved,
                           :arSE_badger_position, :arSE_badger_t, :arSE_badger_is_accepted, :arSE_badger_reason, :arSE_badger_Δ, :arSE_SE_badger_before, :arSE_SE_badger_after, :arSE_badger_cS, :arSE_badger_prob,
                           :arEI_badger_position, :arEI_badger_t, :arEI_badger_is_accepted, :arEI_badger_reason, :arEI_badger_Δ, :arEI_EI_badger_before, :arEI_EI_badger_after, :arEI_badger_cE, :arEI_badger_prob,
                           :arDeaths_badger_position, :arDeaths_badger_t, :arDeaths_badger_is_accepted, :arDeaths_badger_reason,
                                                      :arDeaths_badger_ΔS, :arDeaths_badger_ΔE, :arDeaths_badger_ΔI,
                                                      :arDeaths_badger_Sdths_before, :arDeaths_badger_Edths_before, :arDeaths_badger_Idths_before,
                                                      :arDeaths_badger_Sdths_after, :arDeaths_badger_Edths_after, :arDeaths_badger_Idths_after,
                                                      :arDeaths_badger_bS, :arDeaths_badger_bE, :arDeaths_badger_bI,
                           :arBirths_badger_position, :arBirths_badger_t, :arBirths_badger_is_accepted, :arBirths_badger_reason,
                                                      :arBirths_badger_ΔS, :arBirths_badger_births_before, :arBirths_badger_births_after,
                                                      :arBirths_badger_bS, :arBirths_badger_bE, :arBirths_badger_bI
                           ])

  return(res, other_res, aug_res, tuning_res, update_tracker)
end


####################
### MH FUNCTIONS ###
####################

function mult_mh_accept_ratio(post, post_prime, log_q_ratio, log_params_cur, log_params_draw)
  alpha =  (sum(log_params_draw) + post_prime) - (sum(log_params_cur) + post) + log_q_ratio

  return( min(0., alpha) )
end

function mh_accept_ratio(post, post_prime, log_q_ratio)

  log_α_ratio =  min( 0.,  (post_prime) - (post) + log_q_ratio )

  return(log_α_ratio)
end


# MH function for parameters

function metropolis_hastings_step_params(N_its, res, other_res, it,
                                          proposal_func, llh_update_func, posterior_func,
                                          params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                          llh_array_cur, p_env_llh_array_cur,
                                          f_to_p_structs::Vector{Farm_Parish_info}, log_prior_dists,
                                          n_tune, m, λ, d, covarM)

  # Propose an update

  log_params_cur = log.(params_cur)

  log_params_draw, log_q_ratio, mixture, λ, DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope = proposal_func(N_its, res, other_res,
                                                                                  it, log_params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur, f_to_p_structs,
                                                                                  n_tune, m, λ, d, covarM)

  params_draw = exp.(log_params_draw)

  # Calculate current posterior

  post_cur = posterior_func(llh_array_cur, p_env_llh_array_cur, scope, log_prior_dists, params_cur)

  # println("Proposed Params")

  if any(params_draw[6:7] .> 1)
    log_q_ratio = -Inf
  end

  # Early return: Update is invalid
  if log_q_ratio == -Inf
    return(params_cur, llh_array_cur, p_env_llh_array_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur, [false, -Inf, post_cur, -Inf, 2], mixture, λ)
                               # [is_accepted, log_α_ratio, post_cur, post_prime, reason]
  end

  # Update llh arrays
  llh_array_prime, p_env_llh_array_prime = llh_update_func(scope, llh_array_cur, p_env_llh_array_cur, DATA_res_and_track_prime, DATA_pers_and_parish_prime, params_draw, f_to_p_structs)

  # println("Updated llh")

  # Calculate new posterior

  post_prime = posterior_func(llh_array_prime, p_env_llh_array_prime, scope, log_prior_dists, params_draw)

  # println("Calculated posterior")

  # Calculate MH acceptance probability
  log_α_ratio = mult_mh_accept_ratio(post_cur, post_prime, log_q_ratio, log_params_cur, log_params_draw)
  is_accepted = false
  reason_ = 0

  # Accept/Reject
  accept_test = log(rand())
  if accept_test < log_α_ratio  #If yes:

    params_cur = deepcopy(params_draw)

    llh_array_cur = deepcopy(llh_array_prime)
    p_env_llh_array_cur = deepcopy(p_env_llh_array_prime)

    DATA_res_and_track_cur = deepcopy(DATA_res_and_track_prime)
    DATA_pers_and_parish_cur = deepcopy(DATA_pers_and_parish_prime)


    is_accepted = true
    reason_ = 1
  end

  return(params_cur, llh_array_cur, p_env_llh_array_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur, [is_accepted, log_α_ratio, post_cur, post_prime, reason_], mixture, λ)
end


# MH function for data augmentation

function metropolis_hastings_step_aug(proposal_func, posterior_func,
                                          params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                          llh_array_cur, p_env_llh_array_cur,
                                          f_to_p_structs::Vector{Farm_Parish_info}, ids_to_pos_dict,
                                          movements_record, movement_dict)



  # Propose an update

  DATA_res_and_track_prime, DATA_pers_and_parish_prime, log_q_ratio, scope, dataaug_track = proposal_func(DATA_res_and_track_cur, DATA_pers_and_parish_cur, params_cur, f_to_p_structs)

  # println(" ", "log_q_ratio = ", log_q_ratio)

  # println("Proposed update")

  # Early return: Update is invalid
  if isfinite(log_q_ratio) == false
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, [false, -Inf, Inf, -Inf, dataaug_track[4]], dataaug_track)
                                                              # [is_accepted, log_α_ratio, post_cur, post_prime, reason]
  end


  # Update llh arrays

  llh_array_prime, p_env_llh_array_prime = update_llh_array_ALL_excindvmoves(scope, llh_array_cur, p_env_llh_array_cur, DATA_res_and_track_prime, DATA_pers_and_parish_prime, movements_record, params_cur, movement_dict, f_to_p_structs)

  # println("Updated llh")


  # Calculate new posteriors

  post_cur = posterior_func(llh_array_cur, p_env_llh_array_cur, scope, params_cur)

  post_prime = posterior_func(llh_array_prime, p_env_llh_array_prime, scope, params_cur)


  # println("Calculated posteriors")

  # Calculate MH acceptance probability
  log_α_ratio = mh_accept_ratio(post_cur, post_prime, log_q_ratio)
  is_accepted = false
  reason_ = 0

  # Accept/Reject
  accept_test = log(rand())
  if accept_test < log_α_ratio  #If yes:

    llh_array_cur = deepcopy(llh_array_prime)
    p_env_llh_array_cur = deepcopy(p_env_llh_array_prime)

    DATA_res_and_track_cur = deepcopy(DATA_res_and_track_prime)
    DATA_pers_and_parish_cur = deepcopy(DATA_pers_and_parish_prime)

    is_accepted = true
    reason_ = 1

    dataaug_track[3:4] = [is_accepted, reason_]
  end

  return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, [is_accepted, log_α_ratio, post_cur, post_prime, reason_], dataaug_track)
end


# MH function for movements data augmentation

function metropolis_hastings_step_moves(params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                          llh_array_cur, p_env_llh_array_cur,
                                          f_to_p_structs::Vector{Farm_Parish_info}, ids_to_pos_dict::Dict{Int64, Int64},
                                          movements_record, movement_dict)



  # Propose an update

  DATA_res_and_track_prime, DATA_pers_and_parish_prime, log_q_ratio, scope, movements_record_prime, dataaug_track = propose_AddRem_Movements(DATA_res_and_track_cur, DATA_pers_and_parish_cur, params_cur, movements_record, movement_dict, f_to_p_structs, ids_to_pos_dict)

  # println("Proposed update")

  # Early return: Update is invalid
  if isfinite(log_q_ratio) == -Inf
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, [false, -Inf, post_cur, -Inf, dataaug_track[4]], movements_record, dataaug_track)
                                                              # [is_accepted, log_α_ratio, post_cur, post_prime, reason]
  end


  # Update llh arrays

  llh_array_prime, p_env_llh_array_prime = update_llh_array_ALL(scope, llh_array_cur, p_env_llh_array_cur, DATA_res_and_track_prime, DATA_pers_and_parish_prime, movements_record_prime, params_cur, movement_dict, f_to_p_structs)

  # println("Updated llh")


  # Calculate new posteriors

  post_cur = generic_posterior_dataaug(llh_array_cur, p_env_llh_array_cur, scope, params_cur)

  post_prime = generic_posterior_dataaug(llh_array_prime, p_env_llh_array_prime, scope, params_cur)


  # println("Calculated posteriors")

  # Calculate MH acceptance probability
  log_α_ratio = mh_accept_ratio(post_cur, post_prime, log_q_ratio)
  is_accepted = false
  reason_ = 0

  # Accept/Reject
  accept_test = log(rand())
  if accept_test < log_α_ratio  #If yes:

    llh_array_cur = deepcopy(llh_array_prime)
    p_env_llh_array_cur = deepcopy(p_env_llh_array_prime)

    DATA_res_and_track_cur = deepcopy(DATA_res_and_track_prime)
    DATA_pers_and_parish_cur = deepcopy(DATA_pers_and_parish_prime)

    movements_record = deepcopy(movements_record_prime)

    is_accepted = true
    reason_ = 1

    dataaug_track[3:4] = [is_accepted, reason_]
  end

  return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, [is_accepted, log_α_ratio, post_cur, post_prime, reason_], movements_record, dataaug_track)
end


#####################
### Adaptive MCMC ###
#####################

function Initialise(DATA_res_and_track_cur, DATA_pers_and_parish_cur, epi_params_true, epi_params_dists, moves_record, dict_of_movements, f_to_p_structs::Vector{Farm_Parish_info})

  # Current values
  β_c_cur, β_b_cur, γ_cur, F_cur, ϵ_cur, ρ_cur, ρ_E_cur, θ_bb_cur, θ_bd_cur = fill(missing, 9)

  epi_params_draw = fill(missing, 9)

  # True values
  β_c_true, β_b_true, γ_true, F_true, ϵ_true, ρ_true, ρ_E_true, θ_bb_true, θ_bd_true = epi_params_true

  # Prior distributions
  d_β_c, d_β_b, d_γ, d_F, d_ϵ, d_ρ, d_ρ_E, d_θ_bb, d_θ_bd = epi_params_dists

  # Set up LLH
  llh_array_init = zeros(size(DATA_res_and_track_cur[1], 1), 360, 13)
  p_env_llh_array_init = zeros(size(DATA_pers_and_parish_cur[2], 1), 360, 2)

  scope_init = Scope(1, 360, Vector(1:size(DATA_res_and_track_cur[1], 1)), Vector(1:13))

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)


  arb = true
  while arb == true
    # Initialise parameters
    if ismissing(β_c_true)
      β_c_cur = rand(d_β_c)
    else
      β_c_cur = β_c_true
    end
    if ismissing(β_b_true)
      β_b_cur = rand(d_β_b)
    else
      β_b_cur = β_b_true
    end
    if ismissing(γ_true)
      γ_cur = rand(d_γ)
    else
      γ_cur = γ_true
    end
    if ismissing(F_true)
      F_cur = rand(d_F)
    else
      F_cur = F_true
    end
    if ismissing(ϵ_true)
      ϵ_cur = rand(d_ϵ)
    else
      ϵ_cur = ϵ_true
    end
    if ismissing(ρ_true)
      ρ_cur = rand(d_ρ)
    else
      ρ_cur = ρ_true
    end
    if ismissing(ρ_E_true)
      ρ_E_cur = rand(d_ρ_E)
    else
      ρ_E_cur = ρ_E_true
    end
    if ismissing(θ_bb_true)
      θ_bb_cur = rand(d_θ_bb)
    else
      θ_bb_cur = θ_bb_true
    end
    if ismissing(θ_bd_true)
      θ_bd_cur = rand(d_θ_bd)
    else
      θ_bd_cur = θ_bd_true
    end

    # Current draws
    epi_params_draw = [β_c_cur, β_b_cur, γ_cur, F_cur, ϵ_cur, ρ_cur, ρ_E_cur, θ_bb_cur, θ_bd_cur]

    # Update the probabilities

    DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_pers_EPIDEMIC(DATA_res_and_track_cur, DATA_pers_and_parish_cur, log.(epi_params_draw), f_to_p_structs, scope_init)

    #Calculate the log-likelihood

    llh_array_init, p_env_llh_array_init = update_llh_array_ALL(scope_init,
                                                                  llh_array_init, p_env_llh_array_init,
                                                                  DATA_res_and_track_prime, DATA_pers_and_parish_prime, moves_record,
                                                                  epi_params_draw, dict_of_movements, f_to_p_structs)

    init_llh = calc_llh_h_and_p(scope_init, llh_array_init, p_env_llh_array_init)

    if init_llh != -Inf
      break
    end
  end

  println("Initialised")


  return(epi_params_draw, DATA_res_and_track_prime, DATA_pers_and_parish_prime)
end


function Blk_Adaptive_RWM_MCMC(;N_its, infer_block, data_aug_infer,
                                DATA_res_and_track, DATA_pers_and_parish, moves_record,
                                params_init, tuning,
                                dict_of_movements, f_to_p_structs::Vector{Farm_Parish_info}, ids_to_pos_dict)

  ##################
  ### Extraction ###
  ##################

  λ_inf = tuning[1]
  m_inf = tuning[2]

  λ_det = tuning[3]
  m_det = tuning[4]

  λ_bad = tuning[5]
  m_bad = tuning[6]

  ##############################################
  ### Initialise the parameters and epidemic ###
  ##############################################

    params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur = Initialise(DATA_res_and_track, DATA_pers_and_parish, params_init, epi_params_dists, moves_record, dict_of_movements, f_to_p_structs)

    covarM_inf = CovMatrix()
    covarM_det = CovMatrix()
    covarM_bad = CovMatrix()

  ########################
  ### Results matrices ###
  ########################

    res, other_res, aug_res, tuning_res, update_tracker = create_results_arrays(N_its, params_init)

  ##########################
  ### Functional objects ###
  ##########################

    it = 1

    Δm_inf = m_inf/100
    Δm_det = m_det/100
    Δm_bad = m_bad/100

    n_tune = 1


  ##################
  ### Likelihood ###
  ##################

  llh_array_init = zeros(size(DATA_res_and_track_cur[1], 1), 360, 13)

  # 1. m_off_llh, 2. m_on_out_llh, 3. c_exp_llh, 4. c_inf_llh, 5. exp_det_llh,
  # 6. inf_det_llh, 7. c_dth_llh, 8. b_exp_llh, 9. b_inf_llh, 10. b_bths_llh,
  # 11. bS_dths_llh, 12. bE_dths_llh, 13. bI_dths_llh

  p_env_llh_array_init = zeros(size(DATA_pers_and_parish_cur[2], 1), 360, 2)

  global_scope = Scope(1, 360, Vector(1:size(DATA_res_and_track_cur[1], 1)), Vector(1:13))

  llh_array_cur, p_env_llh_array_cur = update_llh_array_ALL(global_scope,
                                                            llh_array_init, p_env_llh_array_init,
                                                            DATA_res_and_track_cur, DATA_pers_and_parish_cur, moves_record,
                                                            params_cur, dict_of_movements, f_to_p_structs)

  ###########################
  ### ~~ THE ALGORITHM ~~ ###
  ###########################

    progbar = Progress(N_its, dt=10,
                        desc = "Running $N_its iterations:",
                        barglyphs=BarGlyphs('|','█', ['▁' ,'▂' ,'▃' ,'▄' ,'▅' ,'▆', '▇'],' ','|',),
                        barlen=10)

    while it <= N_its

      #######################################
      ### MH step for Epidemic parameters ###
      #######################################

        mh_res_inf::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
                     # is_accepted, log_α_ratio, post_cur, post_prime, reason_

        if infer_block[1] == true

          if it > 1
            fit!(covarM_inf, res[(it-1), [1,2,3,4,5]])
          end

          params_cur, llh_array_cur, p_env_llh_array_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur, mh_res_inf, mixture, λ_inf =
                    metropolis_hastings_step_params(N_its, res, other_res, it,
                                                    propose_epidemic_params, update_llh_array_EPIDEMIC, Infection_params_posterior,
                                                    params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                    llh_array_cur, p_env_llh_array_cur,
                                                    f_to_p_structs, epi_params_dists,
                                                    n_tune, m_inf, λ_inf, 5, covarM_inf)

          if mixture >= 0.05 # mixture is set to 0 while tuning λ
            if mh_res_inf[1] == 0
              m_inf = m_inf - (Δm_inf/((it)^0.5))
            else
              m_inf = m_inf + 2.3*(Δm_inf/((it)^0.5))
            end
          end

        end # if infer_block[1] == true


      ########################################
      ### MH step for Detection parameters ###
      ########################################

        mh_res_det::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
                     # is_accepted, log_α_ratio, post_cur, post_prime, reason_

        if infer_block[2] == true

          if it > 1
            fit!(covarM_det, res[(it-1), [6,7]])
          end

          params_cur, llh_array_cur, p_env_llh_array_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur, mh_res_det, mixture, λ_det =
                    metropolis_hastings_step_params(N_its, res, other_res, it,
                                                    propose_detection_params, update_llh_array_DETECTION, Detection_params_posterior,
                                                    params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                    llh_array_cur, p_env_llh_array_cur,
                                                    f_to_p_structs, epi_params_dists,
                                                    n_tune, m_det, λ_det, 2, covarM_det)


          if mixture >= 0.05 # mixture is set to 0 while tuning λ
            if mh_res_det[1] == 0
              m_det = m_det - (Δm_det/((it)^0.5))
            else
              m_det = m_det + 2.3*(Δm_det/((it)^0.5))
            end
          end

        end # if infer_block[2] == true


      #####################################
      ### MH step for Badger parameters ###
      #####################################

        mh_res_bad::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
                     # is_accepted, log_α_ratio, post_cur, post_prime, reason_

        if infer_block[3] == true

          if it > 1
            fit!(covarM_bad, res[(it-1), [8,9]])
          end

          params_cur, llh_array_cur, p_env_llh_array_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur, mh_res_bad, mixture, λ_bad =
                    metropolis_hastings_step_params(N_its, res, other_res, it,
                                                    propose_badger_params, update_llh_array_BBD, Badger_birth_death_params_posterior,
                                                    params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                    llh_array_cur, p_env_llh_array_cur,
                                                    f_to_p_structs, epi_params_dists,
                                                    n_tune, m_bad, λ_bad, 2, covarM_bad)


          if mixture >= 0.05 # mixture is set to 0 while tuning λ
            if mh_res_bad[1] == 0
              m_bad = m_bad - (Δm_bad/((it)^0.5))
            else
              m_bad = m_det + 2.3*(Δm_bad/((it)^0.5))
            end
          end

        end # if infer_block[3] == true


      ###################################
      ### Move S→E event through time ###
      ###################################

        mh_res_move_SE::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        move_SE_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                       # :mSE_position, :mSE_t, :mSE_is_accepted, :mSE_reason, :mSE_Δt, :mSE_num_moved

        if data_aug_infer[1] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_move_SE, move_SE_track =
                                                          metropolis_hastings_step_aug(propose_Move_SE, generic_posterior_SE_dataaug,
                                                                                        params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                        llh_array_cur, p_env_llh_array_cur,
                                                                                        f_to_p_structs, ids_to_pos_dict,
                                                                                        moves_record, dict_of_movements)
        end


      ###################################
      ### Move E→I event through time ###
      ###################################

        mh_res_move_EI::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        move_EI_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                      # :mEI_position, :mEI_t, :mEI_is_accepted, :mEI_reason, :mEI_Δt, :mEI_num_moved

        if data_aug_infer[2] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_move_EI, move_EI_track =
                                                        metropolis_hastings_step_aug(propose_Move_EI, generic_posterior_dataaug,
                                                                                      params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                      llh_array_cur, p_env_llh_array_cur,
                                                                                      f_to_p_structs, ids_to_pos_dict,
                                                                                      moves_record, dict_of_movements)
        end


      ############################
      ### Add/Remove S→E event ###
      ############################

        mh_res_AddRem_SE::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_SE_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                         # :arSE_position, :arSE_t, :arSE_is_accepted, :arSE_reason, :arSE_Δ, :arSE_SE_before, :arSE_SE_after, :arSE_cS, :arSE_prob

        if data_aug_infer[3] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_SE, AddRem_SE_track =
                                                  metropolis_hastings_step_aug(propose_AddRem_SE, generic_posterior_SE_dataaug,
                                                                                params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                llh_array_cur, p_env_llh_array_cur,
                                                                                f_to_p_structs, ids_to_pos_dict,
                                                                                moves_record, dict_of_movements)
        end


      ############################
      ### Add/Remove E→I event ###
      ############################

        mh_res_AddRem_EI::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_EI_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                         # :arEI_position, :arEI_t, :arEI_is_accepted, :arEI_reason, :arEI_Δ, :arEI_EI_before, :arEI_EI_after, :arEI_cE, :arEI_prob

        if data_aug_infer[4] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_EI, AddRem_EI_track =
                                                    metropolis_hastings_step_aug(propose_AddRem_EI, generic_posterior_dataaug,
                                                                                  params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                  llh_array_cur, p_env_llh_array_cur,
                                                                                  f_to_p_structs, ids_to_pos_dict,
                                                                                  moves_record, dict_of_movements)
        end


      ############################
      ### Add/Remove Det event ###
      ############################

        mh_res_AddRem_Det::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_Det_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                           # :arDet_position, :arDet_t, :arDet_is_accepted, :arDet_reason, :arDet_ΔE, :arDet_ΔI,
                                                      # :arDet_Edet_before, :arDet_Idet_before, :arDet_Edet_after, :arDet_Idet_after,
                                                      # :arDet_cE, :arDet_cI

        if data_aug_infer[5] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_Det, AddRem_Det_track =
                                                  metropolis_hastings_step_aug(propose_AddRem_Det, generic_posterior_dataaug,
                                                                                params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                llh_array_cur, p_env_llh_array_cur,
                                                                                f_to_p_structs, ids_to_pos_dict,
                                                                                moves_record, dict_of_movements)
        end


      ##############################
      ### Add/Remove Death event ###
      ##############################

        mh_res_AddRem_Death::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_Deaths_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                             # :arDeaths_position, :arDeaths_t, :arDeaths_is_accepted, :arDeaths_reason,
                                                 # :arDeaths_ΔS, :arDeaths_ΔE, :arDeaths_ΔI,
                                                 # :arDeaths_Sdths_before, :arDeaths_Edths_before, :arDeaths_Idths_before,
                                                 # :arDeaths_Sdths_after, :arDeaths_Edths_after, :arDeaths_Idths_after,
                                                 # :arDeaths_cS, :arDeaths_cE, :arDeaths_cI

        if data_aug_infer[6] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_Death, AddRem_Deaths_track =
                                                metropolis_hastings_step_aug(propose_AddRem_Deaths, generic_posterior_dataaug,
                                                                              params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                              llh_array_cur, p_env_llh_array_cur,
                                                                              f_to_p_structs, ids_to_pos_dict,
                                                                              moves_record, dict_of_movements)
        end


      ########################
      ### Add/Remove p_env ###
      ########################

        mh_res_AddRem_penv::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_penv_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                           # :arpenv_position, :arpenv_t, :arpenv_is_accepted, :arpenv_reason, :arpenv_Δr, :arpenv_Δn,
                                             # :arpenv_r_pres_before, :arpenv_n_pres_before, :arpenv_r_pres_after, :arpenv_n_pres_after,
                                             # :arpenv_p_env_prev, :arpenv_pI

        if data_aug_infer[7] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_penv, AddRem_penv_track =
                                              metropolis_hastings_step_aug(propose_AddRem_penv, generic_posterior_dataaug,
                                                                            params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                            llh_array_cur, p_env_llh_array_cur,
                                                                            f_to_p_structs, ids_to_pos_dict,
                                                                            moves_record, dict_of_movements)
        end


      #################################
      ### Add/Remove Movement event ###
      #################################

        mh_res_AddRem_Moves::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_Moves_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                            # :arMoves_position, :arMoves_t, :arMoves_is_accepted, :arMoves_reason,
                                               # :arMoves_Soff_before, :arMoves_Eoff_before, :arMoves_Ioff_before,
                                               # :arMoves_Soff_after, :arMoves_Eoff_after, :arMoves_Ioff_after,
                                               # :arMoves_cS, :arMoves_cE, :arMoves_cI

        if data_aug_infer[8] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_Moves, moves_record, AddRem_Moves_track =
                                                  metropolis_hastings_step_moves(params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                  llh_array_cur, p_env_llh_array_cur,
                                                                                  f_to_p_structs, ids_to_pos_dict,
                                                                                  moves_record, dict_of_movements)
        end



      ##########################################
      ### Move badger S→E event through time ###
      ##########################################

        mh_res_move_badger_SE::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        move_badger_SE_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                       # :mSE_position, :mSE_t, :mSE_is_accepted, :mSE_reason, :mSE_Δt, :mSE_num_moved

        if data_aug_infer[9] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_move_badger_SE, move_badger_SE_track =
                                                          metropolis_hastings_step_aug(propose_Move_Badger_SE, generic_posterior_SE_dataaug,
                                                                                        params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                        llh_array_cur, p_env_llh_array_cur,
                                                                                        f_to_p_structs, ids_to_pos_dict,
                                                                                        moves_record, dict_of_movements)
        end


      ##########################################
      ### Move badger E→I event through time ###
      ##########################################

        mh_res_move_badger_EI::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        move_badger_EI_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                      # :mEI_position, :mEI_t, :mEI_is_accepted, :mEI_reason, :mEI_Δt, :mEI_num_moved

        if data_aug_infer[10] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_move_badger_EI, move_badger_EI_track =
                                                        metropolis_hastings_step_aug(propose_Move_Badger_EI, generic_posterior_dataaug,
                                                                                      params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                      llh_array_cur, p_env_llh_array_cur,
                                                                                      f_to_p_structs, ids_to_pos_dict,
                                                                                      moves_record, dict_of_movements)
        end


      ###################################
      ### Add/Remove badger S→E event ###
      ###################################

        mh_res_AddRem_badger_SE::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_badger_SE_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                         # :arSE_position, :arSE_t, :arSE_is_accepted, :arSE_reason, :arSE_Δ, :arSE_SE_before, :arSE_SE_after, :arSE_cS, :arSE_prob

        if data_aug_infer[11] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_badger_SE, AddRem_badger_SE_track =
                                                  metropolis_hastings_step_aug(propose_AddRem_Badger_SE, generic_posterior_SE_dataaug,
                                                                                params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                llh_array_cur, p_env_llh_array_cur,
                                                                                f_to_p_structs, ids_to_pos_dict,
                                                                                moves_record, dict_of_movements)
        end


      ###################################
      ### Add/Remove badger E→I event ###
      ###################################

        mh_res_AddRem_badger_EI::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_badger_EI_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                         # :arEI_position, :arEI_t, :arEI_is_accepted, :arEI_reason, :arEI_Δ, :arEI_EI_before, :arEI_EI_after, :arEI_cE, :arEI_prob

        if data_aug_infer[12] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_badger_EI, AddRem_EI_badger_track =
                                                    metropolis_hastings_step_aug(propose_AddRem_Badger_EI, generic_posterior_dataaug,
                                                                                  params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                                  llh_array_cur, p_env_llh_array_cur,
                                                                                  f_to_p_structs, ids_to_pos_dict,
                                                                                  moves_record, dict_of_movements)
        end



      ##############################
      ### Add/Remove Death event ###
      ##############################

        mh_res_AddRem_badger_Death::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_badger_Deaths_track::Vector{Float64} = [-Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf, -Inf]
                             # :arDeaths_position, :arDeaths_t, :arDeaths_is_accepted, :arDeaths_reason,
                                                 # :arDeaths_ΔS, :arDeaths_ΔE, :arDeaths_ΔI,
                                                 # :arDeaths_Sdths_before, :arDeaths_Edths_before, :arDeaths_Idths_before,
                                                 # :arDeaths_Sdths_after, :arDeaths_Edths_after, :arDeaths_Idths_after,
                                                 # :arDeaths_cS, :arDeaths_cE, :arDeaths_cI

        if data_aug_infer[13] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_badger_Death, AddRem_Deaths_badger_track =
                                                metropolis_hastings_step_aug(propose_AddRem_Badger_Deaths, generic_posterior_dataaug,
                                                                              params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                              llh_array_cur, p_env_llh_array_cur,
                                                                              f_to_p_structs, ids_to_pos_dict,
                                                                              moves_record, dict_of_movements)
        end


      ##############################
      ### Add/Remove Birth event ###
      ##############################

        mh_res_AddRem_badger_Birth::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf]
        AddRem_badger_Births_track::Vector{Float64} = [-Inf, -Inf, Inf, Inf, Inf, -Inf, -Inf, Inf, Inf, Inf]
                             # :arBirths_position, :arBirths_t, :arBirths_is_accepted, :arBirths_reason,
                                                 # :arBirths_ΔS, # :arBirths_Sdths_before, # :arBirths_Sdths_after,
                                                 # :arBirths_bS, :arBirths_bE, :arBirths_bI

        if data_aug_infer[14] == true

          DATA_res_and_track_cur, DATA_pers_and_parish_cur, llh_array_cur, p_env_llh_array_cur, mh_res_AddRem_badger_Birth, AddRem_Births_badger_track =
                                                metropolis_hastings_step_aug(propose_AddRem_Badger_Births, generic_posterior_dataaug,
                                                                              params_cur, DATA_res_and_track_cur, DATA_pers_and_parish_cur,
                                                                              llh_array_cur, p_env_llh_array_cur,
                                                                              f_to_p_structs, ids_to_pos_dict,
                                                                              moves_record, dict_of_movements)
        end



      ##########################
      ### Record the results ###
      ##########################

        # Record parameters
        res[it,:] = params_cur

        # Record other
        other_res[it,:] = [mh_res_inf ; mh_res_det ; mh_res_bad ; it]

        # Record aug
        aug_res[it,:] = [mh_res_move_SE ; mh_res_move_EI ; mh_res_AddRem_SE ; mh_res_AddRem_EI ; mh_res_AddRem_Det ; mh_res_AddRem_Death ; mh_res_AddRem_penv ; mh_res_AddRem_Moves ; mh_res_move_badger_SE ; mh_res_move_badger_EI ; mh_res_AddRem_badger_SE ; mh_res_AddRem_badger_EI ; mh_res_AddRem_badger_Death ; mh_res_AddRem_badger_Birth ; it]

        # Record tuning params
        tuning_res[it, :] = [λ_inf, m_inf, λ_det, m_det, λ_bad, m_bad, it]

        # Record tracking
        update_tracker[it,:] = [move_SE_track ; move_EI_track ; AddRem_SE_track ; AddRem_EI_track ;
                                 AddRem_Det_track ; AddRem_Deaths_track ; AddRem_penv_track ; AddRem_Moves_track ;
                                 move_badger_SE_track ; move_badger_EI_track ; AddRem_badger_SE_track ; AddRem_badger_EI_track ;
                                 AddRem_badger_Deaths_track ; AddRem_badger_Births_track]

        # if rem(it, 1000) == 0
        #   println("it = ", it)
        # end
        next!(progbar)

        # Update count
        if it == (n_tune*25)
          n_tune = n_tune + 1
        end
        it = it + 1

        # if it == 5500
        #   it = 999999
        # end

    end #end of while

    res_df, other_res_df, aug_res_df, tuning_res_df, update_tracker_df = rename_results_arrays(res, other_res, aug_res, tuning_res, update_tracker)

  return(res_df, other_res_df, aug_res_df, tuning_res_df, update_tracker_df)
end

# Reasons
# 0 = MH rejected
# 1 = MH accepted
# 2 = Invalid data
# 3 = Propsal no difference
# 4 = Proposal outside boundss

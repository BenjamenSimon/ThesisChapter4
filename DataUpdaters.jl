
###################################################################
### Functions to calculate exposure and infection probabilities ###
###################################################################

function calc_exp_prop(;States_init, p_env_prev, β, F)

  I = States_init[3]
  N = sum(States_init)

  if N > 0
    exp_prob = 1.0 - exp(- (I/N) * β - F * p_env_prev)
  else
    exp_prob = 0.0
  end

  return(exp_prob)
end

function calc_inf_prob(γ)

  inf_prob = 1.0 - exp(-γ)

  return(inf_prob)
end


###################################################
### Update the persistents after parameter draw ###
###################################################

function update_pers_EPIDEMIC(DATA_res_and_track_cur, DATA_pers_and_parish_cur, log_epi_params_draw, f_to_p_structs::Vector{Farm_Parish_info}, scope::Scope)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  epi_params_draw = exp.(log_epi_params_draw)

  t_start = scope.t_start # 1
  t_end = scope.t_end # size(DATA_res_and_track_cur[1], 2)
  positions = scope.h_positions # 1:size(DATA_res_and_track_cur[1], 1)

  @inbounds for pos in positions
    @inbounds for t in t_start:t_end

      # Update Cattle Exposure Probability

      DATA_pers_and_parish_prime[1][pos, t, 4] = calc_exp_prop(;States_init = DATA_res_and_track_prime[1][pos, t, 4:6],
                                                       p_env_prev = DATA_pers_and_parish_prime[2][f_to_p_structs[pos].parish_position, t, 19],
                                                       β = epi_params_draw[1],
                                                       F = epi_params_draw[4])

      # Update Badger Exposure Probability

      DATA_pers_and_parish_prime[1][pos, t, 5] = calc_exp_prop(;States_init = DATA_res_and_track_prime[1][pos, t, 22:24],
                                                       p_env_prev = DATA_pers_and_parish_prime[2][f_to_p_structs[pos].parish_position, t, 19],
                                                       β = epi_params_draw[2],
                                                       F = epi_params_draw[4])


      # Update Cattle Infection Probability

      DATA_pers_and_parish_prime[1][pos, t, 6] = calc_inf_prob(epi_params_draw[3])

      # Update Badger Infection Probability

      DATA_pers_and_parish_prime[1][pos, t, 7] = calc_inf_prob(epi_params_draw[3])

    end
  end

  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime)
end

function update_cattle_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, scope::Scope)

  t_start = scope.t_start
  t_end = scope.t_end
  positions = scope.h_positions

  @inbounds for pos in positions
    @inbounds for t in t_start:t_end

      # Update Cattle Exposure Probability

      DATA_pers_and_parish_prime[1][pos, t, 4] = calc_exp_prop(;States_init = DATA_res_and_track_prime[1][pos, t, 4:6],
                                                       p_env_prev = DATA_pers_and_parish_prime[2][f_to_p_structs[pos].parish_position, t, 19],
                                                       β = epi_params[1],
                                                       F = epi_params[4])


      # Update Cattle Infection Probability

      DATA_pers_and_parish_prime[1][pos, t, 6] = calc_inf_prob(epi_params[3])

    end
  end



  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime)
end

function update_badger_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, scope::Scope)

  t_start = scope.t_start
  t_end = scope.t_end
  positions = scope.h_positions

  @inbounds for pos in positions
    @inbounds for t in t_start:t_end

      # Update Badger Exposure Probability

      DATA_pers_and_parish_prime[1][pos, t, 5] = calc_exp_prop(;States_init = DATA_res_and_track_prime[1][pos, t, 22:24],
                                                       p_env_prev = DATA_pers_and_parish_prime[2][f_to_p_structs[pos].parish_position, t, 19],
                                                       β = epi_params[2],
                                                       F = epi_params[4])


     # Update Badger Infection Probability

     DATA_pers_and_parish_prime[1][pos, t, 7] = calc_inf_prob(epi_params[3])

    end
  end

  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime)
end


####################################################
### Update the data after data augmentation draw ###
####################################################

### MOVE SE ###

function update_data_Move_SE(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δ, num_SE_moved, f_to_p_structs::Vector{Farm_Parish_info})

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Functional objects ###
  sgnΔ = sign(Δ)
  A = convert(Int, (1 - sgnΔ)/2)
  B = 1-A

  ### Scope ###

  lower_t = (t+(A*Δ))
  upper_t = (t+(B*Δ))
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, 13] -= num_SE_moved
  DATA_res_and_track_prime[2][position, (t+Δ), 13] += num_SE_moved


  ############
  ### Update the states
  ############

  # :cS_init, :cS_Moves, :cS_postM
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [4,7,10]] .+= sgnΔ * num_SE_moved
  # :cS_postEI, :cS_postDet, :cS_final
  DATA_res_and_track_prime[1][position, (lower_t):(upper_t-1), [13,16,19]] .+= sgnΔ * num_SE_moved

  # :cE_init, :cE_Moves, :cE_postM
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [5,8,11]] .-= sgnΔ * num_SE_moved
  # :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, (lower_t):(upper_t-1), [14,17,20]] .-= sgnΔ * num_SE_moved


  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [4,5,7,8,10,11,13,14,16,17,19,20]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    # println("    ", "INVALID UPDATE!")
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0)
                             # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pcS_init
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):(upper_t), 4] .+= sgnΔ * num_SE_moved
  # :pcS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t):(upper_t-1), 7] .+= sgnΔ * num_SE_moved
  # :pcE_init
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):(upper_t), 5] .-= sgnΔ * num_SE_moved
  # :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t):(upper_t-1), 8] .-= sgnΔ * num_SE_moved


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1)
                                  # valid
end

### MOVE EI ###

function update_data_Move_EI(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δ, num_EI_moved, epi_params, f_to_p_structs::Vector{Farm_Parish_info})

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Functional objects ###
  sgnΔ = sign(Δ)
  A = convert(Int, (1 - sgnΔ)/2)
  B = 1-A

  ### Scope ###

  lower_t = (t+(A*Δ))
  upper_t = (t+(B*Δ))
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, 14] -= num_EI_moved
  DATA_res_and_track_prime[2][position, (t+Δ), 14] += num_EI_moved


  ############
  ### Update the states
  ############

  # :cE_init, :cE_Moves, :cE_postM
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [5,8,11]] .+= sgnΔ * num_EI_moved
  # :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, (lower_t):(upper_t-1), [14,17,20]] .+= sgnΔ * num_EI_moved

  # :cI_init, :cI_Moves, :cI_postM
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [6,9,12]] .-= sgnΔ * num_EI_moved
  # :cI_postEI, :cI_postDet, :cI_final
  DATA_res_and_track_prime[1][position, (lower_t):(upper_t-1), [15,18,21]] .-= sgnΔ * num_EI_moved


  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [5,6,8,9,11,12,14,15,17,18,20,21]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0)
                              # invalid
  end

  ##############
  ### Update the parish states
  ##############

  # :pcE_init
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):(upper_t), 5] .+= sgnΔ * num_EI_moved
  # :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t):(upper_t-1), 8] .+= sgnΔ * num_EI_moved
  # :pcI_init
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):(upper_t), 6] .-= sgnΔ * num_EI_moved
  # :pcI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t):(upper_t-1), 9] .-= sgnΔ * num_EI_moved


  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_cattle_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)

  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1)
                                                                      # valid
end


### ADD/REM SE ###

function update_data_AddRem_SE(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δ, f_to_p_structs::Vector{Farm_Parish_info}, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = size(DATA_res_and_track_cur[1], 2) #T
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, 13] += Δ

  ############
  ### Update the states
  ############

  # :cS_postEI, :cS_postDet, :cS_final
  DATA_res_and_track_prime[1][position, lower_t, [13,16,19]] .-= Δ
  # :cS_init, :cS_Moves, :cS_postM, :cS_postEI, :cS_postDet, :cS_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [4,7,10,13,16,19]] .-= Δ

  # :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [5,8,11]] .+= Δ
  # :cE_init, :cE_Moves, :cE_postM, :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, lower_t, [5,8,11,14,17,20]] .+= Δ

  tracker[6:9] = [DATA_res_and_track_cur[2][position, t, 13], DATA_res_and_track_prime[2][position, t, 13],  DATA_res_and_track_cur[1][position, t, 4], DATA_pers_and_parish_cur[1][position, t, 4]]
               #  :arSE_SE_before, :arSE_SE_after, :arSE_cS, :arSE_prob

  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [4,5,7,8,10,11,13,14,16,17,19,20]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0, tracker)
                              # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pcS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 7] -= Δ
  # :pcS_init, :pcS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [4,7]] .-= Δ

  # :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 8] += Δ
  # :pcE_init, :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [5,8]] .+= Δ


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end

### ADD/REM EI ###

function update_data_AddRem_EI(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δ, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = size(DATA_res_and_track_cur[1], 2) #T
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, 14] += Δ


  ############
  ### Update the states
  ############

  # :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, lower_t, [14,17,20]] .-= Δ
  # :cE_init, :cE_Moves, :cE_postM, :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [5,8,11,14,17,20]] .-= Δ

  # :cI_postEI, :cI_postDet, :cI_final
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [6,9,12]] .+= Δ
  # :cI_init, :cI_Moves, :cI_postM, :cI_postEI, :cI_postDet, :cI_final
  DATA_res_and_track_prime[1][position, lower_t, [6,9,12,15,18,21]] .+= Δ


  tracker[6:9] = [DATA_res_and_track_cur[2][position, t, 14], DATA_res_and_track_prime[2][position, t, 14],  DATA_res_and_track_cur[1][position, t, 5], DATA_pers_and_parish_cur[1][position, t, 6]]
                # :arEI_EI_before, :arEI_EI_after, :arEI_cE, :arEI_prob

  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [5,6,8,9,11,12,14,15,17,18,20,21]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0, tracker)
                              # invalid
  end

  ##############
  ### Update the parish states
  ##############

  # :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 8] -= Δ
  # :pcE_init, :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [5,8]] .-= Δ

  # :pcI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 9] += Δ
  # :pcI_init, :pcI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [6,9]] .+= Δ


  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_cattle_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)

  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end


### ADD/REM Detections ###

function update_data_AddRem_Det(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δs, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = size(DATA_res_and_track_cur[1], 2) #T
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  # Δs = [ΔE, ΔI]
  # Check ΔE = 0 outside of func

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, [19,20]] += Δs


  ############
  ### Update the states
  ############


  # cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, lower_t, [17, 20]] .-= Δs[1]
  # :cE_init, :cE_Moves, :cE_postM, :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [5,8,11,14,17,20]] .-= Δs[1]


  # :cI_postDet, :cI_final
  DATA_res_and_track_prime[1][position, lower_t, [18, 21]] .-= Δs[2]
  # :cI_init, :cI_Moves, :cI_postM, :cI_postEI, :cI_postDet, :cI_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [6,9,12,15,18,21]] .-= Δs[2]


  tracker[7:12] = [DATA_res_and_track_cur[2][position, t, 19:20] ; DATA_res_and_track_prime[2][position, t, 19:20] ;  DATA_res_and_track_cur[1][position, t, 14:15]]
                # :arDet_Edet_before, :arDet_Idet_before, :arDet_Edet_after, :arDet_Idet_after, :arDet_cE, :arDet_cI


  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [5,6,8,9,11,12,14,15,17,18,20,21]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0, tracker)
                              # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 8] -= Δs[1]
  # :pcE_init, :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [5,8]] .-= Δs[1]

  # :pcI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 9] -= Δs[2]
  # :pcI_init, :pcI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [6,9]] .-= Δs[2]


  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_cattle_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end


### ADD/REM Deaths ###

function update_data_AddRem_Deaths(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δs, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = size(DATA_res_and_track_cur[1], 2) #T
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  # Δs = [ΔS, ΔE, ΔI]
  # Check sum(Δs) != 0 outside of func

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, [22,23,24]] += Δs


  ############
  ### Update the states
  ############

  # :cS_final
  DATA_res_and_track_prime[1][position, lower_t, [19]] .-= Δs[1]
  # :cS_init, :cS_Moves, :cS_postM, :cS_postEI, :cS_postDet, :cS_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [4,7,10,13,16,19]] .-= Δs[1]


  # :cE_final
  DATA_res_and_track_prime[1][position, lower_t, [20]] .-= Δs[2]
  # :cE_init, :cE_Moves, :cE_postM, :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [5,8,11,14,17,20]] .-= Δs[2]


  # :cI_final
  DATA_res_and_track_prime[1][position, lower_t, [21]] .-= Δs[3]
  # :cI_init, :cI_Moves, :cI_postM, :cI_postEI, :cI_postDet, :cI_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [6,9,12,15,18,21]] .-= Δs[3]


  tracker[8:16] = [DATA_res_and_track_cur[2][position, t, 22:24] ; DATA_res_and_track_prime[2][position, t, 22:24] ;  DATA_res_and_track_cur[1][position, t, 16:18]]
                # :arDeaths_Sdths_before, :arDeaths_Edths_before, :arDeaths_Idths_before,
                # :arDeaths_Sdths_after, :arDeaths_Edths_after, :arDeaths_Idths_after,
                # :arDeaths_cS, :arDeaths_cE, :arDeaths_cI


  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), 4:21] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0, tracker)
                              # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pcS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 7] -= Δs[1]
  # :pcS_init, :pcS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [4,7]] .-= Δs[1]

  # :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 8] -= Δs[2]
  # :pcE_init, :pcE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [5,8]] .-= Δs[2]

  # :pcI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 9] -= Δs[3]
  # :pcI_init, :pcI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [6,9]] .-= Δs[3]


  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_cattle_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end


### ADD/REM Movements ###

function update_data_AddRem_Movement(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope::Scope, position, DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, differences_oi, parish_differences)

  t = scope.t_start
  T = scope.t_end
  h_positions = scope.h_positions
  h_llh_indices = scope.h_llh_indices

  ##########################################
  ### Update the farm that moved animals ###
  ##########################################

  Δ_off = DATA_res_and_track_prime[2][position, t, [7,8,9]] - DATA_res_and_track_cur[2][position, t, [7,8,9]]

  # :cS_postM, :cS_postEI, :cS_postDet, :cS_final
  DATA_res_and_track_prime[1][position, t, [10,13,16,19]] .-= Δ_off[1]
  # :cS_init, :cS_Moves, :cS_postM, :cS_postEI, :cS_postDet, :cS_final
  DATA_res_and_track_prime[1][position, (t+1):T, [4,7,10,13,16,19]] .-= Δ_off[1]


  # :cE_postM, :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, t, [11,14,17,20]] .-= Δ_off[2]
  # :cE_init, :cE_Moves, :cE_postM, :cE_postEI, :cE_postDet, :cE_final
  DATA_res_and_track_prime[1][position, (t+1):T, [5,8,11,14,17,20]] .-= Δ_off[2]


  # :cI_postM, :cI_postEI, :cI_postDet, :cI_final
  DATA_res_and_track_prime[1][position, t, [12,15,18,21]] .-= Δ_off[3]
  # :cI_init, :cI_Moves, :cI_postM, :cI_postEI, :cI_postDet, :cI_final
  DATA_res_and_track_prime[1][position, (t+1):T, [6,9,12,15,18,21]] .-= Δ_off[3]


  ##############################################
  ### Update the farms that recieved animals ###
  ##############################################

  for j in 1:size(differences_oi, 1)

      pos = differences_oi[j, 5]

      Δ_on_j = differences_oi[j, 1:3]

      # :cS_postM, :cS_postEI, :cS_postDet, :cS_final
      DATA_res_and_track_prime[1][pos, t, [10,13,16,19]] .+= Δ_on_j[1]
      # :cS_init, :cS_Moves, :cS_postM, :cS_postEI, :cS_postDet, :cS_final
      DATA_res_and_track_prime[1][pos, (t+1):T, [4,7,10,13,16,19]] .+= Δ_on_j[1]


      # :cE_postM, :cE_postEI, :cE_postDet, :cE_final
      DATA_res_and_track_prime[1][pos, t, [11,14,17,20]] .+= Δ_on_j[2]
      # :cE_init, :cE_Moves, :cE_postM, :cE_postEI, :cE_postDet, :cE_final
      DATA_res_and_track_prime[1][pos, (t+1):T, [5,8,11,14,17,20]] .+= Δ_on_j[2]


      # :cI_postM, :cI_postEI, :cI_postDet, :cI_final
      DATA_res_and_track_prime[1][pos, t, [12,15,18,21]] .+= Δ_on_j[3]
      # :cI_init, :cI_Moves, :cI_postM, :cI_postEI, :cI_postDet, :cI_final
      DATA_res_and_track_prime[1][pos, (t+1):T, [6,9,12,15,18,21]] .+= Δ_on_j[3]


      # :sus_on, :exp_on, :inf_on
      DATA_res_and_track_prime[2][pos, t, [4,5,6]] .+= Δ_on_j

      ##############
      ### Update the probabilities
      ##############

      DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_cattle_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)

  end # end of for each move


  ################################
  ### Update the parish totals ###
  ################################

  for j in 1:size(parish_differences, 1)

    p_pos = parish_differences[j, 4]

    Δ_pj = parish_differences[j, 1:3]


    if p_pos > 0

      # :pcS_final
      DATA_pers_and_parish_prime[2][p_pos, t, 7] += Δ_pj[1]
      # :pcS_init, :pcS_final
      DATA_pers_and_parish_prime[2][p_pos, (t+1):T, [4,7]] .+= Δ_pj[1]

      # :pcE_final
      DATA_pers_and_parish_prime[2][p_pos, t, 8] += Δ_pj[2]
      # :pcE_init, :pcE_final
      DATA_pers_and_parish_prime[2][p_pos, (t+1):T, [5,8]] .+= Δ_pj[2]

      # :pcI_final
      DATA_pers_and_parish_prime[2][p_pos, t, 9] += Δ_pj[3]
      # :pcI_init, :pcI_final
      DATA_pers_and_parish_prime[2][p_pos, (t+1):T, [6,9]] .+= Δ_pj[3]

    end # end if p_pos > 0

  end # end of for each parish


  ####################################
  ### Early return: Invalid Update ###
  ####################################

  for chpos in h_positions
    posi_check = DATA_res_and_track_prime[1][h_positions, t:T, 4:21] .>= 0

    if sum(posi_check) != prod(size(posi_check))
      # returns changed position as a Int instead of a Vector
      return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, 2)
    end
  end

  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, 1)
                          # valid
end


### ADD/REM Environmental Pressure ###

function update_data_AddRem_penv(DATA_res_and_track_cur, DATA_pers_and_parish_cur, p_position, t, Δs, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, ids_to_pos_dict, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = t+1
  h_positions = f_to_p_structs[ ids_to_pos_dict[ DATA_pers_and_parish_prime[2][p_position, t, 23] ] ].parish_members_positions
  h_llh_indices = [3,8]

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  # Δs = [Δr, Δn]
  # Check sum(Δs) != 0 outside of func

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  # :remaining_pressure, :new_pressure
  DATA_pers_and_parish_prime[2][p_position, t, [16, 17]] += Δs

  Δpenv = sum(Δs)/DATA_pers_and_parish_prime[2][p_position, t, 18] # /scaling

  ############
  ### Update the states
  ############

  # :p_env_prev
  DATA_pers_and_parish_prime[2][p_position, (t+1), 19] += Δpenv
  # :p_env_cur
  DATA_pers_and_parish_prime[2][p_position, t, 20] += Δpenv


  tracker[7:12] = [DATA_pers_and_parish_cur[2][p_position, t, 16:17] ; DATA_pers_and_parish_prime[2][p_position, t, 16:17] ;  DATA_pers_and_parish_cur[2][p_position, (t+1), [19,6]]]
                # :arpenv_r_pres_before, :arpenv_n_pres_before, :arpenv_r_pres_after, :arpenv_n_pres_after, :arpenv_p_env_prev, :arpenv_pI

  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_pers_EPIDEMIC(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end


###############
### BADGERS ###
###############


### MOVE BADGER SE ###

function update_data_Move_Badger_SE(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δ, num_SE_moved, f_to_p_structs::Vector{Farm_Parish_info})

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Functional objects ###
  sgnΔ = sign(Δ)
  A = convert(Int, (1 - sgnΔ)/2)
  B = 1-A

  # println("    ", "num_SE_moved = ", num_SE_moved)

  ### Scope ###

  lower_t = (t+(A*Δ))
  upper_t = (t+(B*Δ))
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, 15] -= num_SE_moved
  DATA_res_and_track_prime[2][position, (t+Δ), 15] += num_SE_moved


  ############
  ### Update the states
  ############

  # :bS_init
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [22]] .+= sgnΔ * num_SE_moved
  # :bS_postEI, :bS_final
  DATA_res_and_track_prime[1][position, (lower_t):(upper_t-1), [25,28]] .+= sgnΔ * num_SE_moved

  # :bE_init
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [23]] .-= sgnΔ * num_SE_moved
  # :bE_postEI, :bE_final
  DATA_res_and_track_prime[1][position, (lower_t):(upper_t-1), [26,29]] .-= sgnΔ * num_SE_moved


  ###############
  ### Quick check for validity
  ###############

  # println("    ", "Current data:")
  # println("    ", DATA_res_and_track_cur[1][position, (lower_t):(upper_t), [4,5,7,8,10,11,13,14,16,17,19,20]])
  # println("    ", "Updated data:")
  # println("    ", DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [4,5,7,8,10,11,13,14,16,17,19,20]])

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [22,23,24,25,26,27,28,29]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    # println("    ", "INVALID UPDATE!")
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0)
                             # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pbS_init
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):(upper_t), 10] .+= sgnΔ * num_SE_moved
  # :pbS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t):(upper_t-1), 13] .+= sgnΔ * num_SE_moved
  # :pbE_init
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):(upper_t), 11] .-= sgnΔ * num_SE_moved
  # :pbE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t):(upper_t-1), 14] .-= sgnΔ * num_SE_moved


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1)
                                  # valid
end


### MOVE BADGER EI ###

function update_data_Move_Badger_EI(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δ, num_EI_moved, epi_params, f_to_p_structs::Vector{Farm_Parish_info})

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Functional objects ###
  sgnΔ = sign(Δ)
  A = convert(Int, (1 - sgnΔ)/2)
  B = 1-A

  ### Scope ###

  lower_t = (t+(A*Δ))
  upper_t = (t+(B*Δ))
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, 16] -= num_EI_moved
  DATA_res_and_track_prime[2][position, (t+Δ), 16] += num_EI_moved


  ############
  ### Update the states
  ############

  # :bE_init
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [23]] .+= sgnΔ * num_EI_moved
  # :bE_postEI, :bE_final
  DATA_res_and_track_prime[1][position, (lower_t):(upper_t-1), [26, 29]] .+= sgnΔ * num_EI_moved

  # :bI_init
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [24]] .-= sgnΔ * num_EI_moved
  # :bI_postEI, :bI_final
  DATA_res_and_track_prime[1][position, (lower_t):(upper_t-1), [27,30]] .-= sgnΔ * num_EI_moved


  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [22,23,24,25,26,27,28,29]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0)
                              # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pbE_init
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):(upper_t), 11] .+= sgnΔ * num_EI_moved
  # :pbE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t):(upper_t-1), 14] .+= sgnΔ * num_EI_moved
  # :pbI_init
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):(upper_t), 12] .-= sgnΔ * num_EI_moved
  # :pbI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t):(upper_t-1), 15] .-= sgnΔ * num_EI_moved


  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_badger_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)

  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1)
                                                                      # valid
end


### ADD/REM SE ###

function update_data_AddRem_Badger_SE(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δ, f_to_p_structs::Vector{Farm_Parish_info}, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = size(DATA_res_and_track_cur[1], 2) #T
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, 15] += Δ


  ############
  ### Update the states
  ############

  # :bS_postEI, :bS_final
  DATA_res_and_track_prime[1][position, lower_t, [25,28]] .-= Δ
  # :bS_init, :bS_postEI, :bS_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [22,25,28]] .-= Δ

  # :bE_postEI, :bE_final
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [26, 29]] .+= Δ
  # :bE_init, :bE_postEI, :bE_final
  DATA_res_and_track_prime[1][position, lower_t, [23, 26, 29]] .+= Δ

  tracker[6:9] = [DATA_res_and_track_cur[2][position, t, 15], DATA_res_and_track_prime[2][position, t, 15],  DATA_res_and_track_cur[1][position, t, 22], DATA_pers_and_parish_cur[1][position, t, 5]]
               #  :arSE_bSE_before, :arSE_bSE_after, :arSE_bS_init, :b_exp_prob

  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [22,23,24,25,26,27,28,29]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0, tracker)
                              # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pbS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 13] -= Δ
  # :pbS_init, :pbS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [10,13]] .-= Δ

  # :pbE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 14] += Δ
  # :pbE_init, :pbE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [11,14]] .+= Δ


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end


### ADD/REM EI ###

function update_data_AddRem_Badger_EI(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δ, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = size(DATA_res_and_track_cur[1], 2) #T
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, 16] += Δ


  ############
  ### Update the states
  ############

  # :bE_postEI, :bE_final
  DATA_res_and_track_prime[1][position, lower_t, [26, 29]] .-= Δ
  # :bE_init, :bE_postEI, :bE_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [23, 26, 29]] .-= Δ

  # :bI_postEI, :bI_final
  DATA_res_and_track_prime[1][position, (lower_t+1):(upper_t), [27, 30]] .+= Δ
  # :bI_init, :bI_postEI, :bI_final
  DATA_res_and_track_prime[1][position, lower_t, [24, 27, 30]] .+= Δ


  tracker[6:9] = [DATA_res_and_track_cur[2][position, t, 16], DATA_res_and_track_prime[2][position, t, 16],  DATA_res_and_track_cur[1][position, t, 23], DATA_pers_and_parish_cur[1][position, t, 7]]
                # :arEI_EI_before, :arEI_EI_after, :arEI_bE, :arEI_prob

  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [22,23,24,25,26,27,28,29]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0, tracker)
                              # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pbE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 14] -= Δ
  # :pbE_init, :pbE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [11,14]] .-= Δ

  # :pbI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 15] += Δ
  # :pbI_init, :pbI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [12,15]] .+= Δ


  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_badger_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)

  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end


### ADD/REM Deaths ###

function update_data_AddRem_Badger_Deaths(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δs, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = size(DATA_res_and_track_cur[1], 2) #T
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  # Δs = [ΔS, ΔE, ΔI]
  # Check sum(Δs) != 0 outside of func

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, [26,27,28]] += Δs


  ############
  ### Update the states
  ############

  # :bS_final
  DATA_res_and_track_prime[1][position, lower_t, [28]] .-= Δs[1]
  # :bS_init, :bS_postEI, :bS_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [22,25,28]] .-= Δs[1]


  # :bE_final
  DATA_res_and_track_prime[1][position, lower_t, [29]] .-= Δs[2]
  # :bE_init, :bE_postEI, :bE_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [23,26,29]] .-= Δs[2]


  # :bI_final
  DATA_res_and_track_prime[1][position, lower_t, [30]] .-= Δs[3]
  # :bI_init, :bI_postEI, :bI_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [24,27,30]] .-= Δs[3]


  tracker[8:16] = [DATA_res_and_track_cur[2][position, t, 26:28] ; DATA_res_and_track_prime[2][position, t, 26:28] ;  DATA_res_and_track_cur[1][position, t, 25:27]]
                # :arDeaths_Sdths_before, :arDeaths_Edths_before, :arDeaths_Idths_before,
                # :arDeaths_Sdths_after, :arDeaths_Edths_after, :arDeaths_Idths_after,
                # :arDeaths_bS_postEI, :arDeaths_bE_postEI, :arDeaths_bI_postEI


  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [22,23,24,25,26,27,28,29]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0, tracker)
                              # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pbS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 13] -= Δs[1]
  # :pbS_init, :pbS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [10,13]] .-= Δs[1]

  # :pbE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 14] -= Δs[2]
  # :pbE_init, :pbE_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [11,14]] .-= Δs[2]

  # :pbI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 15] -= Δs[3]
  # :pbI_init, :pbI_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [12,15]] .-= Δs[3]


  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_badger_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end


### ADD/REM Deaths ###

function update_data_AddRem_Badger_Births(DATA_res_and_track_cur, DATA_pers_and_parish_cur, position, t, Δs, epi_params, f_to_p_structs::Vector{Farm_Parish_info}, tracker)

  DATA_res_and_track_prime = deepcopy(DATA_res_and_track_cur)
  DATA_pers_and_parish_prime = deepcopy(DATA_pers_and_parish_cur)

  ### Scope ###

  lower_t = t
  upper_t = size(DATA_res_and_track_cur[1], 2) #T
  h_positions = [position]
  h_llh_indices = Vector(1:13)

  scope = Scope(lower_t, upper_t, h_positions, h_llh_indices)

  # Δs = [ΔS]
  # Check sum(Δs) != 0 outside of func

  ###########################
  ### Generate new states ###
  ###########################

  ##########
  ### Update the events
  ##########

  DATA_res_and_track_prime[2][position, t, [25]] .+= Δs


  ############
  ### Update the states
  ############

  # :bS_final
  DATA_res_and_track_prime[1][position, lower_t, [28]] .-= Δs[1]
  # :bS_init, :bS_postEI, :bS_final
  DATA_res_and_track_prime[1][position, (lower_t+1):upper_t, [22,25,28]] .-= Δs[1]


  tracker[6:10] = [DATA_res_and_track_cur[2][position, t, [25]] ; DATA_res_and_track_prime[2][position, t, [25]] ;  DATA_res_and_track_cur[1][position, t, 25:27]]
                # :arBirths_births_before, :arBirths_births_aftere,
                # :arBirths_bS_postEI, :arBirths_bE_postEI, :arBirths_bI_postEI


  ###############
  ### Quick check for validity
  ###############

  posi_check = (DATA_res_and_track_prime[1][position, (lower_t):(upper_t), [22,23,24,25,26,27,28,29]] .>= 0)

  if sum(sum.(eachrow(posi_check))) != prod(size(posi_check))
    return(DATA_res_and_track_cur, DATA_pers_and_parish_cur, scope, 0, tracker)
                              # invalid
  end


  ##############
  ### Update the parish states
  ##############

  # :pbS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, lower_t, 13] -= Δs[1]
  # :pbS_init, :pbS_final
  DATA_pers_and_parish_prime[2][f_to_p_structs[position].parish_position, (lower_t+1):upper_t, [10,13]] .-= Δs[1]


  ##############
  ### Update the probabilities
  ##############

  DATA_res_and_track_prime, DATA_pers_and_parish_prime = update_badger_pers_general(DATA_res_and_track_prime, DATA_pers_and_parish_prime, epi_params, f_to_p_structs, scope)


  return(DATA_res_and_track_prime, DATA_pers_and_parish_prime, scope, 1, tracker)
                                  # valid
end

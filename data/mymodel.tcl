output -log_file mymodel.iclo 
set_default_model_dichtomous 2PL 
options -default_prior_b none
options -default_prior_a {lognormal 0.0 0.5}
options -D 1.0
allocate_items_dist 18 -num_latent_dist_points 20 
read_examinees mymodel.dat 18i1 
starting_values_dichotomous
EM_steps -max_iter 2000
write_item_param mymodel.iclp 
write_latent_dist mymodel.icld 
release_items_dist

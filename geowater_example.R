if(!exists('OMEGA')) OMEGA = 'prop'
if(!exists('LAMBDA')) LAMBDA = 'coda.norm'

library(dplyr)

load('/home/marc/research/geowater/data/geowater_dataset_0001.RData')
load('/home/marc/research/geowater/data/mixture_model_0001.RData')

data = data %>%
  mutate(
    g0 = sprintf('%02d', m@bestResult@partition),
    post = apply(m@bestResult@proba, 1, max) )

source('/home/marc/research/geowater/hierarchical_merging_functions.R')

POST = data.frame(m@bestResult@proba) %>% tbl_df
POST[POST==0] = .Machine$double.xmin

library(mixpack)
hp = get_hierarchical_partition_fast(POST %>% data.frame %>% as.matrix, omega = OMEGA, lambda=LAMBDA)

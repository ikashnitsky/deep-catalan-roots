################################################################################
#
# ikashnitsky.github.io 2018-06-04
# Deep Catalan roots: playing with stringdist
# https://ikashnitsky.github.io/2018/deep-catalan-roots/
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#
################################################################################

# Erase all objects in memory
rm(list = ls(all = TRUE))


library(tidyverse)
library(magrittr)
library(janitor)
library(lubridate)
library(stringdist)
library(extrafont)
library(hrbrthemes)
# get Roboto Consensed font -- called later as font_rc
import_roboto_condensed()

# set seed
set.seed(911)


# compare string similarity methods ---------------------------------------


meths <- c("osa", "lv", "dl", "hamming", "lcs", "qgram",
           "cosine", "jaccard", "jw", "soundex")

tibble(a = "demography",
       b = "democracy",
       method = meths) %>% 
        mutate(value = pmap_dbl(., stringsim))

tibble(a = "austria",
       b = "australia",
       method = meths) %>% 
        mutate(value = pmap_dbl(., stringsim))

library(microbenchmark)

bench <- microbenchmark(
        osa = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "osa"),
        lv = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "lv"),
        
        dl = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "dl"),
        
        hamming = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "hamming"),
        
        lcs = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "lcs"),
        
        qgarm = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "qgram"),
        
        cosine = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "cosine"),
        
        jaccard = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "jaccard"),
        
        jw = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "jw"),
        
        soundex = 
                stringsim(a = "demography",
                          b = "democracy",
                          method = "soundex")
        
) 

bench

# plot the results of benchmarking
bench %>% autoplot()+ 
        aes(fill = expr) + 
        scale_fill_brewer(palette = "Paired")+
        theme_minimal(base_size = 15, base_family = font_rc)+
        theme(legend.position = "none")+
        labs(title = "Stingdist methods speed",
             subtitle = 'Distance between words "demodraphy" and "democracy"',
             caption = "ikashnitsky.github.io")

gg_bench <- last_plot()

ggsave("benchmarking.png", 
       gg_bench, width = 7, height = 5)





# read in data ------------------------------------------------------------


# let's check if we, EDSD cohort 2017-18, are re-incarnations of some
# noble catalans from 16-17 centuries

# names of the EDSDers
edsd <- read_csv("edsders.csv.gz") %>% 
        mutate(name = name %>% tolower(),
               surname = surname %>% tolower())

# minimal data sample -- best matching records according to "jw" measure 
# 10 records per each name of EDSDers

load("catalan-names-minimal.RData")



# the function ------------------------------------------------------------

# a function to calculate distance index using a scpecific stringdist method
# beware, object and vars names are hard-coded; designed to be used with 
# purrr::map2_dbl()

sim_2col_pmap <- function(name, surname, 
                          name_c, surname_c,
                          method = "jw", ...){
        
        library(tidyverse)
        library(stringdist)
        
        return(
                tibble(
                        nm = stringsim(
                                method = method,
                                name,
                                name_c
                        ) 
                        ,
                        sn = stringsim(
                                method = method,
                                surname,
                                surname_c
                        ) 

                ) %>% 
                        pmap_dbl(
                                .f = function(nm, sn) {
                                        (nm + sn) / 2
                                } 
                        )
        )
        
}


# males -------------------------------------------------------------------


all_measures_males <- catalan_males %>% 
        mutate(
                i_osa = pmap_dbl(.l = ., sim_2col_pmap, method = "osa"),
                i_lcs = pmap_dbl(.l = ., sim_2col_pmap, method = "lcs"),
                i_qgram = pmap_dbl(.l = ., sim_2col_pmap, method = "qgram"),
                i_cosine = pmap_dbl(.l = ., sim_2col_pmap, method = "cosine"),
                i_jaccard = pmap_dbl(.l = ., sim_2col_pmap, method = "jaccard")
        ) %>% 
        mutate(
                index_mean = 
                        pmap_dbl(
                                .l = .,
                                .f = function(i_jw, i_osa, i_lcs, i_qgram,
                                              i_cosine, i_jaccard, ...){
                                        mean(
                                                c(i_jw, i_osa, i_lcs, i_qgram,
                                                  i_cosine, i_jaccard)
                                        )  
                                }
                        )
                ,
                index_geom_avg =
                        pmap_dbl(
                                .l = .,
                                .f = function(i_jw, i_osa, i_lcs, i_qgram,
                                              i_cosine, i_jaccard, ...){
                                        prod(
                                                c(i_jw, i_osa, i_lcs, i_qgram,
                                                  i_cosine, i_jaccard)
                                        ) ^ (1/6) 
                                }
                        )
        )

# select 3 best fittting for each
final_males <- all_measures_males %>% 
        group_by(name, surname) %>% 
        arrange(desc(index_geom_avg)) %>% 
        slice(1:3) %>% 
        ungroup() %>% 
        left_join(all_names %>% 
                          transmute(id, mariage, spouse = name2),
                  by = "id")

# view and take screenshot manually
final_males %>% View



# females -----------------------------------------------------------------



all_measures_females <- catalan_females %>% 
        mutate(
                i_osa = pmap_dbl(.l = ., sim_2col_pmap, method = "osa"),
                i_lcs = pmap_dbl(.l = ., sim_2col_pmap, method = "lcs"),
                i_qgram = pmap_dbl(.l = ., sim_2col_pmap, method = "qgram"),
                i_cosine = pmap_dbl(.l = ., sim_2col_pmap, method = "cosine"),
                i_jaccard = pmap_dbl(.l = ., sim_2col_pmap, method = "jaccard")
        ) %>% 
        mutate(
                index_mean = 
                        pmap_dbl(
                                .l = .,
                                .f = function(i_jw, i_osa, i_lcs, i_qgram,
                                              i_cosine, i_jaccard, ...){
                                        mean(
                                                c(i_jw, i_osa, i_lcs, i_qgram,
                                                  i_cosine, i_jaccard)
                                        )  
                                }
                        )
                ,
                index_geom_avg =
                        pmap_dbl(
                                .l = .,
                                .f = function(i_jw, i_osa, i_lcs, i_qgram,
                                              i_cosine, i_jaccard, ...){
                                        prod(
                                                c(i_jw, i_osa, i_lcs, i_qgram,
                                                  i_cosine, i_jaccard)
                                        ) ^ (1/6) 
                                }
                        )
        )

final_females <- all_measures_females %>% 
        group_by(name, surname) %>% 
        arrange(desc(index_geom_avg)) %>% 
        slice(1:3) %>% 
        ungroup()%>% 
        left_join(all_names %>% transmute(id, mariage, spouse = name1),
                  by = "id")


# view and take screenshot manually
final_females %>% View
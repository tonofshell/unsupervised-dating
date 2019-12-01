Advanced Clustering
================
Adam Shelton
11/11/2019

## Import Data

``` r
original_data = readRDS(here("Data", "full_ok_cupid_cleaned.rds"))

new_features = read_csv(here("Data", "newfeatures.csv")) %>% select(-X1) %>% as.matrix() %>% scale()
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
cluster_data = original_data %>% select(c("age", "body_type", "diet", "drinks", "drugs", "education", "ethnicity", "job", "orientation", "religion", "sex", "smokes", "status", "sign_import", "dogs", "cats", "kids", "multi_ling", "days_since_online", "distance")) %>% select(-c(cats, dogs)) %>%  mutate_if(is.character, factor) %>% filter(distance < 50) %>% select(-distance) 

names(cluster_data) %>% setdiff(cluster_data %>% drop_high_na(0.35) %>% names)
```

    ## [1] "diet"        "sign_import" "kids"

``` r
#%>% .[sample(1:nrow(.), 500), ]
```

## Imputing Data

``` r
if ("full_imputed.rds" %in% list.files(here("Data"))) {
  imputed = readRDS(here("Data", "full_imputed.rds"))
} else {
  setup_cl = function(seed = round(Sys.time())) {
  require(parallel)
  if (exists("cl")) {
    print("Stopping existing cluster")
    try(parallel::stopCluster(cl))
  }
  assign("cl", parallel::makeCluster(parallel::detectCores() - 1, outfile = "out.txt"), envir = globalenv())
  RNGkind("L'Ecuyer-CMRG")
  print(paste("Using", as.numeric(seed), "as parallel RNG seed"))
  clusterSetRNGStream(cl, seed)
}
setup_cl(60615)
registerDoParallel(cl)
  tic()
  imputed = cluster_data %>% as.data.frame() %>% missForest(parallelize = "forests")
  toc()
  saveRDS(imputed, here("Data", "full_imputed.rds"))  
}
imputed_data = imputed$ximp
imputed$OOBerror %>% kable()
```

|       |         x |
| ----- | --------: |
| NRMSE | 0.0055781 |
| PFC   | 0.3178881 |

<!--
# Take 1

## PCA




## CLARA


## DBSCAN


# Take 2
-->

## Descriptive Statistics

``` r
skim_list = imputed_data %>% as_tibble() %>% skim() %>% partition()

skim_list$numeric %>% kable()
```

| skim\_variable      | n\_missing | complete\_rate |       mean |         sd | p0 |       p25 |       p50 |      p75 |     p100 | hist  |
| :------------------ | ---------: | -------------: | ---------: | ---------: | -: | --------: | --------: | -------: | -------: | :---- |
| age                 |          0 |              1 | 32.3484638 |  9.4569922 | 18 | 26.000000 | 30.000000 | 37.00000 | 110.0000 | ▇▂▁▁▁ |
| multi\_ling         |          0 |              1 |  0.5144091 |  0.4996107 |  0 |  0.000000 |  1.000000 |  1.00000 |   1.0000 | ▇▁▁▁▇ |
| days\_since\_online |          0 |              1 | 40.1846817 | 77.3470630 |  0 |  1.328472 |  3.792361 | 32.69444 | 370.2951 | ▇▁▁▁▁ |

``` r
skim_list$factor %>% kable()
```

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                                   |
| :------------- | ---------: | -------------: | :------ | --------: | :-------------------------------------------- |
| body\_type     |          0 |              1 | FALSE   |        12 | ave: 15294, fit: 13416, ath: 12847, cur: 5394 |
| diet           |          0 |              1 | FALSE   |         6 | any: 50043, veg: 6823, oth: 1954, veg: 739    |
| drinks         |          0 |              1 | FALSE   |         6 | soc: 44135, rar: 6132, oft: 5280, not: 3447   |
| drugs          |          0 |              1 | FALSE   |         3 | nev: 48945, som: 10426, oft: 420              |
| education      |          0 |              1 | FALSE   |        32 | gra: 26422, gra: 9961, wor: 6480, gra: 1936   |
| ethnicity      |          0 |              1 | FALSE   |        11 | whi: 36205, asi: 6848, mul: 5426, his: 3180   |
| job            |          0 |              1 | FALSE   |        21 | oth: 7835, stu: 6315, com: 5271, sci: 5220    |
| orientation    |          0 |              1 | FALSE   |         3 | str: 51483, gay: 5554, bis: 2754              |
| religion       |          0 |              1 | FALSE   |         9 | agn: 11333, oth: 11182, ath: 10130, chr: 9403 |
| sex            |          0 |              1 | FALSE   |         2 | m: 35728, f: 24063                            |
| smokes         |          0 |              1 | FALSE   |         5 | no: 48544, som: 4096, whe: 3250, yes: 2388    |
| status         |          0 |              1 | FALSE   |         5 | sin: 55554, see: 2057, ava: 1860, mar: 310    |
| sign\_import   |          0 |              1 | FALSE   |         3 | it’: 30649, it : 28452, it : 690              |
| kids           |          0 |              1 | FALSE   |         3 | doe: 45706, has: 8737, has: 5348              |

``` r
imputed_data %>% mutate_all(as.numeric) %>% cor(use = "pairwise.complete.obs") %>% ggcorrplot()
```

![](advanced_clustering_files/figure-gfm/descr-stats-1.png)<!-- -->

``` r
clusterability = imputed_data %>% mutate_all(as.numeric) %>% sample_n(5000) %>% get_clust_tendency(n = 50)
clusterability$hopkins_stat
```

    ## [1] 0.2012606

``` r
clusterability$plot
```

![](advanced_clustering_files/figure-gfm/descr-stats-2.png)<!-- -->

## PCA

``` r
imputed_data %>% mutate_all(as.numeric) %>% PCA(graph = FALSE) %>% fviz_pca_biplot(label = "var", col.var = "red", col.ind = "grey")
```

![](advanced_clustering_files/figure-gfm/pca-1.png)<!-- -->

``` r
ggsave2(here("Clustering", "pca_v2.png"), height = 7, width = 11)
```

<!--## PAM

-->

## Agglomerative Nesting

``` r
sampled_data = imputed_data %>% as_tibble() %>% sample_n(1000) 
agnes_data = sampled_data %>% mutate_all(as.numeric) %>% mutate_all(scale) 
agnes_diss = agnes_data %>% as.matrix() %>% daisy(metric = "gower")
```

    ## Warning in daisy(., metric = "gower"): binary variable(s) 11, 16 treated as
    ## interval scaled

``` r
nb_results = NbClust(data = agnes_data, diss = agnes_diss, distance = NULL, min.nc = 2, max.nc = 10, method = "ward.D2")
```

![](advanced_clustering_files/figure-gfm/agg-nest-1.png)<!-- -->

    ## *** : The Hubert index is a graphical method of determining the number of clusters.
    ##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
    ##                 significant increase of the value of the measure i.e the significant peak in Hubert
    ##                 index second differences plot. 
    ## 

![](advanced_clustering_files/figure-gfm/agg-nest-2.png)<!-- -->

    ## *** : The D index is a graphical method of determining the number of clusters. 
    ##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
    ##                 second differences plot) that corresponds to a significant increase of the value of
    ##                 the measure. 
    ##  
    ## ******************************************************************* 
    ## * Among all indices:                                                
    ## * 2 proposed 2 as the best number of clusters 
    ## * 11 proposed 3 as the best number of clusters 
    ## * 1 proposed 4 as the best number of clusters 
    ## * 1 proposed 5 as the best number of clusters 
    ## * 2 proposed 6 as the best number of clusters 
    ## * 1 proposed 7 as the best number of clusters 
    ## * 2 proposed 8 as the best number of clusters 
    ## * 2 proposed 9 as the best number of clusters 
    ## * 1 proposed 10 as the best number of clusters 
    ## 
    ##                    ***** Conclusion *****                            
    ##  
    ## * According to the majority rule, the best number of clusters is  3 
    ##  
    ##  
    ## *******************************************************************

``` r
fviz_nbclust(nb_results)
```

    ## Among all indices: 
    ## ===================
    ## * 2 proposed  0 as the best number of clusters
    ## * 1 proposed  1 as the best number of clusters
    ## * 2 proposed  2 as the best number of clusters
    ## * 11 proposed  3 as the best number of clusters
    ## * 1 proposed  4 as the best number of clusters
    ## * 1 proposed  5 as the best number of clusters
    ## * 2 proposed  6 as the best number of clusters
    ## * 1 proposed  7 as the best number of clusters
    ## * 2 proposed  8 as the best number of clusters
    ## * 2 proposed  9 as the best number of clusters
    ## * 1 proposed  10 as the best number of clusters
    ## 
    ## Conclusion
    ## =========================
    ## * According to the majority rule, the best number of clusters is  3 .

![](advanced_clustering_files/figure-gfm/agg-nest-3.png)<!-- -->

``` r
agnes_mod = agnes_diss %>% hcut(isdiss = TRUE, k = 3, hc_func = "agnes")
fviz_dend(agnes_mod)
```

![](advanced_clustering_files/figure-gfm/agg-nest-4.png)<!-- -->

``` r
sampled_data$cluster = agnes_mod$cluster
fviz_cluster(agnes_mod, data = agnes_diss)
```

![](advanced_clustering_files/figure-gfm/agg-nest-5.png)<!-- -->

``` r
saveRDS(sampled_data, here("Data", "Results", "agnes_results.rds"))
write_csv(sampled_data, here("Data", "Results", "agnes_results.csv"))
```

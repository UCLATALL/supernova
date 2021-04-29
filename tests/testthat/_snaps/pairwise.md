# there is an informative error if you try to select a non-existent term

    `correction` must be one of "none", "Bonferroni", or "Tukey".

# catch all other data here

    Code
      pairwise_t(fit)
    Message <cliMessage>
      
      -- Pairwise t-tests ------------------------------------------------------------
      Model: Thumb ~ RaceEthnic
    Output
      
    Message <cliMessage>
      RaceEthnic
      Levels: 5
      Family-wise error-rate: 0.401
    Output
      
         group_1       group_2         diff pooled_se      t    df  lower  upper p_val
         <chr>         <chr>          <dbl>     <dbl>  <dbl> <int>  <dbl>  <dbl> <dbl>
       1 African Amer~ White          2.985     2.023  1.476   152 -0.363  6.333 .1421
       2 Asian         White         -0.964     1.182 -0.816   152 -2.920  0.992 .4160
       3 Latino        White         -1.796     1.434 -1.253   152 -4.169  0.577 .2122
       4 Other         White          5.929     1.953  3.037   152  2.698  9.161 .0028
       5 Asian         African Amer~ -3.949     2.003 -1.971   152 -7.265 -0.634 .0505
       6 Latino        African Amer~ -4.782     2.162 -2.212   152 -8.359 -1.204 .0285
       7 Other         African Amer~  2.944     2.536  1.161   152 -1.252  7.140 .2474
       8 Latino        Asian         -0.832     1.406 -0.592   152 -3.159  1.495 .5548
       9 Other         Asian          6.894     1.932  3.567   152  3.696 10.092 .0005
      10 Other         Latino         7.726     2.096  3.686   152  4.257 11.194 .0003

---

    Code
      pairwise_bonferroni(fit)
    Message <cliMessage>
      
      -- Pairwise t-tests with Bonferroni correction ---------------------------------
      Model: Thumb ~ RaceEthnic
    Output
      
    Message <cliMessage>
      RaceEthnic
      Levels: 5
      Family-wise error-rate: 0.049
    Output
      
         group_1      group_2        diff pooled_se      t    df   lower  upper  p_adj
         <chr>        <chr>         <dbl>     <dbl>  <dbl> <int>   <dbl>  <dbl>  <dbl>
       1 African Ame~ White         2.985     2.023  1.476   152  -2.292  8.263 1.0000
       2 Asian        White        -0.964     1.182 -0.816   152  -4.047  2.119 1.0000
       3 Latino       White        -1.796     1.434 -1.253   152  -5.536  1.944 1.0000
       4 Other        White         5.929     1.953  3.037   152   0.836 11.023  .0282
       5 Asian        African Ame~ -3.949     2.003 -1.971   152  -9.175  1.276  .5049
       6 Latino       African Ame~ -4.782     2.162 -2.212   152 -10.420  0.857  .2845
       7 Other        African Ame~  2.944     2.536  1.161   152  -3.670  9.559 1.0000
       8 Latino       Asian        -0.832     1.406 -0.592   152  -4.500  2.835 1.0000
       9 Other        Asian         6.894     1.932  3.567   152   1.853 11.934  .0048
      10 Other        Latino        7.726     2.096  3.686   152   2.258 13.193  .0032

---

    Code
      pairwise_tukey(fit)
    Message <cliMessage>
      
      -- Tukey's Honestly Significant Differences ------------------------------------
      Model: Thumb ~ RaceEthnic
    Output
      
    Message <cliMessage>
      RaceEthnic
      Levels: 5
      Family-wise error-rate: 0.05
    Output
      
         group_1       group_2        diff pooled_se      q    df   lower  upper p_adj
         <chr>         <chr>         <dbl>     <dbl>  <dbl> <int>   <dbl>  <dbl> <dbl>
       1 African Amer~ White         2.985     2.023  1.476   152  -4.913 10.884 .8347
       2 Asian         White        -0.964     1.182 -0.816   152  -5.579  3.651 .9783
       3 Latino        White        -1.796     1.434 -1.253   152  -7.394  3.802 .9018
       4 Other         White         5.929     1.953  3.037   152  -1.695 13.554 .2058
       5 Asian         African Ame~ -3.949     2.003 -1.971   152 -11.771  3.872 .6325
       6 Latino        African Ame~ -4.782     2.162 -2.212   152 -13.221  3.658 .5227
       7 Other         African Ame~  2.944     2.536  1.161   152  -6.956 12.844 .9238
       8 Latino        Asian        -0.832     1.406 -0.592   152  -6.322  4.657 .9935
       9 Other         Asian         6.894     1.932  3.567   152  -0.651 14.438 .0910
      10 Other         Latino        7.726     2.096  3.686   152  -0.458 15.909 .0742


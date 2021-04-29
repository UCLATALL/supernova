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
       1 African Amer~ White          2.985     2.861  1.043   152 -1.749  7.720 .2984
       2 Asian         White         -0.964     1.672 -0.577   152 -3.730  1.802 .5649
       3 Latino        White         -1.796     2.028 -0.886   152 -5.152  1.560 .3771
       4 Other         White          5.929     2.762  2.147   152  1.359 10.500 .0334
       5 Asian         African Amer~ -3.949     2.833 -1.394   152 -8.638  0.739 .1654
       6 Latino        African Amer~ -4.782     3.057 -1.564   152 -9.841  0.277 .1199
       7 Other         African Amer~  2.944     3.586  0.821   152 -2.991  8.879 .4129
       8 Latino        Asian         -0.832     1.988 -0.419   152 -4.123  2.459 .6762
       9 Other         Asian          6.894     2.733  2.523   152  2.371 11.416 .0127
      10 Other         Latino         7.726     2.964  2.606   152  2.820 12.631 .0101

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
       1 African Ame~ White         2.985     2.861  1.043   152  -4.478 10.448 1.0000
       2 Asian        White        -0.964     1.672 -0.577   152  -5.324  3.396 1.0000
       3 Latino       White        -1.796     2.028 -0.886   152  -7.086  3.493 1.0000
       4 Other        White         5.929     2.762  2.147   152  -1.274 13.133  .3337
       5 Asian        African Ame~ -3.949     2.833 -1.394   152 -11.340  3.441 1.0000
       6 Latino       African Ame~ -4.782     3.057 -1.564   152 -12.756  3.193 1.0000
       7 Other        African Ame~  2.944     3.586  0.821   152  -6.410 12.298 1.0000
       8 Latino       Asian        -0.832     1.988 -0.419   152  -6.019  4.355 1.0000
       9 Other        Asian         6.894     2.733  2.523   152  -0.235 14.022  .1268
      10 Other        Latino        7.726     2.964  2.606   152  -0.006 15.458  .1006

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


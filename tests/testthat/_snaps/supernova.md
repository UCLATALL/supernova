# message is given for number of rows deleted due to missing cases

    Code
      supernova(lm(mpg ~ hp, df_missing))
    Message <supernova_missing_values_message>
      Refitting to remove 1 case with missing value(s)
      i lm(formula = mpg ~ hp, data = listwise_delete(df_missing, c("mpg", "hp")))
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ hp
      
                                     SS df      MS      F   PRE     p
       ----- --------------- | -------- -- ------- ------ ----- -----
       Model (error reduced) |  680.166  1 680.166 44.323 .6045 .0000
       Error (from model)    |  445.027 29  15.346                   
       ----- --------------- | -------- -- ------- ------ ----- -----
       Total (empty model)   | 1125.194 30  37.506                   

---

    Code
      supernova(lm(mpg ~ disp, df_missing))
    Message <supernova_missing_values_message>
      Refitting to remove 2 cases with missing value(s)
      i lm(formula = mpg ~ disp, data = listwise_delete(df_missing, c("mpg", "disp")))
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ disp
      
                                     SS df      MS      F   PRE     p
       ----- --------------- | -------- -- ------- ------ ----- -----
       Model (error reduced) |  810.915  1 810.915 74.074 .7257 .0000
       Error (from model)    |  306.528 28  10.947                   
       ----- --------------- | -------- -- ------- ------ ----- -----
       Total (empty model)   | 1117.443 29  38.533                   

---

    Code
      supernova(lm(mpg ~ hp * disp, df_missing))
    Message <supernova_missing_values_message>
      Refitting to remove 3 cases with missing value(s)
      i lm(formula = mpg ~ hp * disp, data = listwise_delete(df_missing, c("mpg", "hp", "disp")))
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ hp * disp
      
                                       SS df      MS      F   PRE     p
       ------- --------------- | -------- -- ------- ------ ----- -----
         Model (error reduced) |  926.049  3 308.683 40.553 .8295 .0000
            hp                 |  109.549  1 109.549 14.392 .3654 .0008
          disp                 |  186.831  1 186.831 24.545 .4954 .0000
       hp:disp                 |   75.967  1  75.967  9.980 .2853 .0041
         Error (from model)    |  190.297 25   7.612                   
       ------- --------------- | -------- -- ------- ------ ----- -----
         Total (empty model)   | 1116.346 28  39.869                   

# null model tables are beautifully formatted

    Code
      supernova(model)
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ NULL
      
                                     SS  df     MS   F PRE   p
       ----- --------------- | -------- --- ------ --- --- ---
       Model (error reduced) |      --- ---    --- --- --- ---
       Error (from model)    |      --- ---    --- --- --- ---
       ----- --------------- | -------- --- ------ --- --- ---
       Total (empty model)   | 1126.047  31 36.324            

# single predictor tables are beautifully formatted

    Code
      supernova(model)
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ hp
      
                                     SS df      MS      F   PRE     p
       ----- --------------- | -------- -- ------- ------ ----- -----
       Model (error reduced) |  678.373  1 678.373 45.460 .6024 .0000
       Error (from model)    |  447.674 30  14.922                   
       ----- --------------- | -------- -- ------- ------ ----- -----
       Total (empty model)   | 1126.047 31  36.324                   

# multiple predictor tables are beautifully formatted

    Code
      supernova(model)
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ hp + disp
      
                                     SS df      MS      F   PRE     p
       ----- --------------- | -------- -- ------- ------ ----- -----
       Model (error reduced) |  842.554  2 421.277 43.095 .7482 .0000
          hp                 |   33.665  1  33.665  3.444 .1061 .0737
        disp                 |  164.181  1 164.181 16.795 .3667 .0003
       Error (from model)    |  283.493 29   9.776                   
       ----- --------------- | -------- -- ------- ------ ----- -----
       Total (empty model)   | 1126.047 31  36.324                   

# non-verbose tables do not have a description column

    Code
      supernova(model)
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ NULL
      
                                     SS  df     MS   F PRE   p
       ----- --------------- | -------- --- ------ --- --- ---
       Model (error reduced) |      --- ---    --- --- --- ---
       Error (from model)    |      --- ---    --- --- --- ---
       ----- --------------- | -------- --- ------ --- --- ---
       Total (empty model)   | 1126.047  31 36.324            


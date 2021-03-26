# message is given for number of rows deleted due to missing cases

    Code
      supernova(lm(mpg ~ hp, df_missing))
    Message <simpleMessage>
      Missing cases detected.
      Refitting to remove cases with missing values. The new model is
      lm(formula = mpg ~ hp, data = listwise_delete(df_missing, c("mpg", "hp")))
      1 case removed due to missing value(s).
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ hp
      
                                     SS df      MS      F    PRE     p
       ----- --------------- | -------- -- ------- ------ ------ -----
       Model (error reduced) |  680.166  1 680.166 44.323 0.6045 .0000
       Error (from model)    |  445.027 29  15.346                    
       ----- --------------- | -------- -- ------- ------ ------ -----
       Total (empty model)   | 1125.194 30  37.506                    

---

    Code
      supernova(lm(mpg ~ disp, df_missing))
    Message <simpleMessage>
      Missing cases detected.
      Refitting to remove cases with missing values. The new model is
      lm(formula = mpg ~ disp, data = listwise_delete(df_missing, c("mpg", "disp")))
      2 cases removed due to missing value(s).
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ disp
      
                                     SS df      MS      F    PRE     p
       ----- --------------- | -------- -- ------- ------ ------ -----
       Model (error reduced) |  810.915  1 810.915 74.074 0.7257 .0000
       Error (from model)    |  306.528 28  10.947                    
       ----- --------------- | -------- -- ------- ------ ------ -----
       Total (empty model)   | 1117.443 29  38.533                    

---

    Code
      supernova(lm(mpg ~ hp * disp, df_missing))
    Message <simpleMessage>
      Missing cases detected.
      Refitting to remove cases with missing values. The new model is
      lm(formula = mpg ~ hp * disp, data = listwise_delete(df_missing, c("mpg", "hp", "disp")))
      3 cases removed due to missing value(s).
    Output
       Analysis of Variance Table (Type III SS)
       Model: mpg ~ hp * disp
      
                                       SS df      MS      F    PRE     p
       ------- --------------- | -------- -- ------- ------ ------ -----
         Model (error reduced) |  926.049  3 308.683 40.553 0.8295 .0000
            hp                 |  109.549  1 109.549 14.392 0.3654 .0008
          disp                 |  186.831  1 186.831 24.545 0.4954 .0000
       hp:disp                 |   75.967  1  75.967  9.980 0.2853 .0041
         Error (from model)    |  190.297 25   7.612                    
       ------- --------------- | -------- -- ------- ------ ------ -----
         Total (empty model)   | 1116.346 28  39.869                    


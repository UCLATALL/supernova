# nested repeated measures tables are beautifully formatted

    Code
      supernova(model)
    Output
       Analysis of Variance Table (Type III SS)
       Model: value ~ instructions + (1 | group)
      
                                    SS df     MS     F   PRE     p
       ---------------------- | ------ -- ------ ----- ----- -----
       Between Subjects       |                                   
         instructions         | 12.500  1 12.500 4.687 .5396 .0963
       Error                  | 10.667  4  2.667                  
       Total                  | 23.167  5  4.633                  
       ---------------------- | ------ -- ------ ----- ----- -----
       Within Subjects        |                                   
       Total                  |  5.333 12  0.444                  
       ---------------------- | ------ -- ------ ----- ----- -----
       Total                  | 28.500 17  1.676                  

# crossed repeated measures tables are beautifully formatted

    Code
      supernova(model)
    Output
       Analysis of Variance Table (Type III SS)
       Model: rating ~ sex * yearsmarried * children + (1 | couple)
      
                                         SS df     MS     F   PRE     p
       --------------------------- | ------ -- ------ ----- ----- -----
       Between Subjects            |                                   
         yearsmarried              | 10.125  1 10.125 9.529 .4426 .0094
         children                  |  0.500  1  0.500 0.471 .0377 .5058
         yearsmarried:children     | 10.125  1 10.125 9.529 .4426 .0094
       Error                       | 12.750 12  1.063                  
       Total                       | 33.500 15  2.233                  
       --------------------------- | ------ -- ------ ----- ----- -----
       Within Subjects             |                                   
         sex                       |  3.125  1  3.125 8.824 .4237 .0117
         sex:yearsmarried          |  2.000  1  2.000 5.647 .3200 .0350
         sex:children              |  0.125  1  0.125 0.353 .0286 .5635
         sex:yearsmarried:children |  0.500  1  0.500 1.412 .1053 .2577
       Error                       |  4.250 12  0.354                  
       Total                       | 10.000 16  0.625                  
       --------------------------- | ------ -- ------ ----- ----- -----
       Total                       | 43.500 31  1.403                  


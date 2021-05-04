# it prints nicely for supported models

    Code
      generate_models(test_formula, type = 1)
    Message <cliMessage>
      
      -- Comparison Models for Type III SS -------------------------------------------
      
      -- Full Model 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ NULL
    Message <cliMessage>
      
      -- a 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~     b + c + a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- b 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a +     c + a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b +     a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- a:b 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c +       a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- a:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b +       b:c + a:b:c
    Message <cliMessage>
      
      -- b:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c +       a:b:c
    Message <cliMessage>
      
      -- a:b:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c + b:c
      

---

    Code
      generate_models(test_formula, type = 2)
    Message <cliMessage>
      
      -- Comparison Models for Type III SS -------------------------------------------
      
      -- Full Model 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ NULL
    Message <cliMessage>
      
      -- a 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~     b + c + a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- b 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a +     c + a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b +     a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- a:b 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c +       a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- a:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b +       b:c + a:b:c
    Message <cliMessage>
      
      -- b:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c +       a:b:c
    Message <cliMessage>
      
      -- a:b:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c + b:c
      

---

    Code
      generate_models(test_formula, type = 3)
    Message <cliMessage>
      
      -- Comparison Models for Type III SS -------------------------------------------
      
      -- Full Model 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ NULL
    Message <cliMessage>
      
      -- a 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~     b + c + a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- b 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a +     c + a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b +     a:b + a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- a:b 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c +       a:c + b:c + a:b:c
    Message <cliMessage>
      
      -- a:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b +       b:c + a:b:c
    Message <cliMessage>
      
      -- b:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c +       a:b:c
    Message <cliMessage>
      
      -- a:b:c 
    Output
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c + b:c
      

---

    Code
      generate_models(test_lm, type = 1)
    Message <cliMessage>
      
      -- Comparison Models for Type III SS -------------------------------------------
      
      -- Full Model 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ NULL
    Message <cliMessage>
      
      -- hp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~      disp + hp:disp
    Message <cliMessage>
      
      -- disp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp +        hp:disp
    Message <cliMessage>
      
      -- hp:disp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp + disp
      

---

    Code
      generate_models(test_lm, type = 2)
    Message <cliMessage>
      
      -- Comparison Models for Type III SS -------------------------------------------
      
      -- Full Model 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ NULL
    Message <cliMessage>
      
      -- hp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~      disp + hp:disp
    Message <cliMessage>
      
      -- disp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp +        hp:disp
    Message <cliMessage>
      
      -- hp:disp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp + disp
      

---

    Code
      generate_models(test_lm, type = 3)
    Message <cliMessage>
      
      -- Comparison Models for Type III SS -------------------------------------------
      
      -- Full Model 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ NULL
    Message <cliMessage>
      
      -- hp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~      disp + hp:disp
    Message <cliMessage>
      
      -- disp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp +        hp:disp
    Message <cliMessage>
      
      -- hp:disp 
    Output
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp + disp
      


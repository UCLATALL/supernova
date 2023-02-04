# it prints nicely for supported models

    Code
      generate_models(test_formula, type = 1)
    Output
      
      -- Comparison Models for Type I SS ---------------------------------------------
      
      -- Full Model
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ NULL
      
      -- a
      complex: y ~ a
      simple:  y ~ NULL
      
      -- b
      complex: y ~ a + b
      simple:  y ~ a
      
      -- c
      complex: y ~ a + b + c
      simple:  y ~ a + b
      
      -- a:b
      complex: y ~ a + b + c + a:b
      simple:  y ~ a + b + c
      
      -- a:c
      complex: y ~ a + b + c + a:b + a:c
      simple:  y ~ a + b + c + a:b
      
      -- b:c
      complex: y ~ a + b + c + a:b + a:c + b:c
      simple:  y ~ a + b + c + a:b + a:c
      
      -- a:b:c
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c + b:c
      

---

    Code
      generate_models(test_formula, type = 2)
    Output
      
      -- Comparison Models for Type II SS --------------------------------------------
      
      -- Full Model
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ NULL
      
      -- a
      complex: y ~ a + b + c +             b:c
      simple:  y ~     b + c +             b:c
      
      -- b
      complex: y ~ a + b + c +       a:c
      simple:  y ~ a +     c +       a:c
      
      -- c
      complex: y ~ a + b + c + a:b
      simple:  y ~ a + b +     a:b
      
      -- a:b
      complex: y ~ a + b + c + a:b + a:c + b:c
      simple:  y ~ a + b + c +       a:c + b:c
      
      -- a:c
      complex: y ~ a + b + c + a:b + a:c + b:c
      simple:  y ~ a + b + c + a:b +       b:c
      
      -- b:c
      complex: y ~ a + b + c + a:b + a:c + b:c
      simple:  y ~ a + b + c + a:b + a:c
      
      -- a:b:c
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c + b:c
      

---

    Code
      generate_models(test_formula, type = 3)
    Output
      
      -- Comparison Models for Type III SS -------------------------------------------
      
      -- Full Model
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ NULL
      
      -- a
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~     b + c + a:b + a:c + b:c + a:b:c
      
      -- b
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a +     c + a:b + a:c + b:c + a:b:c
      
      -- c
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b +     a:b + a:c + b:c + a:b:c
      
      -- a:b
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c +       a:c + b:c + a:b:c
      
      -- a:c
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b +       b:c + a:b:c
      
      -- b:c
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c +       a:b:c
      
      -- a:b:c
      complex: y ~ a + b + c + a:b + a:c + b:c + a:b:c
      simple:  y ~ a + b + c + a:b + a:c + b:c
      

---

    Code
      generate_models(test_lm, type = 1)
    Output
      
      -- Comparison Models for Type I SS ---------------------------------------------
      
      -- Full Model
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ NULL
      
      -- hp
      complex: mpg ~ hp
      simple:  mpg ~ NULL
      
      -- disp
      complex: mpg ~ hp + disp
      simple:  mpg ~ hp
      
      -- hp:disp
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp + disp
      

---

    Code
      generate_models(test_lm, type = 2)
    Output
      
      -- Comparison Models for Type II SS --------------------------------------------
      
      -- Full Model
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ NULL
      
      -- hp
      complex: mpg ~ hp + disp
      simple:  mpg ~      disp
      
      -- disp
      complex: mpg ~ hp + disp
      simple:  mpg ~ hp
      
      -- hp:disp
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp + disp
      

---

    Code
      generate_models(test_lm, type = 3)
    Output
      
      -- Comparison Models for Type III SS -------------------------------------------
      
      -- Full Model
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ NULL
      
      -- hp
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~      disp + hp:disp
      
      -- disp
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp +        hp:disp
      
      -- hp:disp
      complex: mpg ~ hp + disp + hp:disp
      simple:  mpg ~ hp + disp
      


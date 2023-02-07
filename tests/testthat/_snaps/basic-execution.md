# basic execution

    Code
      print(c5_xy)
    Output
      
      Call:
      C5.0.default(x = dat_tr[, -1], y = dat_tr$class)
      
      Classification Tree
      Number of samples: 100 
      Number of predictors: 15 
      
      Tree size: 8 
      
      Non-standard options: attempt to group attributes
      

---

    Code
      print(c5_fm)
    Output
      
      Call:
      C5.0.formula(formula = class ~ ., data = dat_tr)
      
      Classification Tree
      Number of samples: 100 
      Number of predictors: 15 
      
      Tree size: 8 
      
      Non-standard options: attempt to group attributes
      

---

    Code
      C5.0Control()
    Output
      $subset
      [1] TRUE
      
      $bands
      [1] 0
      
      $winnow
      [1] FALSE
      
      $noGlobalPruning
      [1] FALSE
      
      $CF
      [1] 0.25
      
      $minCases
      [1] 2
      
      $fuzzyThreshold
      [1] FALSE
      
      $sample
      [1] 0
      
      $earlyStopping
      [1] TRUE
      
      $label
      [1] "outcome"
      
      $seed
      [1] 974
      


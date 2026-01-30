module PropFormula
  ( PropFormula,
  )
where

import Formula (Formula)

type PropFormula v = Formula v v

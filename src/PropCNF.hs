module PropCNF
  ( cnf,
  )
where

import Formula (Formula (..))
import PropFormula (Prop (Prop), PropFormula, PropLiteral, lit, lit', litNot)

newtype PropCNF v = PropCNF [[PropLiteral v]]

cnf :: (Ord v, Enum v) => PropFormula v -> PropCNF v
cnf formula =
  let (l, PropCNF delta, _) = cnf' (formula, PropCNF [], undefined)
   in PropCNF $ [l] : delta
  where
    cnf' ::
      (Ord v, Enum v) =>
      (PropFormula v, PropCNF v, v) ->
      (PropLiteral v, PropCNF v, v)
    cnf' (f, delta, new) = case f of
      Atom (Prop x) -> (lit x, delta, new)
      Not p -> (litNot l, delta', new')
        where
          (l, delta', new') = cnf' (p, delta, new)
      p :& q -> (lit new', PropCNF delta', succ new')
        where
          (l1, delta1, new1) = cnf' (p, delta, new)
          (l2, PropCNF delta2, new') = cnf' (q, delta1, new1)
          delta' =
            [lit' new', l1]
              : [lit' new', l2]
              : [litNot l1, litNot l2, lit new']
              : delta2
      p :| q -> (lit new', PropCNF delta', succ new')
        where
          (l1, delta1, new1) = cnf' (p, delta, new)
          (l2, PropCNF delta2, new') = cnf' (q, delta1, new1)
          delta' =
            [litNot l1, lit new']
              : [litNot l2, lit new']
              : [lit' new', l1, l2]
              : delta2
      p :-> q -> cnf' (Not p :| q, delta, new)

module Cardano.Ledger.Pivo.TxBody
  (TxBody (TxBody))
where

-- todo: all eras have to define the transaction body from scratch if they
-- modify one of the transaction fields. For integrating the Priviledge
-- prototype we need to incorporate our own update payload, hence we need to
-- define a new data type for the transaction body.
data TxBody era = TxBody

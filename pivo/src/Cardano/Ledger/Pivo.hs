{-# LANGUAGE TypeFamilies #-}

-- | Priviledge is not Voltaire (Pivo)
--
module Cardano.Ledger.Pivo where

import qualified Cardano.Ledger.Crypto
import Cardano.Ledger.Era (Era (Crypto))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Pivo.TxBody (TxBody)
import qualified Cardano.Ledger.Mary.Value as Mary.Value
import Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMA.Timelocks
import qualified Shelley.Spec.Ledger.Tx as Shelley.Tx

data PivoEra c

instance
  (Cardano.Ledger.Crypto.Crypto c) =>
  Era (PivoEra c)
  where
  type Crypto (PivoEra c) = c

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.TxBody (PivoEra c) = TxBody (PivoEra c)

type instance Core.TxOut (PivoEra c) = Shelley.Tx.TxOut (PivoEra c)

type instance Core.Value (PivoEra c) = Mary.Value.Value c

type instance Core.Script (PivoEra c) = ShelleyMA.Timelocks.Timelock (PivoEra c)

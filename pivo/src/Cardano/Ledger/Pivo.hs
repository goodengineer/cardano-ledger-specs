{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Priviledge is not Voltaire (Pivo)
--
module Cardano.Ledger.Pivo where

import GHC.Records (HasField)
import Control.DeepSeq (deepseq)

import Cardano.Crypto.Hash (castHash, hashWithSerialiser)
import Cardano.Binary (toCBOR)

import Shelley.Spec.Ledger.Metadata (validMetadatum)
import qualified Cardano.Ledger.Crypto
import Cardano.Ledger.Era (Era (Crypto))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Pivo.TxBody (TxBody)
import Cardano.Ledger.AuxiliaryData (ValidateAuxiliaryData ( hashAuxiliaryData
                                                           , validateAuxiliaryData
                                                           )
                                    , AuxiliaryDataHash (AuxiliaryDataHash))

import qualified Cardano.Ledger.Mary.Value as Mary.Value
import qualified Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMA.Timelocks
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as ShelleyMA.AuxiliaryData
import qualified Shelley.Spec.Ledger.Tx as Shelley.Tx
import Cardano.Ledger.Shelley.Constraints (UsesTxBody, UsesValue, UsesTxOut (makeTxOut))

-- Import needed to define era mapping instances
import qualified Shelley.Spec.Ledger.API as Shelley.API
-- TODO: I'd add these imports in a Shelley.Spec.Ledger.STS module that
-- re-exports these symbols so that it becomes clear where each rule came from.
import qualified Shelley.Spec.Ledger.STS.Bbody as Shelley
import qualified Shelley.Spec.Ledger.STS.Epoch as Shelley
import qualified Shelley.Spec.Ledger.STS.Mir as Shelley
import qualified Shelley.Spec.Ledger.STS.Newpp as Shelley
import qualified Shelley.Spec.Ledger.STS.Ocert as Shelley
import qualified Shelley.Spec.Ledger.STS.Overlay as Shelley
import qualified Shelley.Spec.Ledger.STS.Rupd as Shelley
import qualified Shelley.Spec.Ledger.STS.Snap as Shelley
import qualified Shelley.Spec.Ledger.STS.Tick as Shelley
import qualified Shelley.Spec.Ledger.STS.Upec as Shelley

import Cardano.Ledger.Pivo.Rules as Pivo.Rules

data PivoEra c

instance
  (Cardano.Ledger.Crypto.Crypto c) =>
  Era (PivoEra c)
  where
  type Crypto (PivoEra c) = c

--------------------------------------------------------------------------------
-- API instances
--------------------------------------------------------------------------------

instance Shelley.API.PraosCrypto c => Shelley.API.ApplyTx (PivoEra c)
instance Shelley.API.PraosCrypto c => Shelley.API.ApplyBlock (PivoEra c)
instance Shelley.API.PraosCrypto c => Shelley.API.GetLedgerView (PivoEra c)
instance Shelley.API.PraosCrypto c => Shelley.API.ShelleyBasedEra (PivoEra c)

--------------------------------------------------------------------------------
-- Core instances
--------------------------------------------------------------------------------

type instance Core.TxBody (PivoEra c) = TxBody (PivoEra c)

type instance Core.TxOut (PivoEra c) = Shelley.Tx.TxOut (PivoEra c)

type instance Core.Value (PivoEra c) = Mary.Value.Value c

type instance Core.Script (PivoEra c) = ShelleyMA.Timelocks.Timelock c

type instance Core.AuxiliaryData (PivoEra c) = ShelleyMA.AuxiliaryData.AuxiliaryData (PivoEra c)

--------------------------------------------------------------------------------
-- Ledger data instances
--------------------------------------------------------------------------------

instance
  ( Cardano.Ledger.Crypto.Crypto c,
    UsesTxBody (PivoEra c),
    Core.AnnotatedData (Core.AuxiliaryData (PivoEra c)),
    (HasField "vldt" (Core.TxBody (PivoEra c)) ShelleyMA.Timelocks.ValidityInterval)
  ) =>
  Shelley.Tx.ValidateScript (PivoEra c)
  where
  validateScript s tx = ShelleyMA.Timelocks.validateTimelock s tx
  hashScript s = ShelleyMA.Timelocks.hashTimelockScript s

instance
  ( Cardano.Ledger.Crypto.Crypto c,
    Core.AnnotatedData (Core.Script (PivoEra c))
  ) =>
  ValidateAuxiliaryData (PivoEra c)
  where
  hashAuxiliaryData
    = AuxiliaryDataHash . castHash . hashWithSerialiser toCBOR
  validateAuxiliaryData (ShelleyMA.AuxiliaryData.AuxiliaryData md as)
    = deepseq as $ all validMetadatum md

instance Cardano.Ledger.Crypto.Crypto c => UsesValue (PivoEra c)

instance Cardano.Ledger.Crypto.Crypto c => UsesTxOut (PivoEra c) where
  makeTxOut _ a v = Shelley.API.TxOut a v

--------------------------------------------------------------------------------
-- Ledger rules instances (era mapping)
--------------------------------------------------------------------------------

type instance Core.EraRule "BBODY" (PivoEra c) = Shelley.BBODY (PivoEra c)

type instance Core.EraRule "DELEG" (PivoEra c) = Shelley.API.DELEG (PivoEra c)

type instance Core.EraRule "DELEGS" (PivoEra c) = Shelley.API.DELEGS (PivoEra c)

type instance Core.EraRule "DELPL" (PivoEra c) = Shelley.API.DELPL (PivoEra c)

type instance Core.EraRule "EPOCH" (PivoEra c) = Shelley.EPOCH (PivoEra c)

type instance Core.EraRule "LEDGER" (PivoEra c) = Shelley.API.LEDGER (PivoEra c)

type instance Core.EraRule "LEDGERS" (PivoEra c) = Shelley.API.LEDGERS (PivoEra c)

type instance Core.EraRule "MIR" (PivoEra c) = Shelley.MIR (PivoEra c)

type instance Core.EraRule "NEWEPOCH" (PivoEra c) = Shelley.API.NEWEPOCH (PivoEra c)

type instance Core.EraRule "NEWPP" (PivoEra c) = Shelley.NEWPP (PivoEra c)

type instance Core.EraRule "OCERT" (PivoEra c) = Shelley.OCERT (PivoEra c)

type instance Core.EraRule "OVERLAY" (PivoEra c) = Shelley.OVERLAY (PivoEra c)

type instance Core.EraRule "POOL" (PivoEra c) = Shelley.API.POOL (PivoEra c)

type instance Core.EraRule "POOLREAP" (PivoEra c) = Shelley.API.POOLREAP (PivoEra c)

type instance Core.EraRule "PPUP" (PivoEra c) = Shelley.API.PPUP (PivoEra c)

type instance Core.EraRule "RUPD" (PivoEra c) = Shelley.RUPD (PivoEra c)

type instance Core.EraRule "SNAP" (PivoEra c) = Shelley.SNAP (PivoEra c)

type instance Core.EraRule "TICK" (PivoEra c) = Shelley.TICK (PivoEra c)

type instance Core.EraRule "TICKF" (PivoEra c) = Shelley.TICKF (PivoEra c)

type instance Core.EraRule "TICKN" (PivoEra _c) = Shelley.API.TICKN

type instance Core.EraRule "UPEC" (PivoEra c) = Shelley.UPEC (PivoEra c)

-- We cannot reuse the ShelleyMA rules because we need the same transaction body
-- as this era:
--
-- >    • Couldn't match type ‘TxBody (PivoEra c)’
-- >                     with ‘Cardano.Ledger.ShelleyMA.TxBody.TxBody (PivoEra c)’
--

-- todo: define custom utxo rules for Pivo

type instance Core.EraRule "UTXO" (PivoEra c) = Pivo.Rules.UTXO (PivoEra c)
type instance Core.EraRule "UTXOW" (PivoEra c) = Pivo.Rules.UTXOW (PivoEra c)

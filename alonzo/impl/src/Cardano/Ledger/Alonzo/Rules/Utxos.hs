{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Alonzo.Rules.Utxos where

import Cardano.Ledger.Alonzo.Scripts (Script)
import Cardano.Ledger.Alonzo.Tx
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.Shelley.Constraints (TransValue, UsesAuxiliary)
import qualified Cardano.Ledger.Val as Val
import Control.Iterate.SetAlgebra (eval, (∪), (⋪), (◁))
import Control.State.Transition.Extended
import Data.Foldable (toList)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import NoThunks.Class (NoThunks)
import Shelley.Spec.Ledger.LedgerState
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import Shelley.Spec.Ledger.STS.Ppup (PPUPEnv (..))
import Shelley.Spec.Ledger.STS.Utxo (UtxoEnv (..))
import Shelley.Spec.Ledger.UTxO (balance, totalDeposits)

--------------------------------------------------------------------------------
-- The UTXOS transition system
--------------------------------------------------------------------------------

data UTXOS eraq

instance
  forall era.
  ( Era era,
    Core.Script era ~ Script era
  ) =>
  STS (UTXOS era)
  where
  type Environment (UTXOS era) = UtxoEnv era
  type State (UTXOS era) = UTxOState era
  type Signal (UTXOS era) = Tx era
  type PredicateFailure (UTXOS era) = UtxosPredicateFailure era

  transitionRules = [utxosTransition]

utxosTransition ::
  forall era.
  ( UsesAuxiliary era,
    Core.Script era ~ Script era
  ) =>
  TransitionRule (UTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (UtxoEnv _ pp _ _, UTxOState utxo _ _ _, tx)) ->
    let sLst = collectNNScriptInputs pp tx utxo
        scriptEvalResult = evalScripts @era sLst
     in if scriptEvalResult
          then scriptsValidateTransition
          else scriptsNotValidateTransition

scriptsValidateTransition ::
  forall era.
  ( Environment (Core.EraRule "PPUP" era) ~ PPUPEnv era
  ) =>
  TransitionRule (UTXOS era)
scriptsValidateTransition = do
  TRC
    ( UtxoEnv slot pp poolParams genDelegs,
      UTxOState utxo deposited fees pup,
      tx
      ) <-
    judgmentContext
  let txb = txbody tx
      refunded = keyRefunds pp txb
      depositChange =
        totalDeposits
          pp
          poolParams
          (toList $ getField @"certs" txb)
  isValidating tx == IsValidating True ?! ValidationTagMismatch (isValidating tx)
  pup' <-
    trans @(Core.EraRule "PPUP" era) $
      TRC
        (PPUPEnv slot pp genDelegs, pup, getField @"update" tx)
  pure $
    UTxOState
      { _utxo = eval ((getField @"inputs" txb ◁ utxo) ∪ getField @"outputs" txb),
        _deposited = deposited <> depositChange,
        _fees = fees <> getField @"fee" txb,
        _ppups = pup'
      }

scriptsNotValidateTransition :: forall era. TransitionRule (UTXOS era)
scriptsNotValidateTransition = do
  TRC (_, us@(UTxOState utxo _ fees _), tx) <- judgmentContext
  let txb = txbody tx
  isValidating tx == IsValidating False ?! ValidationTagMismatch (isValidating tx)
  pure $
    us
      { _utxo = eval (txinputs_fee txb ⋪ utxo),
        _fees = fees <> Val.coin (balance @era (eval (txinputs_fee txb ◁ utxo)))
      }

data UtxosPredicateFailure era
  = -- | The 'isValidating' tag on the transaction is incorrect. The tag given
    --   here is that provided on the transaction (whereas evaluation of the
    --   scripts gives the opposite.)
    ValidationTagMismatch IsValidating
  | UpdateFailure (PredicateFailure (Core.EraRule "PPUP" era))
  deriving
    (Generic)

deriving stock instance
  ( Shelley.TransUTxOState Show era,
    TransValue Show era,
    Show (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Show (UtxosPredicateFailure era)

deriving stock instance
  ( Shelley.TransUTxOState Eq era,
    TransValue Eq era,
    Eq (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  Eq (UtxosPredicateFailure era)

instance
  ( Shelley.TransUTxOState NoThunks era,
    NoThunks (PredicateFailure (Core.EraRule "PPUP" era))
  ) =>
  NoThunks (UtxosPredicateFailure era)

--------------------------------------------------------------------------------
-- UTXOS helper functions
--------------------------------------------------------------------------------

module MIPS.RegisterAllocator.Allocation where

import qualified MIPS.Types.Physical as P
import qualified MIPS.Types.Virtual  as V
import MIPS.RegisterAllocator.Monad.Class

virtToEmitPhysMIPS
  :: MonadAllocator m
  => V.MipsVirtual
  -> m ()
virtToEmitPhysMIPS mv = case mv of
  V.Addi d s i -> regs_dx (d, s) $ \(d', s') ->
    emit [P.Addi d' s' i]

  V.Add d s t -> regs_dxy (d, s, t) $ \(d', s', t') ->
    emit [P.Add d' s' t']
  
  V.Sub d s t -> regs_dxy (d, s, t) $ \(d', s', t') ->
    emit [P.Sub d' s' t']
  
  V.SubVI d s i -> regs_dxi (d, s, i) $ \(d', s', i') ->
    emit [P.Sub d' s' i']
  
  V.SubIV d i s -> regs_dix (d, i, s) $ \(d', i', s') ->
    emit [P.Sub d' i' s']
  
  V.Mult d s t -> regs_dxy (d, s, t) $ \(d', s', t') -> 
    emit [ P.Mult s' t', P.Mflo d' ]
  
  V.Multi d s i -> regs_dxi (d, s, i) $ \(d', s', i') ->
    emit [ P.Mult s' i', P.Mflo d' ]
  
  V.Div d s t -> regs_dxy (d, s, t) $ \(d', s', t') ->
    emit [ P.Div s' t', P.Mflo d' ]

  V.DivVI d s i -> regs_dxi (d, s, i) $ \(d', s', i') ->
    emit [ P.Div s' i', P.Mflo d' ]
  
  V.DivIV d i s -> regs_dix (d, s, i) $ \(d', i', s') ->
    emit [ P.Div i' s', P.Mflo d' ]
  
  V.Andi d s i -> regs_dx (d, s) $ \(d', s') ->
    emit [ P.Andi d' s' i ]
  
  V.And d s t -> regs_dxy (d, s, t) $ \(d', s', t') ->
    emit [ P.And s' t' ]

  V.Ori d s i -> regs_dx (d, s) $ \(d', s') ->
    emit [ P.Ori d' s' i ]
  
  V.Or d s t -> regs_dxy (d, s, t) $ \(d', s', t') ->
    emit [ P.Or s' t' ]

  _ -> _ -- TODO !!! :)
  
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This is a tutorial on implementing custom effects.
--
-- == What is an effect?
--
-- Algebraic effects are an approach to computations based on the premise that
-- impure behaviour arises from a set of operations. An effect is made up of two
-- parts -- requests and handlers. One can think of requests as  exceptions in
-- other languages and handlers as exception handlers, except that an effect
-- handler may return to the code that sent the request.
--
-- === External interface
--
-- This is the external interface of the effect that users of the effect use.
--
-- 1. __Requests syntax__: the set of operations that enables this effect.
--
-- 2. __Requests interpreter (aka handler)__: the function which interprets the
-- requests.
--
-- === Internal implementation
--
-- In order to implement the above, the implementer has to, additionally, think
-- of:
--
-- 1. __Request messages__: the messages that are dispatched to the handler by
-- user requests.
--
-- 2. __Interpreter logic__: how to process and respond to the requests.
--
-- == How do we implement an effect?
--
-- Each of the four parts above corresponds to something that's needed for
-- implementing the effect:
--
-- 1. __A datatype (/internal/)__: these define the /requests/ that the handler
-- receives.
--
-- 2. __Request functions (/external/)__: these define the /syntax/ that users
-- of the effect will use.
--
-- 3. __A 'Control.Eff.Extend.Handle' instance (/internal/)__: this defines the
-- /interpreter logic/, i.e., how the handler responds to the requests.
--
-- 4. __A handler using the 'Control.Eff.Extend.Handle' instance (/external/)__:
-- this is the /handler/ that users of the effect invoke. We define this by:
--
--     * invoking 'Control.Eff.Extend.handle_relay'
--     * passing an argument which specifies how to embed pure values
--     * tying the recursive-knot using 'Data.Function.fix'
--
-- == The effect design process
--
-- * As with all things, this starts with a name (say @t@). The name of the
-- effect is also the name we give the datatype.
--
-- * Having decided the name of the effect, we need to determine the DSL
-- syntax. The syntax names are shared both by the data constructors as well as
-- the smart constructors.
--
-- * We need to define the types for the DSL syntax. Since this is an effectful
-- DSL, each DSL component will have a type of the form:
--
--     * @Member t r => Eff r b@, or
--     * @Member t r => a0 -> Eff r b@.
--     * @Member t r => a0 -> a1 -> Eff r b@.
--     * ...
--
-- * Having decided the types, we have to think about the implementation.
--
-- Let's implement a DSL describing a simple assembly language for an abstract
-- processor.
module Main where

import Control.Eff
import Control.Eff.Extend
import Data.Function (fix)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- |Our CPU will have an internal state, consisting of several general-purpose
--  registers.
data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6
  deriving (Eq,Ord,Enum,Show)

-- |The state itself, containing the register map.
data CPUState = CPUState { cpuRegisters :: Map Reg Int
                         } deriving (Show)
-- |In the initial state all registers are set to zero.
initialState :: CPUState
initialState = CPUState $ Map.fromList $ zip [R0 .. R6] (repeat 0)

-- |The next thing we have to define is the datatype for our DSL. It will
--  represent the set of possible requests the code may send to a handler.
--  The name of this type is also used in the effects list. Usually, these
--  datatypes are defined as GADTs, though this is not a strict requirement.
--  The type must have at least one parameter, which represents a return value
--  for a given request.
data CPU v where
  -- | Arithmentic operations.
  Add :: Reg -> Reg -> Reg -> CPU ()
  Sub :: Reg -> Reg -> Reg -> CPU ()
  Mul :: Reg -> Reg -> Reg -> CPU ()
  Div :: Reg -> Reg -> Reg -> CPU ()

  Mov :: Int -> Reg -> CPU ()

  PrintReg :: Reg -> CPU ()

-- |Almost done now. The only thing left is to define a handler function.
--  Extensible Effects library allows to do this in several ways. The most common
--  is to define an instance of 'Handle' class for your type.
instance Handle CPU r a (CPUState -> k) where
  handle h q req s = h (q ^$ ()) (runOp req)
    where runOp (Add op1 op2 tgt) = let v1 = cpuRegisters s ! op1
                                        v2 = cpuRegisters s ! op2
                                    in CPUState $ Map.insert tgt (v1 + v2) (cpuRegisters s)
          runOp (Sub op1 op2 tgt) = let v1 = cpuRegisters s ! op1
                                        v2 = cpuRegisters s ! op2
                                    in CPUState $ Map.insert tgt (v1 - v2) (cpuRegisters s)
          runOp (Mul op1 op2 tgt) = let v1 = cpuRegisters s ! op1
                                        v2 = cpuRegisters s ! op2
                                    in CPUState $ Map.insert tgt (v1 * v2) (cpuRegisters s)
          runOp (Div op1 op2 tgt) = let v1 = cpuRegisters s ! op1
                                        v2 = cpuRegisters s ! op2
                                    in CPUState $ Map.insert tgt (v1 `div` v2) (cpuRegisters s)
          runOp (Mov i reg) = CPUState $ Map.insert reg i (cpuRegisters s)
          -- | Here we ignore the rest of available ops.
          runOp _ = s


main :: IO ()
main = return ()

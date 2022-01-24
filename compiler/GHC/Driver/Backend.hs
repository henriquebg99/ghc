{-# LANGUAGE MultiWayIf #-}


-- mutual recursion: Backend is in DynFlags, so can't include a
-- function that depends on DynFlags, even just to extract something 
-- from DynFlags

-- | Code generation backends
module GHC.Driver.Backend
   ( Backend -- export LegacyBackend(..) with legacyBackendUnsafe
   , platformDefaultBackend
   , platformNcgSupported
   , backendProducesObject
   , backendNeedsLink
   , backendGeneratesCode
   , backendInterfaceHasCodegen
   , backendRetainsAllBindings

   , backendWantsLlvmCppMacros
   , backendWantsClangTools

   , backendNeedsFullWays

   , ncgBackend
   , llvmBackend
   , viaCBackend
   , interpreterBackend
   , noBackend

   , useNcgPrimitives
   , useLlvmPrimitives


   , backendSupportsSwitch

   , backendValidityOfCExportStatic
   , backendValidityOfCImport

   , backendSupportsStopC

   , supportsHpc
   , needsPlatformNcgSupport

   , backendUnregisterisedOnly
   , canReplaceViaC
   , canBeReplacedByViaC
   , backendDescription

   , backendForcesOptimization0
   , backendSplitsProcPoints

   , backendSpecialModuleSource

   , backendWantsBreakpointTicks

   , backendSupportsEmbeddedBlobs

   , backendSupportsSimd
   , backendNoSimdMessage

   , backendSptIsDynamic

   , backendInhibitsInterfaceWriting

   , backendIgnoresSpecialise

   , backendWantsInterfaceFile

   , backendNormalSuccessorPhase

   , backendPipelineOutput

   , backendPipeline, PipelineName(..)

   )

where

import GHC.Driver.Backend.Rep
--import GHC.Driver.Phases
import GHC.Prelude
import GHC.Platform
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Driver.Pipeline.Monad

import GHC.Driver.Phases

-- | We really hope to get rid of this, but...

data PipelineName = ViaCPipeline | NCGPipeline | LLVMPipeline | NoPipeline

backendPipeline :: Backend -> PipelineName
backendPipeline ViaC = ViaCPipeline
backendPipeline NCG = NCGPipeline
backendPipeline LLVM = LLVMPipeline
backendPipeline _ = NoPipeline

backendUnregisterisedOnly :: Backend -> Bool

backendUnregisterisedOnly ViaC = True
backendUnregisterisedOnly _ = False

canReplaceViaC :: Backend -> Bool
canReplaceViaC NCG = True
canReplaceViaC LLVM = True
canReplaceViaC _ = False

canBeReplacedByViaC :: Backend -> Bool
canBeReplacedByViaC NCG = True
canBeReplacedByViaC LLVM = True
canBeReplacedByViaC _ = False

needsPlatformNcgSupport :: Backend -> Bool
needsPlatformNcgSupport NCG = True
needsPlatformNcgSupport _ = False

backendForcesOptimization0 :: Backend -> Bool
backendForcesOptimization0 Interpreter = True
backendForcesOptimization0 _ = False

backendNeedsFullWays :: Backend -> Bool
backendNeedsFullWays Interpreter = True
backendNeedsFullWays _ = False

backendSupportsStopC :: Backend -> Bool
backendSupportsStopC ViaC = True
backendSupportsStopC _ = False

backendDescription :: Backend -> String
-- ^ For use in issuing warning messages *only*.  If code depends
-- on what's in the string, you deserve what happens to you.

backendDescription NCG = "native code generator"
backendDescription LLVM =  "LLVM"
backendDescription ViaC = "compiling via C"
backendDescription Interpreter = "byte-code interpreter"
backendDescription NoBackend = "no code generated"



ncgBackend, llvmBackend, viaCBackend, interpreterBackend, noBackend :: Backend

ncgBackend = NCG
llvmBackend = LLVM
viaCBackend = ViaC
interpreterBackend = Interpreter
noBackend = NoBackend

useNcgPrimitives :: Backend -> Bool
useNcgPrimitives NCG = True
useNcgPrimitives _ = False

useLlvmPrimitives :: Backend -> Bool
useLlvmPrimitives LLVM = True
useLlvmPrimitives _ = False

-- these are checks for foreign declarations

-- N.B. If there is no back end, all imports and exports are considered valid.
backendValidityOfCExportStatic :: Backend -> Validity
backendValidityOfCExportStatic NoBackend = IsValid
backendValidityOfCExportStatic ViaC = IsValid
backendValidityOfCExportStatic NCG  = IsValid
backendValidityOfCExportStatic LLVM = IsValid
backendValidityOfCExportStatic _
  = NotValid (text "requires unregisterised, llvm (-fllvm) or native code generation (-fasm)")

-- | Checking a supported backend is in use
backendValidityOfCImport :: Backend -> Validity
backendValidityOfCImport NoBackend   = IsValid
backendValidityOfCImport ViaC        = IsValid
backendValidityOfCImport NCG         = IsValid
backendValidityOfCImport LLVM        = IsValid
backendValidityOfCImport Interpreter = IsValid


supportsHpc :: Backend -> Bool
supportsHpc Interpreter = False
supportsHpc _ = True


-- | Default backend to use for the given platform.
platformDefaultBackend :: Platform -> Backend
platformDefaultBackend platform = if
      | platformUnregisterised platform -> ViaC
      | platformNcgSupported platform   -> NCG
      | otherwise                       -> LLVM


-- | Is the platform supported by the Native Code Generator?
platformNcgSupported :: Platform -> Bool
platformNcgSupported platform = if
      | platformUnregisterised platform -> False -- NCG doesn't support unregisterised ABI
      | ncgValidArch                    -> True
      | otherwise                       -> False
   where
      ncgValidArch = case platformArch platform of
         ArchX86       -> True
         ArchX86_64    -> True
         ArchPPC       -> True
         ArchPPC_64 {} -> True
         ArchSPARC     -> True
         ArchAArch64   -> True
         _             -> False

-- | Will this backend produce an object file on the disk?
backendProducesObject :: Backend -> Bool
backendProducesObject ViaC        = True
backendProducesObject NCG         = True
backendProducesObject LLVM        = True
backendProducesObject Interpreter = False
backendProducesObject NoBackend   = False

backendNeedsLink :: Backend -> Bool
backendNeedsLink NoBackend = False
backendNeedsLink _ = True

backendGeneratesCode :: Backend -> Bool
backendGeneratesCode NoBackend = False
backendGeneratesCode _ = True

backendInhibitsInterfaceWriting :: Backend -> Bool
backendInhibitsInterfaceWriting NoBackend = True
backendInhibitsInterfaceWriting _ = False

-- | Does this backend retain *all* top-level bindings for a module,
-- rather than just the exported bindings, in the TypeEnv and compiled
-- code (if any)?
--
-- Interpreter backend does this, so that GHCi can call functions inside a
-- module.
--
-- When no backend is used we also do it, so that Haddock can get access to the
-- GlobalRdrEnv for a module after typechecking it.
backendRetainsAllBindings :: Backend -> Bool
backendRetainsAllBindings Interpreter = True
backendRetainsAllBindings NoBackend   = True
backendRetainsAllBindings ViaC        = False
backendRetainsAllBindings NCG         = False
backendRetainsAllBindings LLVM        = False


-- | Does the backend support switch out of the box? Then leave this to the
-- backend!
backendSupportsSwitch :: Backend -> Bool
backendSupportsSwitch ViaC = True
backendSupportsSwitch LLVM = True
backendSupportsSwitch _    = False

backendSplitsProcPoints :: Backend -> Bool
backendSplitsProcPoints NCG = False
backendSplitsProcPoints _   = True

-- | Used to help characterize the source of code in GHC.Unit.Module.Graph.
-- The Boolean is a "recomp" flag...
backendSpecialModuleSource :: Bool -> Backend -> Maybe String
backendSpecialModuleSource True Interpreter = Just "interpreted"
backendSpecialModuleSource _ NoBackend = Just "nothing"
backendSpecialModuleSource _ _ = Nothing

backendWantsBreakpointTicks :: Backend -> Bool
backendWantsBreakpointTicks Interpreter = True
backendWantsBreakpointTicks _ = False


backendSupportsSimd :: Backend -> Bool
backendSupportsSimd LLVM = True
backendSupportsSimd _ = False

backendNoSimdMessage :: String
backendNoSimdMessage =
  unlines [ "SIMD vector instructions require the LLVM back-end."
          , "Please use -fllvm."]

-- See Note [Embedding large binary blobs] in GHC.CmmToAsm.Ppr

backendSupportsEmbeddedBlobs :: Backend -> Bool
backendSupportsEmbeddedBlobs NCG = True
backendSupportsEmbeddedBlobs _ = False


-- If we are compiling for the interpreter we will insert
-- any necessary SPT entries dynamically
backendSptIsDynamic :: Backend -> Bool
backendSptIsDynamic Interpreter = True
backendSptIsDynamic _ = False

backendIgnoresSpecialise :: Backend -> Bool
backendIgnoresSpecialise NoBackend = True
backendIgnoresSpecialise Interpreter = True
backendIgnoresSpecialise _ = False


backendWantsInterfaceFile :: Backend -> Bool
backendWantsInterfaceFile Interpreter = False
backendWantsInterfaceFile NoBackend = False
backendWantsInterfaceFile _ = True

backendInterfaceHasCodegen :: Backend -> Bool
backendInterfaceHasCodegen Interpreter = False
backendInterfaceHasCodegen NoBackend = False
backendInterfaceHasCodegen _ = True

-- might make more sense in GHC.Driver.Backend.Output
backendNormalSuccessorPhase :: Backend -> Phase
backendNormalSuccessorPhase ViaC        = HCc
backendNormalSuccessorPhase NCG         = As False
backendNormalSuccessorPhase LLVM        = LlvmOpt
backendNormalSuccessorPhase NoBackend   = StopLn
backendNormalSuccessorPhase Interpreter = StopLn

backendWantsClangTools :: Backend -> Bool
backendWantsClangTools LLVM = True
backendWantsClangTools _ = False

-- I really wanted `backendCppMacros :: Backend -> IO [String]`,
-- but the code depends on `DynFlags` so it couldn't go in this module,
-- and I didn't think it was worth creating another module.
backendWantsLlvmCppMacros :: Backend -> Bool
backendWantsLlvmCppMacros LLVM = True
backendWantsLlvmCppMacros _ = False







backendPipelineOutput :: Backend -> PipelineOutput
backendPipelineOutput Interpreter = NoOutputFile
backendPipelineOutput NoBackend = NoOutputFile
backendPipelineOutput _ = Persistent


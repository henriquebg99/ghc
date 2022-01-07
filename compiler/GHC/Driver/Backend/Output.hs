module GHC.Driver.Backend.Output
  ( backendCodeOutput
  )
where

import GHC.Driver.Backend.Rep

import GHC.Prelude

import GHC.CmmToAsm     ( nativeCodeGen )
import GHC.CmmToLlvm    ( llvmCodeGen )

import GHC.CmmToC           ( cmmToC )
import GHC.Cmm              ( RawCmmGroup )
import GHC.Cmm.CLabel

import GHC.Driver.Session
import GHC.Driver.Config.CmmToAsm  (initNCGConfig)
import GHC.Driver.Config.CmmToLlvm (initLlvmCgConfig)

import GHC.Data.Stream           ( Stream )
import qualified GHC.Data.Stream as Stream


import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Logger
import GHC.Utils.Exception (bracket)
import GHC.Utils.Ppr (Mode(..))

import GHC.Unit

import GHC.Types.Unique.Supply ( mkSplitUniqSupply )

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set



backendCodeOutput
    :: Backend
    -> Logger
    -> DynFlags
    -> Module -- this_mode
    -> ModLocation
    -> FilePath -- filenm
    -> Set UnitId -- ^ Dependencies
    -> Stream IO RawCmmGroup a -- linted_cmm_stream
    -> IO a

backendCodeOutput backend logger dflags this_mod location filenm pkg_deps 
                  linted_cmm_stream 
  = case backend of
      NCG         -> outputAsm logger dflags this_mod location filenm
                     linted_cmm_stream
      ViaC        -> outputC logger dflags filenm linted_cmm_stream pkg_deps
      LLVM        -> outputLlvm logger dflags filenm linted_cmm_stream
      Interpreter -> panic "codeOutput: Interpreter"
      NoBackend   -> panic "codeOutput: NoBackend"

{-
************************************************************************
*                                                                      *
\subsection{C}
*                                                                      *
************************************************************************
-}

outputC :: Logger
        -> DynFlags
        -> FilePath
        -> Stream IO RawCmmGroup a
        -> Set UnitId
        -> IO a
outputC logger dflags filenm cmm_stream unit_deps =
  withTiming logger (text "C codegen") (\a -> seq a () {- FIXME -}) $ do
    let pkg_names = map unitIdString (Set.toAscList unit_deps)
    doOutput filenm $ \ h -> do
      hPutStr h ("/* GHC_PACKAGES " ++ unwords pkg_names ++ "\n*/\n")
      hPutStr h "#include \"Stg.h\"\n"
      let platform = targetPlatform dflags
          writeC cmm = do
            let doc = cmmToC platform cmm
            putDumpFileMaybe logger Opt_D_dump_c_backend
                          "C backend output"
                          FormatC
                          doc
            let ctx = initSDocContext dflags (PprCode CStyle)
            printSDocLn ctx LeftMode h doc
      Stream.consume cmm_stream id writeC

{-
************************************************************************
*                                                                      *
\subsection{Assembler}
*                                                                      *
************************************************************************
-}

outputAsm :: Logger
          -> DynFlags
          -> Module
          -> ModLocation
          -> FilePath
          -> Stream IO RawCmmGroup a
          -> IO a
outputAsm logger dflags this_mod location filenm cmm_stream = do
  ncg_uniqs <- mkSplitUniqSupply 'n'
  debugTraceMsg logger 4 (text "Outputing asm to" <+> text filenm)
  let ncg_config = initNCGConfig dflags this_mod
  {-# SCC "OutputAsm" #-} doOutput filenm $
    \h -> {-# SCC "NativeCodeGen" #-}
      nativeCodeGen logger ncg_config location h ncg_uniqs cmm_stream

{-
************************************************************************
*                                                                      *
\subsection{LLVM}
*                                                                      *
************************************************************************
-}

outputLlvm :: Logger -> DynFlags -> FilePath -> Stream IO RawCmmGroup a -> IO a
outputLlvm logger dflags filenm cmm_stream = do
  lcg_config <- initLlvmCgConfig logger dflags
  {-# SCC "llvm_output" #-} doOutput filenm $
    \f -> {-# SCC "llvm_CodeGen" #-}
      llvmCodeGen logger lcg_config f cmm_stream

doOutput :: String -> (Handle -> IO a) -> IO a
doOutput filenm io_action = bracket (openFile filenm WriteMode) hClose io_action


module GHC.Driver.Backend.Rep
  ( Backend(..) -- only to be imported by Backend and Backend.Utils
  )
where

import Prelude

-- | Code generation backends.
--
-- GHC supports several code generation backends serving different purposes
-- (producing machine code, producing ByteCode for the interpreter) and
-- supporting different platforms.
--
data Backend
   = NCG           -- ^ Native code generator backend.
                   --
                   -- Compiles Cmm code into textual assembler, then relies on
                   -- an external assembler toolchain to produce machine code.
                   --
                   -- Only supports a few platforms (X86, PowerPC, SPARC).
                   --
                   -- See "GHC.CmmToAsm".


   | LLVM          -- ^ LLVM backend.
                   --
                   -- Compiles Cmm code into LLVM textual IR, then relies on
                   -- LLVM toolchain to produce machine code.
                   --
                   -- It relies on LLVM support for the calling convention used
                   -- by the NCG backend to produce code objects ABI compatible
                   -- with it (see "cc 10" or "ghccc" calling convention in
                   -- https://llvm.org/docs/LangRef.html#calling-conventions).
                   --
                   -- Support a few platforms (X86, AArch64, s390x, ARM).
                   --
                   -- See "GHC.CmmToLlvm"


   | ViaC          -- ^ Via-C backend.
                   --
                   -- Compiles Cmm code into C code, then relies on a C compiler
                   -- to produce machine code.
                   --
                   -- It produces code objects that are *not* ABI compatible
                   -- with those produced by NCG and LLVM backends.
                   --
                   -- Produced code is expected to be less efficient than the
                   -- one produced by NCG and LLVM backends because STG
                   -- registers are not pinned into real registers.  On the
                   -- other hand, it supports more target platforms (those
                   -- having a valid C toolchain).
                   --
                   -- See "GHC.CmmToC"


   | Interpreter   -- ^ ByteCode interpreter.
                   --
                   -- Produce ByteCode objects (BCO, see "GHC.ByteCode") that
                   -- can be interpreted. It is used by GHCi.
                   --
                   -- Currently some extensions are not supported
                   -- (foreign primops).
                   --
                   -- See "GHC.StgToByteCode"


   | NoBackend     -- ^ No code generated.
                   --
                   -- Use this to disable code generation. It is particularly
                   -- useful when GHC is used as a library for other purpose
                   -- than generating code (e.g. to generate documentation with
                   -- Haddock) or when the user requested it (via -fno-code) for
                   -- some reason.
  deriving (Show)

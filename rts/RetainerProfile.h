/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001
 * Author: Sungwoo Park
 *
 * Retainer profiling interface.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(PROFILING)

#include "RetainerSet.h"
#include "TraverseHeap.h"

#include "BeginPrivate.h"

void initRetainerProfiling ( void );
void endRetainerProfiling  ( void );
void retainerProfile       ( void );

RetainerSet* retainerSetOf( const StgClosure *c );

// Used by GC.c
W_ retainerStackBlocks(void);

#include "EndPrivate.h"

#endif /* PROFILING */

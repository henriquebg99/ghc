/* -----------------------------------------------------------------------------
 * $Id: HsCore.h,v 1.3 2001/09/13 11:40:28 simonmar Exp $
 *
 * Definitions for package `core' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSCORE_H
#define HSCORE_H

#include "config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_TERMIOS_H
#include <termios.h>
#endif
#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_DIRENT_H
#include <dirent.h>
#endif
#ifdef HAVE_UTIME_H
#include <utime.h>
#endif
#if defined(HAVE_GETTIMEOFDAY)
#  ifdef HAVE_SYS_TIME_H
#   include <sys/time.h>
#  endif
#elif defined(HAVE_GETCLOCK)
# ifdef HAVE_SYS_TIMERS_H
#  define POSIX_4D9 1
#  include <sys/timers.h>
# endif
#endif
#if defined(HAVE_TIME_H)
# include <time.h>
#endif
#ifdef HAVE_SYS_TIMEB_H
#include <sys/timeb.h>
#endif
#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif

#if !defined(mingw32_TARGET_OS) && !defined(irix_TARGET_OS)
# if defined(HAVE_SYS_RESOURCE_H)
#  include <sys/resource.h>
# endif
#endif

#ifdef hpux_TARGET_OS
#include <sys/syscall.h>
#define getrusage(a, b)  syscall(SYS_GETRUSAGE, a, b)
#define HAVE_GETRUSAGE
#endif

/* For System */
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

extern inline int s_isreg_wrap(m)  { return S_ISREG(m);  }
extern inline int s_isdir_wrap(m)  { return S_ISDIR(m);  }
extern inline int s_isfifo_wrap(m) { return S_ISFIFO(m); }
extern inline int s_isblk_wrap(m)  { return S_ISBLK(m);  }
extern inline int s_ischr_wrap(m)  { return S_ISCHR(m);  }
#ifdef S_ISSOCK
extern inline int s_issock_wrap(m) { return S_ISSOCK(m); }
#endif

#include "lockFile.h"

#include "HsFFI.h"

/* in ghc_errno.c */
int *ghcErrno(void);

/* in system.c */
HsInt systemCmd(HsAddr cmd);

/* in inputReady.c */
int inputReady(int fd, int msecs);

#endif

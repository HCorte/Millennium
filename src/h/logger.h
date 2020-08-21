/*  */

/*============================================================================*/
/*                                                                            */
/* This item is the property of GTECH Corporation, West Greewich, Rhode       */
/* Island, and contains confidential and trade secret information. It may     */
/* not be transferred from the custody or control of GTECH except as          */
/* authorized in writing by an officer of GTECH. Neither this item not the    */
/* information it contains may be used, transferred, reproduced, published    */
/* or disclosed, in whole or in part, and directly or indirectly, except      */
/* as expressly authorized by an officer of GTECH, pursuant to written        */
/* agreement.                                                                 */
/*                                                                            */
/* Any and all modifications to this item must have the prior written         */
/* authorization of GTECH's Enterprise Series Platform Team.  GTECH shall     */
/* not be liable in any way for any direct or indirect damages,  whatsoever,  */
/* as a result of any unauthorized modifications.  The Enterprise Series      */
/* Platform Team reserves the right to refuse support as a result of          */
/* unauthorized modification.                                                 */
/*                                                                            */
/* Copyright 2003 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[LOGGER.H]==============================================================*/
/*                                                                            */
/* Purpose: This header file contains GTX Log Management - Definitions        */
/*                                                                            */
/*====[LOGGER.H]==============================================================*/
/*                                                                            */

#ifndef LOGGER_H
#define LOGGER_H

#include "glogdefs.h"

#ifdef GLOBAL
#undef GLOBAL
#endif

#ifndef ALLOCATE
#define GLOBAL extern
#else
#define GLOBAL
#endif

#define TCMD_LOG  TCMD_LOGGER_BASE  /* Log message to logger               *
                                     *    +0  System logger                *
                                     *    +1  Auxilliary logger            *
                                     *    +2  Error logger                 */


/* Function Declarations  */

extern int  log_init (void);
extern int  auxlog_init (void);
extern int  errlog_init (void);

extern void logger (unsigned char sev, const char *format, ...);

extern void log_start (void);
extern void log_stop (void);
extern void log_flush (void);
extern void log_dump (int logtype, char *fnamep);
extern int  log_scrdump (char *fnamep);

extern void vlogger (int logtype, 
               unsigned char sev, const char *format, void *argp);
extern struct mbuf *vlogmsg (int logtype, 
               unsigned char sev, const char *format, void *argp);

extern void hexdump (unsigned char sev, 
               char *str, void *p, int nbytes, int cont);


/* Internal Declarations  */

/* Original GTX code, removed when ported to the host

extern TDESCTYP   logger_tdesc;

*/

#endif  /* LOGGER_H  */

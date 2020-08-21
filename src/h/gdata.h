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
/*====[GDATA.H]===============================================================*/
/*                                                                            */
/* Purpose: This header file contains Global Data - Definitions               */
/*                                                                            */
/*====[GDATA.H]===============================================================*/
/*                                                                            */

#ifndef GDATA_H
#define GDATA_H

#ifdef  GLOBAL
#undef  GLOBAL
#endif

#ifndef ALLOCATE
#define GLOBAL extern
#else
#define GLOBAL
#endif

/* Original GTX code, removed when ported to the host

GLOBAL
struct
t_time
         init_time;                     Time at system initialisation    

GLOBAL
struct
t_date
         init_date;                     Date at system initialisation   
*/

/* GTX States  */
                                       
enum {   GTXINIT     =  1,             /* System Initialisation            */
         GTXSTART    =  2,             /* pSOS Initialised                 */
         GTXUP       =  3,             /* Up & Running                     */
         GTXSHUT     =  4,             /* Shutting Down                    */
         GTXDOWN     =  5              /* Down                             */
};

GLOBAL
int      gtx_state;


/* Debug/Trace Flags  */
                                       
enum {   DBGINIT     =  0x00000001,    /* Initialisation                   */
         DBGSYS      =  0x00000002,    /* System                           */
         DBGKBD      =  0x00000004,    /* Keyboard Driver                  */
         DBGWDOG     =  0x00000008,    /* Watchdog Timer                   */
         DBGBUF      =  0x00000010,    /* Buffer Sanity Checking           */
         DBGBANAL    =  0x00000020,    /* Exhaustive Buffer Checking       */
         DBGBERRS    =  0x00000040,    /* Log buffer allocation errors     */
         DBGNBOOT    =  0x00010000,    /* No reboot on fatal error         */
         DBGCDUMP    =  0x00020000,    /* Dump log to disk on fatal error  */

         DBGTEST     =  0x00100000,    /* Test configuration               */
         DBGDEBUG    =  0x00200000     /* Debug configuration              */
};

#define GDEBUG(bit)  (gtx_debug & (bit))

GLOBAL
int      gtx_debug;


/* Site Options  */
                                       
enum {   SITEGBN     = 0x00000001,     /* UK - National Lottery            */
         SITETRM     = 0x00000002      /* Turkey - Milli Piyango           */
};

#define  GSITEOPT(bit)  (gtx_siteopts & (bit))

GLOBAL
unsigned long
#ifndef ALLOCATE
         gtx_siteopts;
#else
         gtx_siteopts = 0x00000000;
#endif


#define GSYSBOOTNAME "PSOSBOOT.SYS"    /* System bootstrap file name       */

GLOBAL
char *
#ifndef ALLOCATE
         cfgfile;                      /* Configuration file name          */
#else
         cfgfile     = "GTX.CFG";
#endif


GLOBAL
char 
#ifndef ALLOCATE
         cfg_desc [78 + 1];            /* Configuration description        */
#else
         cfg_desc [78 + 1] = { 0 };
#endif


/* gtxuver.c  */
extern int  gtx_version, gtx_release;
extern char gtx_prerel,  gtx_alpha;
extern char *gtx_beta;

extern char *gtx_lstblt, *gtx_bld_date, *gtx_bld_time;

GLOBAL
char
         gtx_revision [80];            /* Product version                  */

#endif  /* GDATA_H  */

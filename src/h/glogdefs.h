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
/*====[GLOGDEFS.H]============================================================*/
/*                                                                            */
/* Purpose: This header file contains GTX Log Management - External           */
/*          Definitions                                                       */
/*                                                                            */
/*====[GLOGDEFS.H]============================================================*/
/*                                                                            */

#ifndef GLOGDEFS_H
#define GLOGDEFS_H


/* Event Type  */

enum LOG_TYPE {
   LOGT_SYS       =  0,             /* System logger                       */
   LOGT_AUX       =  1,             /* Auxilliary logger                   */
   LOGT_ERR       =  2,             /* Error logger                        */
   LOGT_MAX       =  LOGT_ERR
};


/* Message Severity  */

enum LOG_SEVERITY {
   LOGDEBUG       =  10,            /* Debug messages                      */
   LOGTRACE       =  20,            /* Trace events                        */
   LOGINFO        =  25,            /* Informational messages              */
   LOGINFOLO      =  26,            /* Informational messages - highlight  */
   LOGINFOHI      =  27,            /* Informational messages - highlight  */
   LOGERR_INFO    =  40,            /* Error supplemental information      */
   LOGERR         =  50,            /* Error messages                      */
   LOGSEVERE      =  90,            /* Severe errors                       */
   LOGFATAL       =  100            /* Fatal errors                        */
};

/* If set in severity, log top auxilliary log  */
#define LOGAUX       0x80

#endif  /* GLOGDEFS_H  */

static const char *fileid = "";

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
/* Copyright 2007 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[MBUF_STUBS.C]==========================================================*/
/*                                                                            */
/* Purpose: These functions support the MBUF implementation.                  */
/*                                                                            */
/* Functions:                                                                 */
/*          disable_int()                                                     */
/*          hexdump                                                           */
/*          tm_wkafter                                                        */
/*          FATAL                                                             */
/*          UFATAL                                                            */
/*          log_start                                                         */
/*          log_stop                                                          */
/*          logger                                                            */
/*                                                                            */
/*====[MBUF_STUBS.C]==========================================================*/
/*                                                                            */

#include <stdlib.h>
#include <stdio.h>

#include "mbuf_includes.h"

#if defined(XOS_VMS)

#   include <ssdef.h>

    int SYS$SETAST();

#endif

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/*                                                                            */
/* disable_int()                                                              */
/*                                                                            */
/* Purpose: This function disables ASTs.  It provides compatibility with      */
/*          the MBUF routine disable_int, which disables interrupts on        */
/*          the GTX.                                                          */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              1 Normal status from SYS$SETAST system service   */
/*                           0 Error status from SYS$SETAST system service    */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int disable_int (void)
{

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

       return (1);

#   elif defined(XOS_VMS)

        int status;
        unsigned char disable;

        disable = 0;

        status = SYS$SETAST(disable);

        if (status == SS$_NORMAL) {
            return (1);
        }
        else {
            return (0);
        }

#   else

#       error - OS-specific logic not handled.

#   endif

}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Description: Dummy subroutines to resolve linker refererences              */
/*                                                                            */
/* Functions:   hexdump                                                       */
/*              tm_wkafter                                                    */
/*              FATAL                                                         */
/*              UFATAL                                                        */
/*              log_start                                                     */
/*              log_stop                                                      */
/*              logger                                                        */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void hexdump (unsigned char sev,         
               char *str, void *p, int nbytes, int cont)
 {
 return;
 } /* end Tni_Fatal */

void tm_wkafter( int value)              
 {
 return;
 } /* end tm_wkafter */

void FATAL( int value)                   
 {
 printf(" Fatal error detected /n");
 exit(value);
 } /* end Fatal */

void UFATAL( int value)                  
 {
 printf(" Fatal error detected /n");
 exit(value);
 } /* end UFatal */

void log_start()                         
 {
 return;
 } /* end log_start */

void log_stop()                          
 {
 return;
 } /* end log_stop */


void logger (int logtype,                
               unsigned char sev, const char *format, void *argp)
 {
 return;
 } /* end logger */

int restore_int (void)
{

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

        return (1);

#   elif defined(XOS_VMS)

        int status;
        unsigned char enable;

        enable = 1;

        status = SYS$SETAST(enable);

        if (status == SS$_NORMAL) {
            return (1);
        }
        else {
            return (0);
        }

#   else

#       error - OS-specific logic not handled.

#   endif

}

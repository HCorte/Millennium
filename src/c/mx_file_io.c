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
/* Copyright 2005 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[MX_FILE_IO.C]==========================================================*/
/*                                                                            */
/* Purpose: These functions open and close various MX files.                  */
/*                                                                            */
/* Functions:                                                                 */
/*          Open_Dbg_file()                                                   */
/*          Close_Dbg_file()                                                  */
/*          Open_Log_file()                                                   */
/*          Close_Log_file()                                                  */
/*                                                                            */
/*====[MX_FILE_IO.C]==========================================================*/
/*                                                                            */

#include <errno.h>
#include <limits.h>
#include <stdio.h>   
#include <string.h>   

#include "includes.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Open_Dbg_file()                                                            */
/*                                                                            */
/* Purpose: This function computes the difference between the two input       */
/*                                                                            */
/* Input Arguments: Two timeb structures                                      */
/*                                                                            */
/* Output Arguments: timeb structure where the difference in time is stored   */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/* Assumptions:                                                               */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Open_Dbg_file() {

    struct  timeval tval;
    struct  timeb   tbstr1;
    char    *tptr;
    char    month[3],day[3];
    int     year,date,hour,min,sec;
    char    colon;
    char    *dbg_file_name_p;
    char    dbg_file_name[256];
    char    strbuf[256];

    err_string = null_err_string;

    ftime(&tbstr1);

    tptr = ctime(&tbstr1.time);

    sscanf(tptr,"%3s %3s %2d %2d %c %2d %c %2d %4d",
           day,month,&date,&hour, &colon,&min, &colon, &sec, &year);

    dbg_file_name_p = dbg_file_name;

#   if defined(PROSYS_ENV_ALL)

#       if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

            sprintf(dbg_file_name,"$MXSRV/bin/mx_dbg-%d-%c%c%c-%d-%d-%d.dat",
	                        year,month[0],month[1],month[2],date,hour,min);

#       elif defined(XOS_VMS)

            sprintf(dbg_file_name,"mxsrvrun:mx_dbg-%d-%c%c%c-%d-%d-%d.dat",
                    year,month[0],month[1],month[2],date,hour,min);

#       else

#           error - OS-specific logic not handled.

#       endif

#       if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

            g_translate(dbg_file_name_p, strbuf);
            dbg_file_name_p = strbuf;

#       endif

#   endif

#   if defined(GOLS_ENV_ALL)

        sprintf(dbg_file_name,"gxtsk:mx_dbg-%d-%c%c%c-%d-%d-%d.dat",
                year,month[0],month[1],month[2],date,hour,min);

#   endif

    tnicon_p->dbg_p = fopen(dbg_file_name_p, "w");

    if (tnicon_p->dbg_p == NULL) {

        if (errno == USHRT_MAX) {

            sprintf(err_string.par1,"Opening");
        } else {

            sprintf(err_string.par1,"%s",getErrorMsg(errno));
        }

        sprintf(err_string.par2,"%s",dbg_file_name);

        output_err("Open_Dbg_file",
                   ME_FILE_ERROR,
                   MX_ERR_LVL_ERROR,
                   err_string);

        tnicon_p->dbg_state = DBG_FILE_ERR;

    } else {

        tnicon_p->dbg_state = DBG_ACTIVE;
    }
} 

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Close_Dbg_file()                                                           */
/*                                                                            */
/* Purpose: This function computes the difference between the two input       */
/*                                                                            */
/* Input Arguments: Two timeb structures                                      */
/*                                                                            */
/* Output Arguments: timeb structure where the difference in time is stored   */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/* Assumptions:                                                               */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Close_Dbg_file()
{

    fclose(tnicon_p->dbg_p);

    tnicon_p->dbg_state = DBG_OFF;
} 

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Open_Log_file()                                                            */
/*                                                                            */
/* Purpose: This function                                                     */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Open_Log_file() {

    char   *log_file_name_p;
    char    strbuf[256];

    log_file_name_p = tnicon_p->log_file_name;

#   if defined(PROSYS_ENV_V8)

        g_translate(log_file_name_p, strbuf);
        log_file_name_p = strbuf;

#   endif

    tnicon_p->log_p = fopen(log_file_name_p, "a+");

    if (tnicon_p->log_p == NULL) {

        if (errno == USHRT_MAX) {

            sprintf(err_string.par1,"Opening");
        } else {
        
            sprintf(err_string.par1,"%s",getErrorMsg(errno));
        }

        sprintf(err_string.par2,"[%s]",log_file_name_p);

        output_err("Open_Log_file",
                   ME_FILE_ERROR,
                   MX_ERR_LVL_ERROR,
                   err_string);

        tnicon_p->log_state = LOG_FILE_ERR;

    } else {

        tnicon_p->log_state = LOG_ACTIVE;
        tnicon_p->amount_logged = 0;
        tnicon_p->excess_log_check_time = time(NULL);
    }
} 

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Close_Log_file()                                                           */
/*                                                                            */
/* Purpose: This function                                                     */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Close_Log_file() {

    fclose(tnicon_p->log_p);

    tnicon_p->log_state = LOG_OFF;
    tnicon_p->amount_logged = 0;
} 

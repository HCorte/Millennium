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
/* Copyright 2003 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[OUTPUT_ERR.C]==========================================================*/
/*                                                                            */
/* FUNCTION NAME AND CALLING PARAMETERS GO HERE                               */
/*                                                                            */
/* Purpose:                                                                   */
/*          output errors and information messges to the log file             */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/* Output Arguments:                                                          */
/*                                                                            */
/* Return Value:                                                              */
/*                                                                            */
/* Assumptions:                                                               */
/*                                                                            */
/*====[OUTPUT_ERR.C]==========================================================*/
/*                                                                            */

#include <stdio.h>
#include <string.h>

#include "includes.h"

#if defined(GOLS_ENV_ALL)

#   if defined(XOS_VMS)

#        include <descrip.h>

#   endif

#endif

void  output_err(char *func_name,
                 int error_code,
                 int error_level,
                 struct ERR_STRING err_string)
{
    int status;
    char error_string[116];
    char error_msg[132];

#   if defined(GOLS_ENV_ALL)

#       if defined(XOS_VMS)

            struct dsc$descriptor_s gols_err_msg;

#       endif

#   endif

#   if defined(PROSYS_ENV_ALL)

        G_ERROR error;

#   endif

    sprintf(error_msg,
            "%132c",
            ' ');

    sprintf(error_string,
            TNI_ERROR_CODES[error_code].err_fmt,
            err_string.par1,
            err_string.par2,
            err_string.par3,
            err_string.par4,
            err_string.par5);


#   if defined(GOLS_ENV_ALL)

#       if defined(XOS_VMS)

            sprintf(error_msg,
                    "MXSRV %s",
                    error_string);

            gols_err_msg.dsc$b_dtype = DSC$K_DTYPE_T;
            gols_err_msg.dsc$b_class = DSC$K_CLASS_S;
            gols_err_msg.dsc$w_length = sizeof(error_msg);
            gols_err_msg.dsc$a_pointer = error_msg;

            MLOG (&gols_err_msg, &status);
#       endif

#   endif

#   if defined(PROSYS_ENV_ALL)

        g_setgtmserr(GE_STRING,&error);

        error.errlvl = (byte_4) error_level;

        g_adderrstr(error_string,&error);
        g_senderror(&error);

#   endif    

    LogMsg(error_string);

}


char* getErrorMsg(int errnum) {

   return errnum > -1 ? strerror(errnum) : "unknown";
}

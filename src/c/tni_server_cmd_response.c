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
/*====[TNI_SERVER_CMD_RESPONSE.C]=============================================*/
/*                                                                            */
/* Tni_Server_Cmd_Response (struct PDU_STRUCT *pdu_struct,                    */
/*                          long int pdulen,                                  */
/*                          unsigned short conn)                              */
/*                                                                            */
/* Purpose: This function is a stub to handle a Server Command Response PDU.  */
/*          Currently, the MX task takes no action upon receiving this PDU.   */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Server Command    */
/*                           Response PDU.  Currently, this PDU only consists */
/*                           of a fixed header part.                          */
/*                                                                            */
/*          pdulen           PDU length                                       */
/*                                                                            */
/*          conn             Connection number                                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: The PDU Fixed Header has been validated by the calling        */
/*              function prior to entering this function.                     */
/*                                                                            */
/*====[TNI_SERVER_CMD_RESPONSE.C]=============================================*/
/*                                                                            */

#include <stdio.h>

#include "includes.h"

void
Server_Cmd_Response (struct PDU_STRUCT *pdu_struct,
                     long int pdulen, 
                     unsigned short conn)
{
    int offset;                            /* offset to each parameter        */
                                           /* code/length/value block         */
                                           /* contained in the var part of the*/
                                           /* PDU                             */
    int rc = P_SUCCESS;                    /* generic return code             */

    long int length;                       /* number of bytes in a single     */
                                           /* parameter unit, including code, */
                                           /* length, value fields            */
    long int value;                        /* generic parameter value returned*/
                                           /* by Check_Parameter              */
    long int result_code;
    long int cmd_code;

    int parm_good;                         /* status of the check_paramter    */

    unsigned char param_code;              /* PDU parameter code              */

    struct TNI_DATA *param_data;           /* pointer to a parameter          */
                                           /* code/length/value block         */

    char string_val[MAX_RPC_RQST_TAG_LEN + 1];

    err_string = null_err_string;

    Print_Pdu ("server command response",pdu_struct,pdulen);
   
    tnicon_p->srv_cmd_resp_cnt_tot[conn]++;

    result_code = -1;
    cmd_code = -1;

/* Assume that first byte following fixed header is a parameter block         */

    for( offset = 0; offset < (pdulen - sizeof (struct TNI_FIXED_HDR));
       offset+= length ) {

        param_data = (struct TNI_DATA *)&pdu_struct->msg_data[offset];
        parm_good = Check_Parameter (conn,
                                     param_data,
                                     &param_code,
                                     &length,
                                     &value,
                                     string_val);

        if( !parm_good ) {

            LogPdu(pdu_struct, -1, conn);

            rc = send_invalid_pdu_error (conn);

            return;
        }

/*    length returned by check_parameter is only the parameter value length   */
/*    i.e. it excludes the size of the parameter code and parameter length    */
/*    fields                                                                  */

        length += sizeof(param_data->param_code) +
                  sizeof(param_data->param_length);

        switch( param_code ) {

        case COMMAND_CODE:
            cmd_code = (unsigned int)value;
            break;

        case RESULT_CODE:
            result_code = value;
            break;

        default:

            LogPdu(pdu_struct, -1, conn);

            rc = send_invalid_pdu_error (conn);

            break;

        }                                          /* end switch (param_code) */
    }                                              /* end for                 */

    if (result_code != SUCCESS) {

        switch (cmd_code)
        {
        case CLOSE_CONNECTION:

            sprintf(err_string.par2,"CLOSE");
            break;

        default:

            sprintf(err_string.par2,"Unkown");
            break;
        }

        sprintf(err_string.par1,"%d",conn);

        output_err("Server_Cmd_Response",
                   MI_TNI_CMD_NACK,
                   MX_ERR_LVL_ERROR,
                   err_string);
    }
    return;
}

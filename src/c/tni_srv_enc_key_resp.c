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
/* Copyright 2010 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[TNI_SRV_ENC_KEY_RESP.C]================================================*/
/*                                                                            */
/* Srv_Encryption_Key_Resp (struct PDU_STRUCT *pdu_struct,                    */
/*                          long int pdulen,                                  */
/*                          unsigned short conn)                              */
/*                                                                            */
/* Purpose: This function parses the Variable Data Part of a Server           */
/*          Encryption Key Response PDU contained in pdu_struct, validates    */
/*          the parameters, decrypts the client's encryption and stores is    */
/*          in global memeory.                                                */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Server Encryption */
/*                           Key Response PDU, i.e. PDU fixed header part     */
/*                           followed by variable data part.                  */
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
/*====[TNI_SRV_ENC_KEY_RESP.C]================================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

void
Srv_Encryption_Key_Resp (struct PDU_STRUCT *pdu_struct,
                         long int pdulen,
                         unsigned short conn)
{
    char string_val[1] = {'\0'};

    int offset;                            /* offset to each parameter        */
                                           /* code/length/value block         */
                                           /* contained in the var part of the*/
    int parm_good;                         /* status of the check_paramter    */
    int rc = P_SUCCESS;                    /* generic return code             */
    int invalid_pdu = P_FALSE;

    long int length;                       /* number of bytes in a single     */
                                           /* parameter unit, including code, */
                                           /* length, value fields            */

    long int value;                        /* generic parameter value returned*/
                                           /* by Check_Parameter              */
    long int enc_key_x_method = -1;
    long int enc_key_type = -1;
    long int enc_key_length = -1;
    long int result_code = -1;

    unsigned char *enc_key_ptr = NULL;
    unsigned char param_code;              /* PDU parameter code              */

    struct TNI_DATA *param_data;           /* pointer to a parameter          */
                                           /* code/length/value block         */

    err_string = null_err_string;
    sprintf(err_string.par1, "%d", conn);

    Print_Pdu ("Server Encryption Key Response",pdu_struct,pdulen);

    tnicon_p->srv_enc_key_resp_cnt_tot[conn]++;

/*  Read parameters if they are present */

    if (pdulen > sizeof (struct TNI_FIXED_HDR))
    {
        for (offset = 0;
             (offset < (pdulen - sizeof (struct TNI_FIXED_HDR))) &&
             (rc == P_SUCCESS);
             offset+= length )
        {
            param_data = (struct TNI_DATA *)&pdu_struct->msg_data[offset];

            parm_good = Check_Parameter (conn,
                                         param_data, 
                                         &param_code, 
                                         &length, 
                                         &value,
                                         string_val);

            if( !parm_good )
            {
                sprintf(err_string.par2, "invalid parameter");
                invalid_pdu = P_TRUE;
                rc = P_FAILURE;
            }

/* length returned by check_parameter is only the parameter value length      */
/* i.e. it excludes the size of the parameter code and parameter length       */
/* fields                                                                     */

            length += sizeof(param_data->param_code) +
                      sizeof(param_data->param_length);

            switch (param_code)
            {
            case ENC_KEY_X_METHOD:
                enc_key_x_method = value;
                if (enc_key_x_method != 
                    tnicon_p->connection[conn].enc_key_x_method)
                {
                    sprintf(err_string.par2, "key exchange method mismatch");
                    rc = P_FAILURE;
                }
                break;

            case ENC_KEY_TYPE:
                enc_key_type = value;
                if (enc_key_type != 
                    tnicon_p->connection[conn].enc_key_type)
                {
                    sprintf(err_string.par2, "encryption key type mismatch");
                    rc = P_FAILURE;
                }
                break;

            case ENC_KEY_LENGTH:
                enc_key_length = value;
                if (enc_key_length > 0)
                {
                    enc_key_ptr = (unsigned char *) (&pdu_struct->msg_data[offset] + length);
                }
                else
                {
                    sprintf(err_string.par2, "invalid key length");
                    invalid_pdu = P_TRUE;
                    rc = P_FAILURE;
                }
                length += enc_key_length;
                break;

            case RESULT_CODE:
                result_code = value;
                if (result_code != SUCCESS)
                {
                    sprintf(err_string.par2, "result code (%ld)", result_code);
                    rc = P_FAILURE;
                }
                break;

            default:
                sprintf(err_string.par2, "invalid parameter");
                invalid_pdu = P_TRUE;
                rc = P_FAILURE;
                break;
            }                                      /* end switch (param_code) */
        }                                          /* end for                 */

        /* Verify all required parameters are present */

        if (rc == P_SUCCESS)
        {
            if (result_code == -1)
            {
                sprintf(err_string.par2, "missing parameter");
                invalid_pdu = P_TRUE;
                rc = P_FAILURE;
            }
            else if (result_code == SUCCESS)
            {
                if ((enc_key_x_method == -1) ||
                    (enc_key_type == -1) ||
                    (enc_key_length == -1))
                {
                    sprintf(err_string.par2, "missing parameter");
                    invalid_pdu = P_TRUE;
                    rc = P_FAILURE;
                }
                else
                {
                    rc = Decrypt_Client_Key (conn, enc_key_length, enc_key_ptr);
                    if (rc == P_FAILURE)
                    {
                        sprintf(err_string.par2, "key decryption failure");
                    }
                }
            }
            else if (result_code > SUCCESS)
            {
                sprintf(err_string.par2, "result code (%ld)", result_code);
                rc = P_FAILURE;
            }
        }
    }
    else
    {
        sprintf(err_string.par2, "no parameters");
        invalid_pdu = P_TRUE;
        rc = P_FAILURE;
    }

    if (rc == P_FAILURE)
    {
        output_err ("Srv_Encryption_Key_Resp",
                    MI_TNI_ENC_KEY_ERR,
                    MX_ERR_LVL_ERROR,
                    err_string);
    }

    if (invalid_pdu == P_TRUE)
    {
        LogPdu(pdu_struct, -1, conn);
        rc = send_invalid_pdu_error (conn);
    }
}

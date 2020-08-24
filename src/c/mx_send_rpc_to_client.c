static const char *fileid = "";

/*
 * =================[send_Rpc_to_client.c]==================================== 
 *
 * Description:
 *
 * Function  to send rpc to client.
 *
 * Functions:
 *
 * send_Rpc_to_client      - Finds a client encode pdu and  determine 
 *                           the time to send server data PDU. 
 *
 * -----------------------------------------------------------------------
 * This item is the property of GTECH Corporation, Providence,
 * Rhode Island, and contains confidential and trade secret information.
 * It may not be transfered from the custody or control of GTECH except
 * as authorized in writing by an officer of GTECH.  Neither this item
 * nor the information it contains may be used, transfered, reproduced,
 * published, or disclosed, in whole or in part, and directly or
 * indirectly, except as expressly authorized by an officer of GTECH,
 * pursuant to written agreement.
 *
 * Any and all modification to this item must have the prior written 
 * authorization of GTECH's Enterprise Series Platform Team.  GTECH shall
 * not be liable in any way for any direct or indirect damages,  whatsoever,
 * as a result of any unauthorized modifications.  The Enterprise Series
 * Platform Team reserves the right to refuse support as a result of
 * unauthorized modification.
 *
 * Copyright (c) 2005 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * =======================================================================
 */

#include "includes_mbuf.h"

/* [Send_Rpc_To_Client]
 *
 * Summary:
 *
 * Send_Rpc_To_Client (int addr_num,
 *                     char *rpc_rqst_tag,
 *                     long int del_mode,
 *                     int enc_mode,
 *                     long int msg_len,
 *                     unsigned char *msg,
 *                     int *result_code,
 *                     char *result_text)
 *
 * addr_num     - address to send the message
 * rpc_rqst_tag - RPC request tag
 * del_mode     - delivery mode
 * enc_mode     - encryption mode
 * msg_len      - RPC message length
 * msg          - pointer to RPC message data
 * result_code  - pointer to the result code
 * result_text  - pointer to the error String
 *
 * Description:
 *
 * This function validates the delivery mode.  Finds a client to send the
 * RPC to, based on the application name.  Encodes the RPC message in the
 * TNI protocol.  Determines if it is time to send the RPC Server Data PDU.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Send_Rpc_To_Client (int addr_num,
                    char *rpc_rqst_tag,
                    long int del_mode,
                    int enc_mode,
                    long int msg_len,
                    unsigned char *msg,
                    int *result_code,
                    char *result_text)
{
    byte_4              rc = P_SUCCESS;
    int                 enc_msg_flag = P_FALSE;
    short int           tot_msg_len = 0;   /* length of the input message,    */
                                           /* excluding TNI protocol header + */
                                           /* the TNI terminal message header */
                                           /* terminal message header         */
    int                 conn = 0;          /* connection number attempted for */
                                           /* write                           */
    unsigned char      *rpc_msg_ptr = NULL;

    struct mbuf        *p = NULL;          /* pointer to mbuf chain containing*/
                                           /* entire RPC Server Data PDU, only*/
                                           /* used if we're sending the PDU to*/
                                           /* the cleint from this function   */
    struct mbuf        *header_mbuf = NULL; /* pointer to mbuf holding RPC    */
                                           /* message header, i.e. parameters */
    struct mbuf        *msg_mbuf = NULL;   /* pointer to mbuf holding RPC     */
                                           /* message data, excluding TNI     */
                                           /* protocol header                 */
    struct TNI_PARAM_PAIR params[2];       /* only the message count variable */
                                           /* data parameter is used for      */
                                           /* RPC Server Data PDU             */
    char               del_mode_str[20];   /* Delivery mode string            */
    byte_4             local_msg_len;

 
    local_msg_len = msg_len;

    err_string = null_err_string;

    switch (del_mode)
    {

        case REQUEST:
            sprintf(del_mode_str,"Request");

            /* Find the appropriate connection to send RPC message to */
            conn = Find_Conn_For_App (MSG_TYPE_ES_RPC, enc_mode, addr_num, NULL);

            if (conn == 0)
            {
                sprintf (result_text,
                         "No connection available to send RPC for application %s",
                         tnicon_p->app[addr_num].name);

                sprintf (err_string.par1, "%s", tnicon_p->app[addr_num].name);

                output_err ("Send_Rpc_To_Client",
                            MI_TNI_NOCONN_AVL,
                            MX_ERR_LVL_WARNING,
                            err_string);

                *result_code = MXSRV_RPC_ERR_NOCONN;
                rc = P_FAILURE;
            }
            else
            {
                switch (enc_mode)
                {
                case ENC_RQST:
                case ENC_RQST_RESP:
                    enc_msg_flag = P_TRUE;
                    break;
                }
            }

            break;

        case RESPONSE:
            sprintf(del_mode_str,"Response");

            conn = addr_num;

            /* An RPC response is supported only for TNI_VERSION_23  and above*/ 
            if (tnicon_p->connection[conn].tni_proto_ver < TNI_VERSION_23)
            {
                sprintf (result_text,
                         "Delivery of RPC response message not supported");

                sprintf (err_string.par1, "RPC response");

                output_err ("Send_Rpc_To_Client",
                            MI_TNI_NOSEND_SUPP,
                            MX_ERR_LVL_WARNING,
                            err_string);

                *result_code = MXSRV_RPC_ERR_MSGTYP_NOSUPP;
                rc = P_FAILURE;
            }
            break;

        case UNSOLICITED:

            sprintf (result_text,
                     "Delivery of RPC unsolicited message not supported");

            sprintf (err_string.par1, "RPC unsolicited");

            output_err ("Send_Rpc_To_Client",
                        MI_TNI_NOSEND_SUPP,
                        MX_ERR_LVL_WARNING,
                        err_string);

            *result_code = MXSRV_RPC_ERR_MSGTYP_NOSUPP;
            rc = P_FAILURE;

            break;

        case BROADCAST:

            sprintf (result_text,
                     "Delivery of RPC broadcast message not supported");

            sprintf (err_string.par1, "RPC broadcast");

            output_err ("Send_Rpc_To_Client",
                        MI_TNI_NOSEND_SUPP,
                        MX_ERR_LVL_WARNING,
                        err_string);

            *result_code = MXSRV_RPC_ERR_MSGTYP_NOSUPP;
            rc = P_FAILURE;

            break;
    }

/* Encrypt RPC message data if required */

    if (rc == P_SUCCESS)
    {
        rpc_msg_ptr = (unsigned char *)msg;
        msg_mbuf = Encrypt_Data(conn, del_mode, enc_mode, &local_msg_len,
                                rpc_msg_ptr, &rc);

        if (rc != P_SUCCESS)
        {
            *result_code = rc;
            sprintf (result_text, "Error encrypting %s message", del_mode_str);
            rc = P_FAILURE;
        }
    }

/* Build the mbuf for the RPC message header */

    if (rc == P_SUCCESS)
    {
        if ((header_mbuf = Build_Rpc_Msg_Header (conn,
                                                 rpc_rqst_tag,
                                                 local_msg_len,
                                                 del_mode,
                                                 enc_mode)) == NULL)
        {
            *result_code = MXSRV_RPC_ERR_NOBUILD;
            rc = P_FAILURE;
        }
    }

/* Append RPC message header mbuf and RPC message data mbuf */

    if (rc == P_SUCCESS)
    {
        mb_append (&header_mbuf, msg_mbuf);

        tot_msg_len = mb_len_p (header_mbuf);

        if (tot_msg_len == 0)
        {
            mb_free_p (header_mbuf);

            sprintf (result_text,
                     "Zero length data chain in Send_Rpc_To_Client");

            sprintf (err_string.par1, "Send_Rpc_To_Client");

            output_err ("Send_Rpc_To_Client",
                        MI_TNI_ZERO_CHAIN,
                        MX_ERR_LVL_ERROR,
                        err_string);

            *result_code = MXSRV_RPC_ERR_NOBUILD;
            rc = P_FAILURE;
        }
    }

    if (rc == P_SUCCESS)
    {
        if (Insert_Msg_In_Server_Data_Pdu (conn,
                                           tot_msg_len,
                                           del_mode,
                                           enc_msg_flag,
                                           header_mbuf) == P_FAILURE)
        {
            sprintf (result_text,
                     "RPC %s of %d bytes exceeds max PDU size of %d bytes",
                     del_mode_str,
                     tot_msg_len + Calc_Pdu_Hdr_Size(RPC_SERVER_DATA_PDU),
                     tnicon_p->connection[conn].max_pdu_size);

            sprintf (err_string.par1, "RPC %s", del_mode_str);
            sprintf (err_string.par2, 
                     "%d",
                     tot_msg_len + Calc_Pdu_Hdr_Size(RPC_SERVER_DATA_PDU));
            sprintf (err_string.par3, 
                     "%d",
                     tnicon_p->connection[conn].max_pdu_size);

            output_err ("Send_Rpc_To_Client",
                        MI_TNI_MAX_PDU_EXCD,
                        MX_ERR_LVL_ERROR,
                        err_string);

            *result_code = MXSRV_RPC_ERR_MSG_TOBIG;
            rc = P_FAILURE;
        }
    }

    return (rc);
}

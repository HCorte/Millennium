static const char *fileid = "";

/*
 * ===[tni_send_error_pdu.c]==============================================
 *
 * Description:
 *
 * Functions to send error PDUs.
 *
 * Functions:
 *
 * send_invalid_pdu_error
 * send_term_unknown_error
 * send_msg_undelivered_error
 * send_rpc_rqst_error_pdu
 * send_not_authenticated_error
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
 * Any and all modifications to this item must have the prior written
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

#include <stdio.h>

#include "includes_mbuf.h"

/* [send_invalid_pdu_error]
 *
 * Summary:
 *
 * send_invalid_pdu_error (int conn_num)
 *
 * conn_num - Connection number the error PDU is to be send over
 *
 * Description:
 *
 * This function builds and sends an error PDU indicating an invalid PDU has
 * been received by the MX Server.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
send_invalid_pdu_error (int conn_num)
{
    int                 act_conn;          /* Actual connection PDU was sent  */
    int                 rc = P_SUCCESS;

    struct TNI_PARAM_PAIR params[2];       /* sized by max number of PDU      */
                                           /* parameters we may send in a     */
                                           /* possible Error PDU              */

    struct mbuf        *mymbuf;            /* pointer to mbuf for possible    */
                                           /* PDU                             */

    params[0].param_code = RESULT_CODE;
    params[0].param_value = INVALID_PDU;
    params[1].param_code = 0;              /* NULL terminated list  */
    params[1].param_value = 0;

    mymbuf = Build_Pdu (conn_num, ERROR_PDU, NULL, &params[0]);

    act_conn = Send_To_Conn (mymbuf, conn_num, NO_ALT_CONN);

    if (act_conn == 0)
    {
        sprintf(err_string.par1, "ERROR");
        sprintf(err_string.par2, "%d", conn_num);

        output_err("send_invalid_pdu_error",
                   MI_TNI_ERR_SEND,
                   MX_ERR_LVL_ERROR,
                   err_string);

        rc = P_FAILURE;
    }
    else
    {
        tnicon_p->error_sent_cnt_tot[act_conn]++;
    }

    return(rc);
}

/* [send_term_unknown_error]
 *
 * Summary:
 *
 * send_term_unknown_error (int conn_num,
 *                          long int terminal_server_id,
 *                          long int terminal_client_id,
 *                          char *terminal_client_tag,
 *                          char *correlation_tag)
 *
 * conn_num            - Connection number the error PDU is to be send over
 * terminal_server_id  - Terminal server identifier 
 * terminal_client_id  - Terminal client identifier
 * terminal_client_tag - Terminal client tag
 * correlation_tag     - Correlation tag sent from MX Client
 *
 * Description:
 *
 * This function builds and sends an error PDU indicating a message with an 
 * unknown terminal has been received by the MX Server.  The error PDU will
 * be sent to any available connection to the sending MX Client.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
send_term_unknown_error (int conn_num,
                         long int terminal_server_id,
                         long int terminal_client_id,
                         char *terminal_client_tag,
                         char *correlation_tag)
{
    int                 act_conn;          /* Actual connection PDU was sent  */
    int                 rc = P_SUCCESS;

    struct TNI_PARAM_PAIR params[4];       /* sized by max number of PDU      */
                                           /* parameters we may send in a     */
                                           /* possible Error PDU              */

    struct mbuf        *mymbuf;            /* pointer to mbuf for possible    */
                                           /* PDU                             */

    params[0].param_code = RESULT_CODE;
    params[0].param_value = TERMINAL_UNKNOWN;

/*
 *  Put terminal identification is error PDU
 */
 
    switch (tnicon_p->connection[conn_num].term_id_method)
    {

    case METH_TERM_CLIENT_ID:

        params[1].param_code = TERM_CLIENT_ID;
        params[1].param_value = terminal_client_id;
        break;

    case METH_TERM_CLIENT_TAG:

        params[1].param_code = TERM_CLIENT_TAG;
        strcpy(params[1].param_value_char,terminal_client_tag);
        break;

    case METH_TERM_SERVER_ID:

        params[1].param_code = TERM_SERVER_ID;
        params[1].param_value = terminal_server_id;
        break;

    }

/*
 *  Put correlation id in error PDU if one was sent
 */

    if (correlation_tag != NULL)
    {
        if (correlation_tag[0] != '\0')
        {
            params[2].param_code = CORRELATION_TAG;
            strcpy(params[2].param_value_char, correlation_tag);

            params[3].param_code = 0;                /* NULL terminated list  */
            params[3].param_value = 0;
        }
        else
        {
            params[2].param_code = 0;                /* NULL terminated list  */
            params[2].param_value = 0;
        }
    }
    else
    {
        params[2].param_code = 0;                    /* NULL terminated list  */
        params[2].param_value = 0;
    }

    mymbuf = Build_Pdu (conn_num, ERROR_PDU, NULL, &params[0]);

    act_conn = Send_To_Conn (mymbuf, conn_num, ALT_CONN);

    if (act_conn == 0)
    {
        sprintf(err_string.par1, "ERROR");
        sprintf(err_string.par2, "%d", conn_num);

        output_err("send_term_unknown_error",
                   MI_TNI_ERR_SEND,
                   MX_ERR_LVL_ERROR,
                   err_string);

        rc = P_FAILURE;
    }
    else
    {
        tnicon_p->error_sent_cnt_tot[act_conn]++;
    }

    return(rc);
}

/* [send_msg_undelivered_error]
 *
 * Summary:
 *
 * send_msg_undelivered_error (int conn_num,
 *                             long int terminal_server_id,
 *                             long int terminal_client_id,
 *                             char *terminal_client_tag,
 *                             char *correlation_tag)
 *
 * conn_num            - Connection number the error PDU is to be send over
 * terminal_server_id  - Terminal server identifier
 * terminal_client_id  - Terminal client identifier
 * terminal_client_tag - Terminal client tag
 * correlation_tag     - Correlation tag sent from MX Client
 *
 * Description:
 *
 * This function builds and sends an error PDU indicating the MX Server was
 * unable to deliver the message sent by the MX Client.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
send_msg_undelivered_error (int conn_num,
                            long int terminal_server_id,
                            long int terminal_client_id,
                            char *terminal_client_tag,
                            char *correlation_tag)
{
    int                 act_conn;          /* Actual connection PDU was sent  */
    int                 rc = P_SUCCESS;

    struct TNI_PARAM_PAIR params[4];       /* sized by max number of PDU      */
                                           /* parameters we may send in a     */
                                           /* possible Error PDU              */

    struct mbuf        *mymbuf;            /* pointer to mbuf for possible    */
                                           /* PDU                             */

    params[0].param_code = RESULT_CODE;
    params[0].param_value = MSG_UNDELIVERABLE;

/*
 *  Put terminal identification is error PDU
 */

    switch (tnicon_p->connection[conn_num].term_id_method)
    {

    case METH_TERM_CLIENT_ID:

        params[1].param_code = TERM_CLIENT_ID;
        params[1].param_value = terminal_client_id;
        break;

    case METH_TERM_CLIENT_TAG:

        params[1].param_code = TERM_CLIENT_TAG;
        strcpy(params[1].param_value_char,terminal_client_tag);
        break;

    case METH_TERM_SERVER_ID:

        params[1].param_code = TERM_SERVER_ID;
        params[1].param_value = terminal_server_id;
        break;

    }

/*
 *  Put correlation id in error PDU if one was sent
 */

    if (correlation_tag != NULL)
    {
        if (correlation_tag[0] != '\0')
        {
            params[2].param_code = CORRELATION_TAG;
            strcpy(params[2].param_value_char, correlation_tag);

            params[3].param_code = 0;                /* NULL terminated list  */
            params[3].param_value = 0;
        }
        else
        {
            params[2].param_code = 0;                /* NULL terminated list  */
            params[2].param_value = 0;
        }
    }
    else
    {
        params[2].param_code = 0;                    /* NULL terminated list  */
        params[2].param_value = 0;
    }

    mymbuf = Build_Pdu (conn_num, ERROR_PDU, NULL, &params[0]);

    act_conn = Send_To_Conn (mymbuf, conn_num, ALT_CONN);

    if (act_conn == 0)
    {
        sprintf(err_string.par1, "ERROR");
        sprintf(err_string.par2, "%d", conn_num);

        output_err("send_msg_undelivered_error",
                   MI_TNI_ERR_SEND,
                   MX_ERR_LVL_ERROR,
                   err_string);

        rc = P_FAILURE;
    }
    else
    {
        tnicon_p->error_sent_cnt_tot[act_conn]++;
    }

    return(rc);
}

/* [send_rpc_rqst_error_pdu]
 *
 * Summary:
 *
 * send_rpc_rqst_error_pdu  (int conn_num,
 *                           char *rpc_rqst_tag,
 *                           int  result_code)
 *
 * conn_num            - Connection number the error PDU is to be send over
 * rpc_rqst_tag        - Tag sent from MX Client in RPC rqst
 * result_code         - Error result code  
 *
 * Description:
 *
 * This function builds and sends an error PDU indicating the MX Server was
 * unable to deliver the message sent by the MX Client.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
send_rpc_rqst_error_pdu (int  conn_num,
                         char *rpc_rqst_tag,
                         int  result_code)
{
    int              rc = P_SUCCESS;
    int              act_conn;          /* Actual connection PDU was sent  */
    struct TNI_PARAM_PAIR params[3];       /* sized by max number of PDU      */
                                           /* parameters we may send in a     */
                                           /* possible Error PDU              */
    struct mbuf        *mymbuf;            /* pointer to mbuf for possible    */
                                           /* PDU                             */

    params[0].param_code = RESULT_CODE;
    params[0].param_value = result_code;

/*
 *  Put the RPC request tag in error PDU if one was sent
 */

    if (rpc_rqst_tag != NULL)
    {
        if (rpc_rqst_tag[0] != '\0')
        {
            params[1].param_code = RPC_REQUEST_TAG;
            strcpy(params[1].param_value_char, rpc_rqst_tag);

            params[2].param_code = 0;                /* NULL terminated list  */
            params[2].param_value = 0;
        }
        else
        {
            params[1].param_code = 0;                /* NULL terminated list  */
            params[1].param_value = 0;
        }
    }
    else
    {
        params[1].param_code = 0;                    /* NULL terminated list  */
        params[1].param_value = 0;
    }

    mymbuf = Build_Pdu (conn_num, ERROR_PDU, NULL, &params[0]);

    act_conn = Send_To_Conn (mymbuf, conn_num, ALT_CONN);

    if (act_conn == 0)
    {
        sprintf(err_string.par1, "ERROR");
        sprintf(err_string.par2, "%d", conn_num);

        output_err("send_rpc_rqst_error_pdu",
                   MI_TNI_ERR_SEND,
                   MX_ERR_LVL_ERROR,
                   err_string);

        rc = P_FAILURE;
    }
    else
    {
        tnicon_p->error_sent_cnt_tot[act_conn]++;
    }

    return(rc);
}

/* [send_not_authenticated_error]
 *
 * Summary:
 *
 * send_not_authenticated_error (int conn_num)
 *
 * conn_num - Connection number the error PDU is to be send over
 *
 * Description:
 *
 * This function builds and sends an error PDU indicating 
 * the connection is not in authenticated state
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
send_not_authenticated_error (int conn_num)
{
    int                 act_conn;          /* Actual connection PDU was sent  */
    int                 rc = P_SUCCESS;

    struct TNI_PARAM_PAIR params[2];       /* sized by max number of PDU      */
                                           /* parameters we may send in a     */
                                           /* possible Error PDU              */

    struct mbuf        *mymbuf;            /* pointer to mbuf for possible    */
                                           /* PDU                             */

    params[0].param_code = RESULT_CODE;
    params[0].param_value = NOT_AUTHENTICATED;
    params[1].param_code = 0;              /* NULL terminated list  */
    params[1].param_value = 0;

    mymbuf = Build_Pdu (conn_num, ERROR_PDU, NULL, &params[0]);

    act_conn = Send_To_Conn (mymbuf, conn_num, NO_ALT_CONN);

    if (act_conn == 0)
    {
        sprintf(err_string.par1, "ERROR");
        sprintf(err_string.par2, "%d", conn_num);

        output_err("send_not_authenticated_error",
                   MI_TNI_ERR_SEND,
                   MX_ERR_LVL_ERROR,
                   err_string);

        rc = P_FAILURE;
    }
    else
    {
        tnicon_p->error_sent_cnt_tot[act_conn]++;
    }

    return(rc);
}


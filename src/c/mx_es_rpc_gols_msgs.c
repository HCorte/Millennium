static const char *fileid = "";

/*
 * ===[mx_es_rpc_gols_msgs.c]============================================= 
 *
 * Description:
 *
 * Functions to process the ES RPC alphaGOLS messages.
 *
 * Functions:
 *
 * Process_Es_Rpc_Rqst
 * Build_Es_Rpc_Prod_Reply
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

#include <string.h>

#include "includes.h"

/* [Process_Es_Rpc_Rqst]
 *
 * Summary:
 * 
 * Process_Es_Rpc_Rqst(int product_num, ES_RPC_RQST *rpc_rqst)
 *
 * product_num - product number from which the request was received
 * rpc_rqst    - pointer to the RPC request product-to-product message
 *
 * Description:
 *
 *
 *
 * Returns Values: None
 *
 */

void
Process_Es_Rpc_Rqst(int cdc,
                    int procom_buffer_num,
                    int reply_queue_num,
                    char *application_name,
                    int rpc_rqst_len,
                    unsigned char *rpc_rqst_msg)
{
    int                 rc = P_SUCCESS;             /* return code */
    int                 app_num = -1;
    int                 result_code = P_SUCCESS;
    int                 rqst_event_num = -1;
    char                rpc_request_tag[MAX_RPC_RQST_TAG_LEN + 1];
    char                result_text[132];
    time_t              raw_rqst_time;
    struct tm          *rqst_time_info;

    ES_RPC_RQST         rpc_rqst;

    RQST_EVENT         *event = NULL;

    err_string = null_err_string;
    memset(result_text, '\0', sizeof(result_text));

    rpc_rqst.rpcAckRequired = 1;

    rpc_rqst.bodyLength = (long) rpc_rqst_len;

    strncpy (rpc_rqst.applicationName,
             application_name,
             MXSRV_MAX_APP_NAME_LEN + 1);

    memcpy (rpc_rqst.body,
            rpc_rqst_msg,
            rpc_rqst_len);

    if (rpc_rqst.rpcAckRequired)
    {
        tnicon_p->tot_rpc_rqsts_ack++;
        tnicon_p->num_rpc_rqsts_ack++;

        app_num = Find_Application_Number (rpc_rqst.applicationName);

        if (app_num != -1)
        {
            time (&raw_rqst_time);
            rqst_time_info = localtime (&raw_rqst_time);

            rc = Allocate_Rpc_Rqst_Event (app_num,
                                          procom_buffer_num,
                                          cdc,
                                          rqst_time_info->tm_hour,
                                          rqst_time_info->tm_min,
                                          rqst_time_info->tm_sec,
                                          reply_queue_num,
                                          &rqst_event_num);

            if (rc == P_SUCCESS)
            {
                sprintf (rpc_request_tag, 
                         "%d-%d-%d-%d-%d:%d.%d",
                         rqst_event_num,
                         app_num,
                         procom_buffer_num,
                         cdc, 
                         rqst_time_info->tm_hour,
                         rqst_time_info->tm_min,
                         rqst_time_info->tm_sec);
            }
            else
            {
                rqst_event_num = -1;

                sprintf (result_text,
                         "Maximum outstanding RPC requests exceeded");

                output_err ("Process_Es_Rpc_Rqst",
                            MI_TNI_MAX_RPC_RQSTS,
                            MX_ERR_LVL_WARNING,
                            err_string);

                result_code = MXSRV_RPC_ERR_MAXRQSTS_EXCD;
            }
        }
        else
        {
            rqst_event_num = -1;

            sprintf (result_text,
                     "Unable to send RPC request, application %s is undefined",
                     rpc_rqst.applicationName);

            sprintf (err_string.par1, rpc_rqst.applicationName);

            output_err ("Process_Es_Rpc_Rqst",
                        MI_TNI_NOSEND_APP,
                        MX_ERR_LVL_ERROR,
                        err_string);

            result_code = MXSRV_RPC_ERR_APP_NOTDEF;
            rc = P_FAILURE;
        }

    }
    else
    {
        tnicon_p->tot_rpc_rqsts_noack++;
        tnicon_p->num_rpc_rqsts_noack++;
       
        sprintf (rpc_request_tag, "\n");
    }

    if (rc == P_SUCCESS)
    {
        /* validate rpc message length in this routine */

        rc = Send_Rpc_To_Client (app_num,
                                 rpc_request_tag,
                                 REQUEST,
                                 ENC_NONE,
                                 rpc_rqst.bodyLength,
                                 (unsigned char *)rpc_rqst.body,
                                 &result_code,
                                 result_text);

    }

    if (rc == P_SUCCESS)
    {
        if (!rpc_rqst.rpcAckRequired)
        {
            Build_Es_Rpc_Gols_Reply (procom_buffer_num,
                                     reply_queue_num,
                                     P_SUCCESS,
                                     NULL,
                                     NULL,
                                     0);
        }
    }

    if (rc == P_FAILURE)
    {
        tnicon_p->tot_rpc_rqst_failures++;
        tnicon_p->num_rpc_rqst_failures++;

/* Deallocate event, if one was allocated */

        if (rqst_event_num != -1)
        {
 
            rc = Find_Rpc_Rqst_Event (rqst_event_num,
                                      app_num,
                                      procom_buffer_num,
                                      cdc,
                                      rqst_time_info->tm_hour,
                                      rqst_time_info->tm_min,
                                      rqst_time_info->tm_sec,
                                      &event);

            if (rc == P_SUCCESS)
            {
                rc = Deallocate_Rpc_Rqst_Event (event);
            }
        }

        Build_Es_Rpc_Gols_Reply (procom_buffer_num,
                                 reply_queue_num,
                                 result_code,
                                 result_text,
                                 NULL,
                                 0);
    }
}

/* [Build_Es_Rpc_Gols_Reply]
 *
 * Summary:
 *
 * Build_Es_Rpc_Gols_Reply(int reply_queue_num, 
 *                         int result_code, 
 *                         char *result_text, 
 *                         char *result_data, 
 *                         long len_of_result_data)
 *
 * reply_queue_num    - application queue number the reply is to be sent to
 * result_code        - result indicator (success/ failure) of RPC execute
 * result_text        - pointer to text with falure description
 * result_data        - pointer to RPC result data
 * len_of_result_data - length of RPC result data
 *
 * Description:
 *
 * Place RPC result indicator, result text, and result data into 
 * product-to-product reply message.  Reply is sent to product.  This function
 * assumes the product-to-product request message transaction has already been 
 * restored, if required.
 *
 * Returns Values: None
 *
 */

void
Build_Es_Rpc_Gols_Reply(int procom_buffer_num,
                        int reply_queue_num, 
                        int result_code, 
                        char *result_text, 
                        char *result_data, 
                        long len_of_result_data)
{
    long result_text_len = 0;

    if (result_code == P_SUCCESS)
    {
        QUEUE_RPC_RESP (&procom_buffer_num,
                        &reply_queue_num,
                        &result_code,
                        &len_of_result_data,
                        result_data);
    }
    else
    {
        result_text_len = strlen(result_text);

        QUEUE_RPC_RESP (&procom_buffer_num,
                        &reply_queue_num,
                        &result_code,
                        &result_text_len,
                        result_text);
    } 

    return;
}    

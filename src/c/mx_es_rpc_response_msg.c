static const char *fileid = "";

/*
 * ===[mx_es_rpc_repsonse_msg.c]==========================================
 *
 * Description:
 *
 * Functions to process an RPC response message
 *
 * Functions:
 *
 * Send_Rpc_Response_To_Game - Parse RPC Client Data PDU
 * Process_Rpc_Msg_Block - Process each RPC message in RPC Client Data PDU
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

#include <errno.h>
#include <string.h>

#include "includes.h"

/* [Send_Rpc_Response_To_Game]
 *
 * Summary:
 *
 * Send_Rpc_Response_To_Game(char *rpc_request_tag,
 *                           int reply_code,
 *                           char *reply_text,
 *                           long int mes_len,
 *                           char *msg_data,
 *                           int calc_resp_time)
 *
 * rpc_request_tag - pointer to the RPC request tag of response
 * reply_code      - reply code to send to product
 * reply_text      - error string to send to product
 * mes_len         - length of RPC response message
 * msg_data        - pointer to RPC response message
 * calc_resp_time  - boolean to determine if response time should be calcualted
 *
 * Description:
 *
 * This function breaks the RPC request tag into its component parts.
 * Finds the assoicated request event and send the RPC response message to
 * the product that initiated the RPC request.  If the RPC request tag is 
 * invalid, then the RPC response is discarded.  If a request event can't
 * be found, then the RPC response message is discarded.
 *
 * Returns Values: None
 *
 */

void
Send_Rpc_Response_To_Game(char *rpc_request_tag,
                          int reply_code,
                          char *reply_text,
                          long int mes_len,
                          char *msg_data,
                          int calc_resp_time)
{
    int                 rc = P_SUCCESS;             /* return code */
    int                 app_num = -1;
    int                 reply_queue_num = -1;
    int                 rqst_event_num = -1;
    int                 product_num = -1;
    int                 transaction_id = -1;
    int                 rqst_cdc = -1;
    int                 rqst_hours = -1;
    int                 rqst_minutes = -1;
    int                 rqst_seconds = -1;
    int                 rpc_rqst_time_secs = 0;
    int                 rpc_resp_time_secs = 0;
    int                 delta_cdc = 0;
    int                 response_time = 0;
    int                 current_cdc;

    char                work_tag[MAX_RPC_RQST_TAG_LEN + 1];

    time_t              raw_resp_time;
    struct tm          *resp_time_info;

    RQST_EVENT         *event = NULL;

    err_string = null_err_string;

    strncpy(work_tag, rpc_request_tag, MAX_RPC_RQST_TAG_LEN + 1);

#   if defined(PROSYS_ENV_ALL)

        current_cdc = gblcom_p->hdr.cdc;

#   endif

#   if defined(GOLS_ENV_ALL)

        GET_CDC (&current_cdc);

#   endif

/* Parse RPC request tag */

    rc = Parse_Rpc_Request_Tag (work_tag,
                                &rqst_event_num,
                                &app_num,
                                &transaction_id,
                                &rqst_cdc,
                                &rqst_hours,
                                &rqst_minutes,
                                &rqst_seconds);

    if (rc == P_SUCCESS)
    {
        rc = Find_Rpc_Rqst_Event (rqst_event_num,
                                  app_num,
                                  transaction_id,
                                  rqst_cdc,
                                  rqst_hours,
                                  rqst_minutes,
                                  rqst_seconds,
                                  &event);

        if (rc == P_SUCCESS)
        {

#           if defined(GOLS_ENV_ALL)

                reply_queue_num = event->product_num;

#           endif

/* Only calculate response time if request */

            if (calc_resp_time)
            {
                response_time = ((current_time.time -
                                  event->sent_time.time) * 1000) +
                                 (current_time.millitm -
                                  event->sent_time.millitm);

                tnicon_p->tot_rpc_resp_time += response_time;

                Update_Min_Max_Resp_Times (response_time,
                                           &tnicon_p->min_rpc_resp_time,
                                           &tnicon_p->max_rpc_resp_time);
            }

            rc = Deallocate_Rpc_Rqst_Event (event);

#           if defined(PROSYS_ENV_ALL)

/* Attempt to restore trabuf */

                rc = Restore_Tra (transaction_id);

                if (rc == P_FAILURE)
                {
                    sprintf (err_string.par1, "RPC request");
                    sprintf (err_string.par2, "%s", rpc_request_tag);

                    output_err ("Send_Rpc_Response_To_Game",
                                MI_TNI_NORESTR_TRANS,
                                MX_ERR_LVL_ERROR,
                                err_string);
                }

#           endif

        }
        else
        {

/* Check here for late response, but only if the reply code is SUCCESS */

            if (reply_code == P_SUCCESS)
            {
                time (&raw_resp_time);
                resp_time_info = localtime (&raw_resp_time);

                rpc_rqst_time_secs = (rqst_hours * 3600) +
                                     (rqst_minutes * 60) +
                                     rqst_seconds; 

                rpc_resp_time_secs = (resp_time_info->tm_hour * 3600) +
                                     (resp_time_info->tm_min * 60) +
                                     resp_time_info->tm_sec;

                if (rqst_cdc < current_cdc)
                {
                    rpc_resp_time_secs += 86400;

                    delta_cdc = current_cdc - rqst_cdc;

                    if (delta_cdc >= 2)
                    {
                        rpc_resp_time_secs += (delta_cdc - 1) * 86400;
                    }
                }

                if ((rpc_resp_time_secs - rpc_rqst_time_secs) >= 
                    tnicon_p->app[app_num].rpc_rqst_timeout)
                {
                    tnicon_p->tot_rpc_resp_late++;
                    tnicon_p->num_rpc_resp_late++;

                    log_rpc_transaction(RPC_ERR_LATE_RESP,
                                        rpc_request_tag,
                                        mes_len,
                                        msg_data);

                    sprintf (err_string.par1, "RPC response");
                    sprintf (err_string.par2, "%s", rpc_request_tag);

                    output_err ("Send_Rpc_Response_To_Game",
                                MI_TNI_LATE_RESP,
                                MX_ERR_LVL_WARNING,
                                err_string);
                }
                else
                {
                    tnicon_p->tot_rpc_resp_failures++;
                    tnicon_p->num_rpc_resp_failures++;

                    log_rpc_transaction(RPC_ERR_NO_OUTSTND_RQST,
                                        rpc_request_tag,
                                        mes_len,
                                        msg_data);

                    sprintf (err_string.par1, "RPC response");
                    sprintf (err_string.par2, "%s", rpc_request_tag);

                    output_err ("Send_Rpc_Response_To_Game",
                                MI_TNI_NOOUT_RQST,
                                MX_ERR_LVL_WARNING,
                                err_string);
                }
            }
            else
            {
                log_rpc_transaction(RPC_ERR_NO_OUTSTND_RQST,
                                    rpc_request_tag,
                                    mes_len,
                                    msg_data);
            }
        }
    }
    else
    {
        if (reply_code == P_SUCCESS)
        {
            tnicon_p->tot_rpc_resp_failures++;
            tnicon_p->num_rpc_resp_failures++;
        }

        log_rpc_transaction(RPC_ERR_INV_RQST_TAG,
                            rpc_request_tag,
                            mes_len,
                            msg_data);

        sprintf (err_string.par1, "RPC response");

        output_err ("Send_Rpc_Response_To_Game",
                    MI_TNI_MSG_DISCARD,
                    MX_ERR_LVL_WARNING,
                    err_string);
    }

    if (rc == P_SUCCESS)
    {
        if (reply_code == P_SUCCESS)
        {
            tnicon_p->tot_rpc_resps++;
            tnicon_p->num_rpc_resps++;
        } 

#       if defined(PROSYS_ENV_ALL)

            Build_Es_Rpc_Prod_Reply (product_num,
                                     reply_code,
                                     reply_text,
                                     msg_data,
                                     mes_len);

#       endif

#       if defined(GOLS_ENV_ALL)

            Build_Es_Rpc_Gols_Reply (transaction_id,
                                     reply_queue_num,
                                     reply_code,
                                     reply_text,
                                     msg_data,
                                     mes_len);

#       endif

    }

    return;
}

/* [Parse_Rpc_Request_Tag]
 *
 * Summary:
 *
 * Parse_Rpc_Request_Tag(char *rpc_request_tag,
 *                       int *rqst_event_num,
 *                       int *app_num,
 *                       int *transaction_id,
 *                       int *cdc,
 *                       int *hours,
 *                       int *minutes,
 *                       int *seconds)
 *
 * rpc_request_tag - pointer to the RPC request tag
 * rqst_event_num  - pointer to the event number extracted from tag
 * app_num         - pointer to the application number extracted from tag
 * transaction_id  - pointer to the GTMS trans Id extracted from tag
 * cdc             - pointer to the cdc extracted from tag
 * hours           - pointer to the hours extracted from tag
 * minutes         - pointer to the minutes extracted from tag
 * seconds         - pointer to the seconds extracted from tag
 *
 * Description:
 *
 * This function parses through the RPC request tag.  Each component is
 * extracted and converted into an integer.  Any missing components or
 * convertion errors cause the function to fail.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Parse_Rpc_Request_Tag(char *rpc_request_tag,
                      int *rqst_event_num,
                      int *app_num,
                      int *transaction_id,
                      int *cdc,
                      int *hours,
                      int *minutes,
                      int *seconds)
{
    int                 rc = P_FAILURE;             /* return code */
    char               *temp = NULL;

    err_string = null_err_string;

    errno = 0;

    sprintf (err_string.par2, "%s", rpc_request_tag);

    if (rpc_request_tag != NULL)
    {
        temp = strtok (rpc_request_tag, "-");

        if (temp != NULL)
        {
            *rqst_event_num = atoi (temp);

            if (errno == 0)
            {
                 rc = P_SUCCESS;
            }
        }

        if (rc == P_FAILURE)
        {
            sprintf (err_string.par1, "Event Number");

            output_err ("parseRpcRequestTag",
                        MI_TNI_BAD_RQSTAG,
                        MX_ERR_LVL_WARNING,
                        err_string);
        }
    }
    else
    {
        sprintf (err_string.par1, "RPC response");

        output_err ("parseRpcRequestTag",
                    MI_TNI_NORPC_RQSTAG,
                    MX_ERR_LVL_WARNING,
                    err_string);

        rc = P_FAILURE;
    }

    if (rc == P_SUCCESS)
    {
        errno = 0;
        rc = P_FAILURE;

        temp = strtok (NULL, "-");

        if (temp != NULL)
        {
            *app_num = atoi (temp);

            if (errno == 0)
            {
                rc = P_SUCCESS;
            }
        }

        if (rc == P_FAILURE)
        {
            sprintf (err_string.par1, "Application Number");

            output_err ("parseRpcRequestTag",
                        MI_TNI_BAD_RQSTAG,
                        MX_ERR_LVL_WARNING,
                        err_string);
        }
    }

    if (rc == P_SUCCESS)
    {
        errno = 0;
        rc = P_FAILURE;

        temp = strtok (NULL, "-");

        if (temp != NULL)
        {
            *transaction_id = atoi (temp);

            if (errno == 0)
            {
                rc = P_SUCCESS;
            }
        }

        if (rc == P_FAILURE)
        {
            sprintf (err_string.par1, "Trans Id");

            output_err ("parseRpcRequestTag",
                        MI_TNI_BAD_RQSTAG,
                        MX_ERR_LVL_WARNING,
                        err_string);
        }
    }

    if (rc == P_SUCCESS)
    {
        errno = 0;
        rc = P_FAILURE;

        temp = strtok (NULL, "-");

        if (temp != NULL)
        {
            *cdc = atoi (temp);

            if (errno == 0)
            {
                rc = P_SUCCESS;
            }
        }

        if (rc == P_FAILURE)
        {
            sprintf (err_string.par1, "CDC");

            output_err ("parseRpcRequestTag",
                        MI_TNI_BAD_RQSTAG,
                        MX_ERR_LVL_WARNING,
                        err_string);
        }
    }

    if (rc == P_SUCCESS)
    {
        errno = 0;
        rc = P_FAILURE;

        temp = strtok (NULL, ":");

        if (temp != NULL)
        {
            *hours = atoi (temp);

            if (errno == 0)
            {
                rc = P_SUCCESS;
            }
        }

        if (rc == P_FAILURE)
        {
            sprintf (err_string.par1, "Hours");

            output_err ("parseRpcRequestTag",
                        MI_TNI_BAD_RQSTAG,
                        MX_ERR_LVL_WARNING,
                        err_string);
        }
    }

    if (rc == P_SUCCESS)
    {
        errno = 0;
        rc = P_FAILURE;

        temp = strtok (NULL, ".");

        if (temp != NULL)
        {
            *minutes = atoi (temp);

            if (errno == 0)
            {
                rc = P_SUCCESS;
            }
        }

        if (rc == P_FAILURE)
        {
            sprintf (err_string.par1, "Minutes");

            output_err ("parseRpcRequestTag",
                        MI_TNI_BAD_RQSTAG,
                        MX_ERR_LVL_WARNING,
                        err_string);
        }
    }

    if (rc == P_SUCCESS)
    {
        errno = 0;
        rc = P_FAILURE;

        temp = strtok (NULL, ".");

        if (temp != NULL)
        {
            *seconds = atoi (temp);

            if (errno == 0)
            {
                rc = P_SUCCESS;
            }
        }

        if (rc == P_FAILURE)
        {
            sprintf (err_string.par1, "Seconds");

            output_err ("parseRpcRequestTag",
                        MI_TNI_BAD_RQSTAG,
                        MX_ERR_LVL_WARNING,
                        err_string);
        }
    }

    return(rc);
}

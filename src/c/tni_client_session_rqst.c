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
/*====[TNI_CLIENT_SESSION_RQST.C]=============================================*/
/*                                                                            */
/* Client_Session_Request (struct PDU_STRUCT *pdu_struct,                     */
/*                         long int pdulen,                                   */
/*                         unsigned short conn)                               */
/*                                                                            */
/* Purpose: This function parses the Variable Data Part of a single Client    */
/*          Session Request PDU contained in pdu_struct, validates the        */
/*          parameters, loads their values into the global section, and sends */
/*          the Client Session Response PDU.                                  */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_struct       structure containing an entire Client Session    */
/*                           Request PDU, i.e. PDU fixed header part followed */
/*                           by variable data part.                           */
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
/*====[TNI_CLIENT_SESSION_RQST.C]=============================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

void
Client_Session_Request (struct PDU_STRUCT *pdu_struct,
                        long int pdulen,
                        unsigned short conn)
{
    int act_conn;                          /* Actual connection PDU was sent  */
    int conn_num = 1;
    int found_conn = 0;
    int offset;                            /* offset to each parameter        */
                                           /* code/length/value block         */
                                           /* contained in the var part of the*/
    int parm_good;                         /* status of the check_paramter    */
    int parm_count;
    int rc = P_SUCCESS;                    /* generic return code             */

    long int length;                       /* number of bytes in a single     */
                                           /* parameter unit, including code, */
                                           /* length, value fields            */

    long int value;                        /* generic parameter value returned*/
                                           /* by Check_Parameter              */
    long int result_code;

    char string_val[MAX_SESSION_TAG_LEN + 1];
    char session_tag[MAX_SESSION_TAG_LEN + 1];

    unsigned char param_code;              /* PDU parameter code              */

    struct TNI_DATA *param_data;           /* pointer to a parameter          */
                                           /* code/length/value block         */
    struct mbuf *mymbuf;                   /* pointer to mbuf containing      */
                                           /* CLient Parameter Response PDU   */
    struct TNI_PARAM_PAIR params[3];    

    time_t              raw_rqst_time;
    struct tm          *rqst_time_info;

    mymbuf = NULL;   

    err_string = null_err_string;

    Print_Pdu ("client session request",pdu_struct,pdulen);

    tnicon_p->clt_session_req_cnt_tot[conn]++;

/* Initialize all valid parameters so we can determine which parameters       */
/* are present in the PDU.                                                    */

    parm_count = 0;
    result_code = INVALID_SESSION_TAG;
    session_tag[0] = '\0';

/*  Read parameters if they are present */

    if (pdulen > sizeof (struct TNI_FIXED_HDR))
    {
        for (offset = 0;
             offset < (pdulen - sizeof (struct TNI_FIXED_HDR));
             offset+= length )
        {
            param_data = (struct TNI_DATA *)&pdu_struct->msg_data[offset];

            memset(string_val, 0x00, MAX_SESSION_TAG_LEN + 1);

            parm_good = Check_Parameter (conn,
                                         param_data, 
                                         &param_code, 
                                         &length, 
                                         &value,
                                         string_val);

            if( !parm_good )
            {
                LogPdu(pdu_struct, -1, conn);

                rc = send_invalid_pdu_error (conn);

                return;
            }

/* length returned by check_parameter is only the parameter value length      */
/* i.e. it excludes the size of the parameter code and parameter length       */
/* fields                                                                     */

            length += sizeof(param_data->param_code) +
                      sizeof(param_data->param_length);

            switch (param_code)
            {
 
            case SESSION_TAG:
                strncpy(session_tag, string_val, MAX_SESSION_TAG_LEN + 1);

                params[parm_count].param_code = SESSION_TAG;
                strncpy (params[parm_count].param_value_char,
                         session_tag,
                         MAX_SESSION_TAG_LEN + 1);
                parm_count++;

                break;

            }                                      /* end switch (param_code) */
        }                                          /* end for                 */
    }

/* A session can be established only after the client has sent the Client     */
/* Parameter Request PDU.                                                     */

    if (conn <= MAX_CONFIGURABLE_CONN)
    {
        if (session_tag[0] == '\0')  /* create session tag */
        {
            time (&raw_rqst_time);
            rqst_time_info = localtime (&raw_rqst_time);

            sprintf (session_tag, 
                     "%d-%d-%d-%d:%d.%d",
                     tnicon_p->connection[conn].app_idx,
                     conn,
                     rqst_time_info->tm_yday,
                     rqst_time_info->tm_hour,
                     rqst_time_info->tm_min,
                     rqst_time_info->tm_sec);

            strncpy (tnicon_p->connection[conn].session_tag,
                     session_tag,
                     MAX_SESSION_TAG_LEN);

            params[parm_count].param_code = SESSION_TAG;
            strncpy (params[parm_count].param_value_char,
                     session_tag,
                     MAX_SESSION_TAG_LEN + 1);
            parm_count++;

            result_code = SUCCESS;
        }
        else  /* verify at least on other connection shares the session tag */
        {
            while ((conn_num <= MAX_CONN) && (!found_conn))
            {
                if ((tnicon_p->connection[conn_num].conn_state > CONN_DEFINED)
                    && (tnicon_p->connection[conn_num].app_idx ==
                        tnicon_p->connection[conn].app_idx))
                {
                    if (!strcmp (tnicon_p->connection[conn_num].session_tag,
                                 session_tag))
                    {
                        strncpy (tnicon_p->connection[conn].session_tag,
                                 session_tag,
                                 MAX_SESSION_TAG_LEN);

                        result_code = SUCCESS;
                        found_conn = 1;
                    }
                }
                conn_num++;
            }
        }
    }
    else
    {
        result_code = CLT_NOT_IDENTIFIED;
    }

/* Now load all the parameters allowed on the response and build the response */

    params[parm_count].param_code = RESULT_CODE;
    params[parm_count].param_value = result_code;
    parm_count++;

    params[parm_count].param_code = 0;                /* NULL terminated list */
    params[parm_count].param_value = 0;

    mymbuf = Build_Pdu (conn, CLT_SESSION_RESP_PDU, NULL, &params[0]);

    /* Send response on the same connection on which CLIENT SESSION REQUEST   */
    /* PDU was received                                                       */

    act_conn = Send_To_Conn (mymbuf, conn, NO_ALT_CONN);

    if (act_conn == 0)
    {
        sprintf(err_string.par1,"CLIENT SESSION RESPONSE");
        sprintf(err_string.par2,"%d",conn);

        output_err("Client_Session_Request",
                   MI_TNI_ERR_SEND,
                   MX_ERR_LVL_ERROR,
                   err_string);
    }
    else
    {
        tnicon_p->clt_session_resp_cnt_tot[act_conn]++;
    }
}

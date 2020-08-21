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
/*====[TNI_CHECK_GAME_OUTPUT.C]===============================================*/
/*                                                                            */
/* Check_Game_Output (void)                                                   */
/*                                                                            */
/* Purpose: This function checks all sockets whose connection states are      */
/*          greater than DEFINED, i.e. CONNECTED, NOT PRIMARY; CONNECTED,     */
/*          SWITCH PRIMARY; CONNECTED, HOST PRIMARY, CONNECTED, PRIMARY       */
/*          PENDING; or CONNECTED, PRIMARY to determine if it's time to send  */
/*          a Host alive message to the host via the socket.  If it's  time to*/
/*          send to the host, then the message is sent immediately, i.e. it   */
/*          is not buffered in TNI.  In addition, this routine also           */
/*          determines if a socket has timed out, i.e. we have not received   */
/*          anything on the socket for 3 times the keepalive interval.  If    */
/*          a socket has timed out, the close the connection and change the   */
/*          connection's state to CONN_DEFINED.                               */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_CHECK_GAME_OUTPUT.C]===============================================*/
/*                                                                            */

#include "includes.h"

#if defined(GOLS_ENV_ALL)

    void Check_Game_Output (void)
    {
        int               cdc;
        int               procom_buffer_num;
        int               mx_cmd_status;
        int               reply_queue_num;
        unsigned long int term_net_id;
        unsigned long int delivery_mode;
        long int          msg_buf_len;
        unsigned char     msg_buffer[MXSRV_MAX_APP_MSG];
        char              app_name[MXSRV_MAX_APP_NAME_LEN + 1];

        TERM_STATS       *term_stats_p;

        memset (app_name, '\0', (MXSRV_MAX_APP_NAME_LEN + 1));

        err_string = null_err_string;

        tnicon_p->gr_receive_count++;

        DEQUEUE_FROM_GAME (&term_net_id,
                           &msg_buf_len,
                           &delivery_mode,
                           &procom_buffer_num,
                           &reply_queue_num,
                           &cdc,
                           app_name,
                           msg_buffer);

        while (term_net_id != 0)
        {
            switch (delivery_mode)
            {
                case RESPONSE:

                    Send_Oltp_Resp_Msg (term_net_id,
                                        msg_buf_len,
                                        msg_buffer);
  
                    break;

                case UNSOLICITED:

                    if ((term_net_id > 0) &&
                        (term_net_id <= max_terminal))
                    {
                        Send_Oltp_Unso_Msg (term_net_id,
                                            msg_buf_len,
                                            msg_buffer);
                    }
                    else
                    {
                        sprintf(err_string.par1, "OLTP unsolicited");

                        output_err("Check_Game_Output",
                                   MI_TNI_NOPROC_MSG,
                                   MX_ERR_LVL_ERROR,
                                   err_string);

                        sprintf(err_string.par1,"%d",term_net_id);
                        sprintf(err_string.par2,"1");
                        sprintf(err_string.par3,"%d",max_terminal);

                        output_err("Check_Game_Output",
                                   ME_BAD_TER_RNG,
                                   MX_ERR_LVL_ERROR,
                                   err_string);
                    }
  
                    break;

                case BROADCAST:

                    term_net_id = 0x000000FF;

                    Send_Oltp_Bro_Msg (term_net_id,
                                       msg_buf_len,
                                       msg_buffer);

                    break;

                case MX_COMMAND:

                    mx_cmd_status = Process_Command((COMMAND_INFO *)msg_buffer);
  
                    break;

                case GOLS_RPC_REQUEST:
 
                    Process_Es_Rpc_Rqst(cdc,
                                        procom_buffer_num,
                                        reply_queue_num,
                                        app_name,
                                        msg_buf_len,
                                        msg_buffer);

                    break;
            }
 
            memset (app_name, '\0', (MXSRV_MAX_APP_NAME_LEN + 1));

            tnicon_p->gr_receive_count++;

            DEQUEUE_FROM_GAME (&term_net_id,
                               &msg_buf_len,
                               &delivery_mode,
                               &procom_buffer_num,
                               &reply_queue_num,
                               &cdc,
                               app_name,
                               msg_buffer);
        }

        tnicon_p->gr_nomsg_count++;
    }

#endif

#if defined(PROSYS_ENV_ALL)

    G_ERROR  error;
    GM_ADDRESS srcadr;
    GM_MESDSC mesdsc, cntxtdsc;

    void Check_Game_Output (GM_MESDSC *mesdsc, 
                            GM_MESDSC *cntxtdsc) {

        int product;
        int result;
        int receive;

    /* See if GTMS has any messages to deliver  */  

        receive = 1;

        while( receive ) {

        /* update running sum of total number of times we attempt to    */
        /* dequeue a messages from GTMS                                 */

            tnicon_p->gr_receive_count++;

            result=gtms_receive(&srcadr, mesdsc, cntxtdsc, &error);

            if( error.errflg ) {
                g_senderror(&error);
                break;
            }

        /* Service any request from GTMS */

            switch( result ) {

        /* All replies will be resolved  */
        /* by executing context function */
            
            case GR_REPLY:                
                reply(&srcadr, mesdsc, cntxtdsc);
                break;

            /* Request coming from PX2X ie. terminal network */ 
            /* Process message and send to TNI client Host   */ 

            case GR_REQUEST:

                switch( srcadr.adrtyp ) {
                
                case GAT_GTMS:
                    do_gtms_request(mesdsc);
                    receive=0;
                    break;

                case GAT_PRODUCT:
                    product = srcadr.adrval.prod.num;
                    Do_Prod_Request(product, mesdsc);
                    break;

                case GAT_TASK:
                    Do_Task_Request(mesdsc);
                    break;

                default:  
                    /* Report invalid source address type */
                    g_seterror(GTMS_PRODNUM, GE_INVALID, GEL_WARNING, &error);
                    g_adderrstr("srcadr.adrtyp", &error);
                    g_adderrint(srcadr.adrtyp, &error);
                    g_senderror(&error);

                    /* Acknowledge the message & log it to MJF */
                    Do_Unexpected_Request(mesdsc);
                    break;

                } /* end switch (srcadr.adrtyp) */
                break;

            /* GTMS has nothing for us. Take this time     */
            /* to process any messages coming in from the  */ 
            /* client network                              */

            case GR_NOMESSAGE:
                result = 0;
                receive = 0;

            /* update running sum of total number times we have attempted */
            /* to dequeue messages from GTMS but we have nothing to read  */

                tnicon_p->gr_nomsg_count++;
                break; 

            default:
                g_setgtmserr(GE_INVALID, &error);
                g_adderrstr("result", &error);
                g_adderrint(result, &error);
                error.errlvl = GEL_INFO;
                g_senderror(&error);
                error.errflg=0;
                break;

            } /* end switch */

        }  /* end while receive */
    }

#endif

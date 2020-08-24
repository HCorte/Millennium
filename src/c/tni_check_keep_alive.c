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
/*====[TNI_CHECK_KEEP_ALIVE.C]================================================*/
/*                                                                            */
/* Check_Keep_Alive()                                                         */
/*                                                                            */
/* Purpose: This function checks all sockets whose connection states are      */
/*          greater than DEFINED, i.e. CONNECTED, NOT PRIMARY; CONNECTED,     */
/*          SWITCH PRIMARY; CONNECTED, HOST PRIMARY, CONNECTED, PRIMARY       */
/*          PENDING; or CONNECTED, PRIMARY to determine if it's time to send  */
/*          a TNI alive message to the host via the socket.  If it's  time to */
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
/*====[TNI_CHECK_KEEP_ALIVE.C]================================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

void Check_Keep_Alive() {
    int act_conn;                        /* Actual connection PDU was sent    */
    int j;                               /* local connection index            */
    int st;                              /* rtn code from VMS system service  */
    struct mbuf *mymbuf;                 /* pointer to mbuf containing TNI    */
                                         /* Alive PDU                         */
    struct timeb elapsed_time;

    err_string = null_err_string;

    for( j = 1; j <= MAX_CONN ; j++ ) {

        if( tnicon_p->connection[j].conn_state >=
            CONN_CONNECTED ) {

            subtimes(&current_time, &tnicon_p->connection[j].time_last_sent,
                     &elapsed_time);

            if( (elapsed_time.time > tnicon_p->connection[j].keepalive_time.time) ||
                ((elapsed_time.time == tnicon_p->connection[j].keepalive_time.time) &&
                 (elapsed_time.millitm >= tnicon_p->connection[j].keepalive_time.millitm))
              ) {

                mymbuf = Build_Pdu (j, SERVER_ALIVE_PDU, NULL, NULL);

                act_conn = Send_To_Conn (mymbuf, j, NO_ALT_CONN);

                if (act_conn == 0) {

                    sprintf(err_string.par1,"ERROR");
                    sprintf(err_string.par2,"%d",j);

                    output_err("Check_Keep_Alive",
                               MI_TNI_ERR_SEND,
                               MX_ERR_LVL_ERROR,
                               err_string);
                } else {

                    tnicon_p->srv_alive_cnt_tot[act_conn]++;
                }
            }

            subtimes(&current_time, &tnicon_p->connection[j].time_last_rcvd,
                     &elapsed_time);

            if( (elapsed_time.time > tnicon_p->connection[j].conn_alive_timeout.time) ||
                ((elapsed_time.time == tnicon_p->connection[j].conn_alive_timeout.time) &&
                 (elapsed_time.millitm >= 
                  tnicon_p->connection[j].conn_alive_timeout.millitm)) ) {

                output_err("Check_Keep_Alive",
                           MI_TNI_TIMEOUT,
                           MX_ERR_LVL_ERROR,
                           err_string);

                Close_Connection(j);          
            }

        }                                           /* end of if state code   */
    }                                               /*      end socket loop   */
}

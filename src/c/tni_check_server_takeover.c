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
/* ===[TNI_CHECK_SERVER_TAKEOVER.C]===========================================*/
/*                                                                            */
/* Functions:                                                                 */
/*           Check_Server_Takeover()                                          */
/*           All_Clients_Connected(int *connection)                           */
/*           Awaiting_Notification(int connection)                            */
/*                                                                            */
/* ===[TNI_CHECK_SERVER_TAKEOVER.C]===========================================*/
/*                                                                            */

#include <stdio.h>
#include <string.h>

#include "includes.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void Check_Server_Takeover()                                               */
/*                                                                            */
/* Purpose: To determine if the system changed its state from backup to       */
/*          primary or from primary to backup.  If a change is detected, then */
/*          a TNI parameter request PDU is sent to all connected connections. */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Check_Server_Takeover() {

    char   next_state;                     /* next connection state           */

    int    act_conn;                       /* Actual connection PDU was sent  */
    int    conn;                           /* local connection index          */
    int    current_state;                  /* current system state            */

    struct TNI_PARAM_PAIR params[2];       /* sized by max number of PDU      */
                                           /* parameters used in TNI request  */
    struct mbuf *mymbuf;                   /* pointer to mbuf containing TNI  */
                                           /* Parameter Request PDU           */

    err_string = null_err_string;

/* Determine the current state of the system                                  */

#   if defined(PROSYS_ENV_ALL)

        if( (volcom_p->sys.gtms_idn == volcom_p->prod.prime_id)&&
            (volcom_p->prod.prime_id != -1) )
        {
            current_state = PRIMARY;

        }
        else
        {
            current_state = NOT_PRIMARY;
        }

#   endif

#   if defined(GOLS_ENV_ALL)

        GET_GAME_STATE(&current_state);

#   endif

/* Check if a system state change has taken place                             */

    if( current_state != tnicon_p->tni_server_state ) {  

        switch (current_state) {

        case PRIMARY:

            if (tnicon_p->prim_notify_overide) {

                tnicon_p->prim_notify_overide = 0;

            } else {
                if ((!All_Clients_Connected(&conn)) &&
                    (tnicon_p->prim_notify_mode == NOTIFY_WAIT)) {

                    current_state = NOT_PRIMARY;
                    Awaiting_Notification(conn);
                }
            }

            if (current_state == PRIMARY) {

                switch (tnicon_p->comm_mode) {

                case COMM_SYNC:

                    current_state = NOT_PRIMARY;
                    break;
                }
            }
            break;
        }

        if (current_state != tnicon_p->tni_server_state ) {

            tnicon_p->tni_server_state = current_state;   /* Update TNI server*/
                                                          /* state            */

/*          Send a TNI parameter request PDU to all connections in the        */
/*          connected state.                                                  */

            for( conn = 1; conn <= MAX_CONN; conn++ ) {

                if( tnicon_p->connection[conn].conn_state >
                    CONN_CONNECTED ) {

                    next_state = 
                        Get_Next_State (conn, 
                                        tnicon_p->tni_server_state, 
                             tnicon_p->connection[conn].last_client_state);

                    switch( next_state ) {
                    case -1:

                        output_err("Check_Server_Takeover",
                                   MI_TNI_INV_STATE,
                                   MX_ERR_LVL_ERROR,
                                   err_string);
                        break;
                    case 0:
                        break;
                    default:

                       tnicon_p->connection[conn].conn_state = next_state;
                        break;
                    }

                    params[0].param_code = PRIMARY_STATUS;
                    params[0].param_value = tnicon_p->tni_server_state; 
                    params[1].param_code = 0;         /* NULL terminated list */
                    params[1].param_value = 0;

                    mymbuf = Build_Pdu (conn, SERVER_REQUEST_PDU, NULL, &params[0]);

                    act_conn = Send_To_Conn (mymbuf, conn, NO_ALT_CONN);

                    if (act_conn == 0) {

                        sprintf(err_string.par1,"ERROR");
                        sprintf(err_string.par2,
                                "Unable to send PDU over Connection[%d]",
                                conn);

                        output_err("Check_Server_Takeover",
                                   MI_TNI_ERR_SEND,
                                   MX_ERR_LVL_ERROR,
                                   err_string);

                    } else {

                        tnicon_p->srv_param_req_cnt_tot[act_conn]++;

                    }
                }

            }
        }

    }                                    /* end check for system state change */
    return;
}                                        /* end of Tni_Check_Server_Takeover  */

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* All_Clients_Connected(int *connection)                                     */
/*                                                                            */
/* Purpose: This function checks to see if all defined connection are         */
/*          connected.                                                        */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments:                                                          */
/*          connection       Number of a connection not currently connected.  */
/*                                                                            */
/* Return Value:                                                              */
/*          int              = 0 if at least 1 defined connection is not      */
/*                               currently connected                          */
/*                           = 1 All defined connection are currently         */
/*                               connected                                    */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int All_Clients_Connected(int *connection) {

    int conn;
    int conn_count;
    int status;

    status = 1;
    conn_count = 0;
    *connection = 0;

    for( conn = 1; conn <= MAX_CONFIGURABLE_CONN; conn++ ) {

       if (tnicon_p->connection[conn].conn_state == CONN_DEFINED) {

           conn_count++;
           *connection = conn;
           status = 0;
       }
    }

    return(status);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Awaiting_Notification(int connection)                                      */
/*                                                                            */
/* Purpose: This function tells the operator when the Server is unable to     */
/*          send Server primary notification to the Clients.   The error      */
/*          message is sent to the ELOG every check takeover interval until   */
/*          all defined connections are connected.                            */
/*                                                                            */
/* Input Arguments:                                                           */
/*          connection       Number of connection not currently connected.    */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Awaiting_Notification(int connection) {

    static int init = 1;

    static struct timeb last_notify_time;
    struct timeb elapsed_time;

    err_string = null_err_string;

    if (init) {

        last_notify_time = current_time;
        init = 0;
    }

    if ((connection != 0) && (tnicon_p->comm_mode == COMM_ENABLED)) {

        subtimes(&current_time,
                 &last_notify_time,
                 &elapsed_time);

        if ((elapsed_time.time > tnicon_p->takeover_time.time) ||
            ((elapsed_time.time == tnicon_p->takeover_time.time) &&
             (elapsed_time.millitm > tnicon_p->takeover_time.millitm))) {

            sprintf(err_string.par1,
                    "%s",
                    tnicon_p->connection[connection].remote_domain_name);

            output_err("Awaiting_Notification",
                       MI_TNI_AWAITING_CONN,
                       MX_ERR_LVL_WARNING,
                       err_string);

            last_notify_time = current_time;
        }
    }
}

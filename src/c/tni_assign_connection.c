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
/*====[TNI_ASSIGN_CONNECTION.C]===============================================*/
/*                                                                            */
/* Assign_Connection (int client_id,                                          */
/*                    char *app_name ,                                        */
/*                    unsigned short *conn)                                   */
/*                                                                            */
/* Purpose: This function                                                     */
/*                                                                            */
/* Input Arguments:                                                           */
/*          client_id  Client Id value assoicated with the connection         */
/*          *app_name  pointer for  Application Name                          */
/*          *conn      pointer for Connection                                 */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_ASSIGN_CONNECTION.C]===============================================*/
/*                                                                            */

#include <stdio.h>
#include <string.h>

#include "includes.h"

int Assign_Connection (int client_id,
                       char *app_name,
                       unsigned short *conn) {
    unsigned short i;                      /* local indices                   */
    int act_conn;                          /* Actual connection PDU was sent  */
    int app_num = 0;
    int conn_established = 0;
    int conn_idx = 0;
    int conn_num = 0;
    int found_conn = 0;

    struct TNI_PARAM_PAIR params[2];       /* file sized by max number of PDU */
                                           /* parameters used in TNI request  */
    struct mbuf *mymbuf;                   /* pointer to mbuf containing TNI  */
                                           /* Parameter Request PDU           */
    char next_state;                       /* next connection state           */


/* Search all defined connections for the name of the client attempting       */
/* a connection.  If a connection has been defined in the TNI config file     */
/* for the local host and client name, the add the client's information to    */
/* the connection table.                                                      */

    err_string = null_err_string;

    if( *conn >= MAX_CONFIGURABLE_CONN ) {

        if ((tnicon_p->connection[*conn].tni_proto_ver == TNI_VERSION_01) ||
            (tnicon_p->connection[*conn].tni_proto_ver == TNI_VERSION_02))
        {
            conn_idx = 1;

            while ((conn_idx <= MAX_CONFIGURABLE_CONN) &&
                   (!found_conn))
            {
                if ((tnicon_p->connection[conn_idx].managed_by ==
                     CONNS_CLIENT_ID) &&
                    (strcmp(tnicon_p->connection[conn_idx].remote_domain_name,
                            tnicon_p->connection[*conn].remote_domain_name)
                     == 0) &&
                    (strcmp(tnicon_p->connection[conn_idx].local_domain_name,
                            tnicon_p->connection[*conn].local_domain_name)
                     == 0) &&
                    (tnicon_p->connection[conn_idx].client_id_val == client_id))
                {
                    conn_num = conn_idx;

                    if (tnicon_p->connection[conn_num].conn_state
                         > CONN_DEFINED)
                    {
                        conn_established = 1;
                    }
                    else
                    {
                        found_conn = 1;
                        conn_established = 0;
                    } 
                }
                conn_idx++;
            }
        }
        else
        {
            app_num = Find_Application_Number(app_name);

            if (app_num != -1)
            {
                tnicon_p->connection[*conn].app_idx = app_num;

                while ((conn_idx < tnicon_p->app[app_num].client_conn_cnt) &&
                       (!found_conn))
                {
                    conn_num = tnicon_p->app[app_num].client_conn[conn_idx];

                    if (strcmp(tnicon_p->connection[conn_num].remote_domain_name,
                               tnicon_p->connection[*conn].remote_domain_name)
                         == 0)
                    {
                        if (tnicon_p->connection[conn_num].conn_state
                             > CONN_DEFINED)
                        {
                            conn_established = 1;
                        }
                        else
                        {
                            found_conn = 1;
                            conn_established = 0;
                        }
                    }
                    conn_idx++;
                }
            }
            else
            {
                sprintf(err_string.par1, "%s", app_name);

                output_err("Assign_Connection",
                           MI_TNI_NODEF_CONNS,
                           MX_ERR_LVL_WARNING,
                           err_string);
            }
        }
 
/*      Reassign connection number from a temporary one to a          */
/*      configured one.  First clear out temporary connection         */
/*      information.                                                  */

        if (found_conn) 
        {
            tnicon_p->connection[*conn].app_idx = -1;

            tnicon_p->connection[*conn].conn_state = CONN_UNDEFINED;

            strncpy ((char *)tnicon_p->connection[*conn].remote_domain_name,
                     "\0",
                     sizeof(tnicon_p->connection[0].remote_domain_name));

            tnicon_p->connection[conn_num].sock =
                tnicon_p->connection[*conn].sock;

            tnicon_p->connection[*conn].sock = -1;

/*          Copy assigned TNI protocol version                                */

            tnicon_p->connection[conn_num].tni_proto_ver = 
               tnicon_p->connection[*conn].tni_proto_ver;

            tnicon_p->connection[*conn].tni_proto_ver = TNI_VERSION_UNKNOWN;

            *conn = conn_num;

            tnicon_p->connection[conn_num].time_last_sent =
                current_time;
            tnicon_p->connection[conn_num].time_last_rcvd =
                current_time;

/*          Now that we have found a match, load socket information into     */
/*          connection structures                                            */

            tnicon_p->connection[conn_num].conn_state = CONN_CONNECTED;

            next_state =
                Get_Next_State (conn_num,
                                tnicon_p->tni_server_state,
                                tnicon_p->connection[conn_num].last_client_state);

            switch( next_state ) {
            case -1:

                output_err("Assign_Connection",
                           MI_TNI_INV_STATE,
                           MX_ERR_LVL_ERROR,
                           err_string);
                break;
            case 0:
                break;
            default:

                tnicon_p->connection[conn_num].conn_state = next_state;
                break;
            }

/*          use default keepalive timer value until client tells us to use    */
/*          a different timer value                                           */

            tnicon_p->connection[conn_num].keepalive_time =
                Set_Timervalue (DEF_KEEPALIVE_TIME * TICS_PER_SEC);

            tnicon_p->connection[conn_num].conn_alive_timeout =
                Set_Timervalue (DEF_KEEPALIVE_TIME * 3 * TICS_PER_SEC);

/*          Check TNI server task's status and send it to client in TNI       */
/*          Parameter Request PDU                                             */

            params[0].param_code = PRIMARY_STATUS;
            params[0].param_value = tnicon_p->tni_server_state;
            params[1].param_code = 0;
            params[1].param_value = 0;

            mymbuf = Build_Pdu (conn_num, SERVER_REQUEST_PDU, NULL, &params[0]);

            act_conn = Send_To_Conn (mymbuf, conn_num, NO_ALT_CONN);

            if (act_conn == 0)
            {
                sprintf(err_string.par1,"SERVER REQUEST PDU");
                sprintf(err_string.par2,"%d", conn_num);

                output_err("Assign_Connection",
                           MI_TNI_ERR_SEND,
                           MX_ERR_LVL_ERROR,
                           err_string);
            }
            else
            {
                tnicon_p->srv_param_req_cnt_tot[act_conn]++;
            }
        }
        else
        {
            if (conn_established)
            {
                output_err("Assign_Connection",
                           MI_TNI_CONN_ESTABLD,
                           MX_ERR_LVL_WARNING,
                           err_string);
            }
            else
            {
                sprintf(err_string.par1,"%s",
                        tnicon_p->connection[*conn].remote_domain_name);

                if (tnicon_p->connection[*conn].managed_by ==
                    CONNS_CLIENT_ID)
                {
                    sprintf(err_string.par2,"client id %d",
                            client_id);
                }
                else
                {
                    sprintf(err_string.par2,"app %s",
                            app_name);
                }

                output_err("Assign_Connection",
                           MI_TNI_NOT_CONFIGED,
                           MX_ERR_LVL_ERROR,
                           err_string);
            }
        }
    }
    else               /* not a temp connections          */
    {
        found_conn = 1;
    }

    return(found_conn);
}

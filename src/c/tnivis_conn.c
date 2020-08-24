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
/*====[TNIVIS_CONN.C]=========================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Conn()                                                            */
/*          Conn_Summary()                                                    */
/*          Conn_Detailed()                                                   */
/*          Conn_Input(char *command, int par)                                */
/*                                                                            */
/*====[TNIVIS_CONN.C]=========================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

#define MAX_DISPLAYABLE_LINES 14

static int conn_num = 1;
static int confg_details = 1;
static int page;
static int pdus_details = 0;
static int stats_details = 0;

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Conn()                                                                     */
/*                                                                            */
/* Purpose: This function determines which connection snapshot screens to     */
/*          display.  If no connection number is entered with the connection  */
/*          command, then the summary screen is displayed.  If the connection */
/*          command is accompanied with a connection number, then the         */
/*          detailed screen is displayed.                                     */
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

void Conn() {

    static int last_conn_num = 0;

    if (last_conn_num != conn_num) {

        wclear(win);
        last_conn_num = conn_num;
    }

    if (page != PAGE_NEUTRAL) {

        wclear(win);
    }

    if (conn_num != -1) {

        Conn_Detailed();
    } else {

        Conn_Summary();
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Conn_Summary()                                                             */
/*                                                                            */
/* Purpose: This function displays the connection summary screen.             */
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

void Conn_Summary() {

    int app_num;
    int conn;
    int disp_cnt;
    int line_no = 0;
    int something_to_display;

    static int first_disp_conn = 0;
    static int last_disp_conn = 0;
    static int init_conn_sum_dsp = 1;

    char timeret[9];

    struct tm loc_time;
    struct tm *loc_time_p;

    struct timeb last_active_time;

    if (init_conn_sum_dsp) {

        init_conn_sum_dsp = 0;
    }

    loc_time_p = &loc_time;

    wmove(win,line_no,0);
    wprintw(win,"MX CONNECTION SUMMARY SNAPSHOT ");

    wmove(win,line_no, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    line_no++;
    line_no++;

    wmove(win,line_no,4);
    wprintw(win,"Conns  Application  Domain Name  State          Last Time  ");

    wmove(win,line_no,64);
    wprintw(win,"Transactions");

    line_no++;
    wmove(win,line_no,4);
    wprintw(win,"          Name                                   Active");

    wmove(win,line_no,64);
    wprintw(win,"per Second");


    line_no++;
    wmove(win,line_no,4);
    wprintw(win,"-------------------------------------------------------");

    wmove(win,line_no,59);
    wprintw(win,"------------------");


    switch (page)
    {
        case PAGE_FORWARD:

             if (last_disp_conn != MAX_CONFIGURABLE_CONN) {

                 first_disp_conn = last_disp_conn;
             }

             page = PAGE_NEUTRAL;
             break;

        case PAGE_BACKWARD:

             if ((first_disp_conn - MAX_DISPLAYABLE_LINES) > 0) {

                 first_disp_conn = first_disp_conn -
                                   MAX_DISPLAYABLE_LINES;
             } else {
                 first_disp_conn = 1;
             }

             page = PAGE_NEUTRAL;
             break;
    }

    conn = first_disp_conn;
    disp_cnt = 0;
    something_to_display = 1;

    while ((disp_cnt < MAX_DISPLAYABLE_LINES) &&
           (something_to_display)) {

        if (tnicon_p->connection[conn].conn_state >=
            CONN_DEFINED) {

            line_no++;
            wmove(win,line_no,5);
            wprintw(win,"%3d", conn);

            app_num = tnicon_p->connection[conn].app_idx;

            wmove(win,line_no,11);

            if (tnicon_p->connection[conn].managed_by == CONNS_APP_NAME)
            {
                wprintw(win,"%.11s",
                        tnicon_p->app[app_num].name);
            }
            else
            {
                if (tnicon_p->connection[conn].conn_state > 
                    CONN_CONNECTED)
                {
                    if (tnicon_p->connection[conn].tni_proto_ver ==
                        TNI_VERSION_01)
                    {
                        wprintw(win,"Clt Id %d",
                                tnicon_p->connection[conn].client_id_val);
                    }
                    else
                    {
                        if (tnicon_p->connection[conn].messaging_type ==
                            MSG_TYPE_ES_RPC)
                        {
                            wprintw(win,"%-11s",
                                tnicon_p->connection[conn].application_name);
                        }
                        else
                        {
                            if (tnicon_p->connection[conn].application_name !=
                                NULL)
                            {
                                wprintw(win,"%-11s",
                                   tnicon_p->connection[conn].application_name);
                            }
                            else
                            {
                                wprintw(win,"Clt Id %d",
                                      tnicon_p->connection[conn].client_id_val);
                            }
                        }
                    }
                }
                else
                {
                    wprintw(win,"Clt Id %d",
                            tnicon_p->connection[conn].client_id_val);
                }
            }

            wmove(win,line_no,24);
            wprintw(win,"%.11s",
                    tnicon_p->connection[conn].remote_domain_name);

            wmove(win,line_no,37);
            wprintw(win,"%s",
                    states[tnicon_p->connection[conn].conn_state]);

            if (tnicon_p->connection[conn].time_last_sent.time >
                tnicon_p->connection[conn].time_last_rcvd.time ) {

                last_active_time = tnicon_p->connection[conn].time_last_sent;
            } else {

                last_active_time = tnicon_p->connection[conn].time_last_rcvd;
            }

            if (last_active_time.time != 0) {

                loc_time_p = localtime(&last_active_time.time);
                strftime(timeret, 9, "%H:%M:%S", loc_time_p);
            } else {

                sprintf(timeret,"--:--:--");
            }

            wmove(win,line_no,52);
            wprintw(win,"%s ",timeret);

            wmove(win,line_no,66);
            wprintw(win,"%5d",
                ((tnicon_p->clt_data_rqst_msgs_cnt_dis[conn] +
                 tnicon_p->clt_data_resp_msgs_cnt_dis[conn] +
                 tnicon_p->clt_data_unso_msgs_cnt_dis[conn] +
                 tnicon_p->srv_data_rqst_msgs_cnt_dis[conn] +
                 tnicon_p->srv_data_resp_msgs_cnt_dis[conn] +
                 tnicon_p->srv_data_unso_msgs_cnt_dis[conn] )/2)
                     /tnicon_p->statistics_invl);

            disp_cnt++;
        }

        if (conn == MAX_CONFIGURABLE_CONN) {
            something_to_display = 0;
        }
        else
        {
           conn++;
        }
    }
    last_disp_conn = conn;

    wmove(win,21,0);
    wprintw(win,"    F (page forward), B (page back), EXIT, MENU");

    wmove(win,23,0);
    wprintw(win,"Selection: ");

    refresh();
    wrefresh(win);

    wmove(win,23,11);

    input_y = 23;
    input_x = 11;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Conn_Detailed()                                                            */
/*                                                                            */
/* Purpose: This function displays the connection detailed screen.            */
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

void Conn_Detailed() {

    int read_percentage;
    int line_no;
    int app_num;

    char timeret[9];
    struct tm loc_time;
    struct tm *loc_time_p;


    loc_time_p = &loc_time;


    line_no = 0;

    wmove(win,line_no,0);
    wprintw(win,"MX CONNECTION (%d) SNAPSHOT",conn_num);

    wmove(win,line_no, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    line_no++;
    line_no++;

    if (1 == confg_details)
    {
        wmove(win,line_no,2);
        wprintw(win,"Client Configuration Parameters");

        line_no++;
        wmove(win,line_no,2);
        wprintw(win,"-------------------------------");

	line_no++;
	line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Socket Number                   : %d",
                tnicon_p->connection[conn_num].sock);

        line_no++;
	
        wmove(win,line_no,2);
        wprintw(win,"Client Name                     : %.25s",
                tnicon_p->connection[conn_num].remote_domain_name);

        line_no++;

        app_num = tnicon_p->connection[conn_num].app_idx;

        if (tnicon_p->connection[conn_num].conn_state > CONN_CONNECTED)
        {
            wmove(win,line_no,2);
            wprintw(win,"Messaging Type                  : %s",
                    msg_types[tnicon_p->connection[conn_num].messaging_type]);

            line_no++;
	
            wmove(win,line_no,2);

            if (tnicon_p->connection[conn_num].managed_by == CONNS_APP_NAME)
            {
                wprintw(win,"Application Name                : %-15s",
                        tnicon_p->app[app_num].name);
            }
            else
            {
                if (tnicon_p->connection[conn_num].tni_proto_ver == 
                        TNI_VERSION_01)
                {
                    wprintw(win,"Application Name                : ---------------");
                }
                else
                {
                    wprintw(win,"Application Name                : %-15s",
                            tnicon_p->connection[conn_num].application_name);
                }
            }
        }
        else
        {
            wmove(win,line_no,2);
            wprintw(win,"Messaging Type                  : ----");

            line_no++;
	
            wmove(win,line_no,2);
            wprintw(win,"Application Name                : ---------------");
        }

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Maximum PDU Size                : %d",
                tnicon_p->connection[conn_num].max_pdu_size);

        line_no++;
	
        wmove(win,line_no,2);
        wprintw(win,"Maximum Messages per PDU        : %d",
                tnicon_p->connection[conn_num].max_messages_per_pdu);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Keep Alive Interval             : %d (s)",
                tnicon_p->connection[conn_num].keepalive_time);

        line_no++;
	
        wmove(win,line_no,2);
        wprintw(win,"Terminal Identification Method  : %s",
                term_id_methods[tnicon_p->connection[conn_num].term_id_method]);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Blocking Interval               : %d (ms)",
                tnicon_p->connection[conn_num].blocking_time.time*1000
                + tnicon_p->connection[conn_num].blocking_time.millitm);

        line_no++;
	
        wmove(win,line_no,2);
        wprintw(win,"Connection State                : %s", 
                states[tnicon_p->connection[conn_num].conn_state]);

        line_no++;

        wmove(win,line_no,2);
        clrtoeol();

        if (tnicon_p->connection[conn_num].managed_by == CONNS_APP_NAME)
        {
            if (tnicon_p->connection[conn_num].session_tag[0] == '\0')
            {
                wprintw(win,"Session Identifier              : ---------");
            }
            else
            {
                wprintw(win,"Session Identifier              : %s",
                        tnicon_p->connection[conn_num].session_tag);
            }
        }
        else
        {
            wprintw(win,"Client Identifier               : %d",
                    tnicon_p->connection[conn_num].client_id_val);
        }

        line_no++;

        wmove(win,line_no,2);

        if (tnicon_p->connection[conn_num].conn_state > CONN_CONNECTED)
        {
            switch (tnicon_p->connection[conn_num].tni_proto_ver)
            {
                case TNI_VERSION_01:
                    wprintw(win,"TNI Protocol Version            : 1.0");
                    break;

                case TNI_VERSION_02:

                    wprintw(win,"TNI Protocol Version            : 2.0");
                    break;

                default:
                    wprintw(win,"TNI Protocol Version            : %d.%d",
                        (tnicon_p->connection[conn_num].tni_proto_ver & 0xf0) >> 4,
                        (tnicon_p->connection[conn_num].tni_proto_ver & 0x0f));
                    break;
            }
        }
        else
        {
            wprintw(win,"TNI Protocol Version            : ---");
        }

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Authentication State            : %s", 
                auth_states[tnicon_p->connection[conn_num].auth_state]);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Successive Auth Failure Count   : %d", 
                tnicon_p->connection[conn_num].consec_auth_failure_cnt);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Exchange Key Method             : %s",
                exchange_methods[tnicon_p->connection[conn_num].enc_key_x_method]);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Encryption Key Type             : %s",
                encryption_key_types[tnicon_p->connection[conn_num].enc_key_type]);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Encryption State                : %s",
                encryption_states[tnicon_p->connection[conn_num].enc_state]);

        wmove(win,23,0);
        wprintw(win,"    PDUS, STATS, CLOSE, EXIT, MENU");
    }
    else if (1 == pdus_details)
    {
        wmove(win,line_no,2);
        wprintw(win,"PDUs Received From Client");

        wmove(win,line_no,49);  
        wprintw(win,"PDUs Sent To Client");

        line_no++;

	wmove(win,line_no,2);
        wprintw(win,"--------------------------");
	
	wmove(win,line_no,49);
        wprintw(win,"--------------------------");

        line_no++;
        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Clt Parameter Rqst : %8d", 
                tnicon_p->clt_param_req_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"Clt Parameter Resp : %8d", 
                tnicon_p->clt_param_resp_cnt_tot[conn_num]);

        line_no++;

        wmove(win,line_no,2);    
        wprintw(win,"Srv Parameter Resp : %8d",
                tnicon_p->srv_param_resp_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"Srv Parameter Rqst : %8d",
                tnicon_p->srv_param_req_cnt_tot[conn_num]);

        line_no++;
        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Srv Challenge Resp : %8d",
                tnicon_p->srv_challenge_resp_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"Srv Challenge Rqst : %8d",
                tnicon_p->srv_challenge_req_cnt_tot[conn_num]);

        line_no++;

        wmove(win,line_no,49);
        wprintw(win,"Srv Challenge Note : %8d",
                tnicon_p->srv_challenge_notify_cnt_tot[conn_num]);

        line_no++;
        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Srv Enc Key Resp   : %8d",
                tnicon_p->srv_enc_key_resp_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"Srv Enc Key Rqst   : %8d",
                tnicon_p->srv_enc_key_rqst_cnt_tot[conn_num]);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Clt Enc Key Note   : %8d",
                tnicon_p->clt_enc_key_notify_cnt_tot[conn_num]);

        line_no++;
        line_no++;

        wmove(win,line_no,2);   
        wprintw(win,"Clt Session Rqst   : %8d",
                tnicon_p->clt_session_req_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"Clt Session Resp   : %8d",
                tnicon_p->clt_session_resp_cnt_tot[conn_num]);

        line_no++;
        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"OLTP Clt Data      : %8d",
                tnicon_p->oltp_clt_data_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"OLTP Srv Data      : %8d",
                tnicon_p->oltp_srv_data_cnt_tot[conn_num]);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"RPC Clt Data       : %8d",
                tnicon_p->rpc_clt_data_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"RPC Srv Data       : %8d",
                tnicon_p->rpc_srv_data_cnt_tot[conn_num]);

        line_no++;
        line_no++;

        /* if( tnicon_p->read_select_success_count_dis != 0 ) {
            read_percentage = (int)(
                ((float)tnicon_p->conn_read_select_success_count_dis[conn_num]/
                tnicon_p->read_select_success_count_dis)*100);
        } else {
            read_percentage = 0;
        } */

        wmove(win,line_no,2);
        wprintw(win,"Clt Alive          : %8d",
                tnicon_p->clt_alive_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"Srv Alive          : %8d",
                tnicon_p->srv_alive_cnt_tot[conn_num]);

        line_no++;
	
        wmove(win,line_no,2);
        wprintw(win,"Error              : %8d",
                tnicon_p->error_rcvd_cnt_tot[conn_num]);

        wmove(win,line_no,49);
        wprintw(win,"Error              : %8d",
                tnicon_p->error_sent_cnt_tot[conn_num]);

        wmove(win,23,0);
        wprintw(win,"    CONFG, STATS, EXIT, MENU");
    }
    else if (1 == stats_details)
    {
        wmove(win,line_no,(MAXCOL - 18)/2);
        wprintw(win,"Connection Activity");

        line_no++;

        wmove(win,line_no,(MAXCOL - 18)/2);
        wprintw(win,"-------------------");

        line_no++;
        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Bytes Read(kb)     : %8d",
                tnicon_p->conn_read_byte_count_tot[conn_num]/1024);

        wmove(win,line_no,49);
        wprintw(win,"Bytes Written(kb)  : %8d",
                tnicon_p->conn_write_byte_count_tot[conn_num]/1024);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Read Errors        : %8d",
                tnicon_p->conn_read_err_count_tot[conn_num]);    

        wmove(win,line_no,49);
        wprintw(win,"Write Errors       : %8d",
                tnicon_p->conn_write_err_count_tot[conn_num]);

        line_no++;

        wmove(win,line_no,2);
        wprintw(win,"Transactions       : %8d",
                (tnicon_p->clt_data_rqst_msgs_cnt_tot[conn_num] +
                 tnicon_p->clt_data_resp_msgs_cnt_tot[conn_num] +
                 tnicon_p->clt_data_unso_msgs_cnt_tot[conn_num] +
                 tnicon_p->srv_data_rqst_msgs_cnt_tot[conn_num] +
                 tnicon_p->srv_data_resp_msgs_cnt_tot[conn_num] +
                 tnicon_p->srv_data_unso_msgs_cnt_tot[conn_num] )/2);

        wmove(win,line_no,49);
        wprintw(win,"Transaction Rate   :     %4d",
                (tnicon_p->clt_data_rqst_msgs_cnt_dis[conn_num] +
                 tnicon_p->clt_data_resp_msgs_cnt_dis[conn_num] +
                 tnicon_p->clt_data_unso_msgs_cnt_dis[conn_num] +
                 tnicon_p->srv_data_rqst_msgs_cnt_dis[conn_num] +
                 tnicon_p->srv_data_resp_msgs_cnt_dis[conn_num] +
                 tnicon_p->srv_data_unso_msgs_cnt_dis[conn_num] )/2);

        line_no++;
        line_no++;

        loc_time_p = localtime(&tnicon_p->last_stats_time.time);
        strftime(timeret, 9, "%H:%M:%S", loc_time_p);

        wmove(win,line_no,(MAXCOL - 26)/2);
        wprintw(win,"Last Stats Update: %s",timeret);

        line_no++;

        wmove(win,line_no,(MAXCOL - 26)/2);
        wprintw(win,"---------------------------");

        line_no++;
        line_no++;

        wmove(win,line_no,2);
        clrtoeol();
        wprintw(win,"Bytes read         : %8d",
                tnicon_p->conn_read_byte_count_dis[conn_num]);

        wmove(win,line_no,49);
        clrtoeol();
        wprintw(win,"Bytes written      : %8d",
                tnicon_p->conn_write_byte_count_dis[conn_num]);

        line_no++;

        wmove(win,line_no,2);
        clrtoeol();
        wprintw(win,"Read errors        : %8d",
                tnicon_p->conn_read_err_count_dis[conn_num]);

        wmove(win,line_no,49);
        clrtoeol();
        wprintw(win,"Write errors       : %8d",
                tnicon_p->conn_write_err_count_dis[conn_num]);

        wmove(win,23,0);
        wprintw(win,"    CONFG, PDUS, EXIT, MENU");
    }

    wmove(win,25,0);
    wprintw(win,"Selection: ");

    refresh();
    wrefresh(win);

    wmove(win,25,11);

    input_y = 25;
    input_x = 11;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Conn_Input(char *command, int par)                                         */
/*                                                                            */
/* Purpose: This function processes all the commands specific to the          */
/*          connection screens                                         .      */
/*                                                                            */
/* Input Arguments:                                                           */
/*          command          string portion of the command entered            */
/*          par              integer command or integer parameter accompaning */
/*                           command (optional)                               */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: par is -1 when command is entered from summary screen         */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void
Conn_Input(char *command, int par)
{

    int status;

/*  Detailed screen commands                                                  */

    if (conn_num != -1)
    {
        if (*command == '\0')
        {
            if ((par > 0) && (par <= MAX_CONN))
            {
                conn_num = par;
            }
        }
        else if ((strcasecmp(command, "CON")) == 0)
        {
            if (((par > 0) && (par <= MAX_CONN)) ||
                (par == -1))
            {
                conn_num = par;
            }
        }
        else if ((strcasecmp(command, "CLOSE")) == 0 )
        {
            status = Generate_Vision_Cmd(CLOSE_CLIENT_CONN,
                                         conn_num,
                                         0,
                                         NULL,
                                         NULL);
            Display_Cmd_Result(status);
        }
        else if ((strcasecmp(command, "CONFG")) == 0 )
        {
            confg_details = 1;
            pdus_details = 0;
            stats_details = 0;

            wclear(win);
	}
        else if ((strcasecmp(command, "PDUS")) == 0 )
        {
            confg_details = 0;
            pdus_details = 1;
            stats_details = 0;

            wclear(win);
        }
        else if ((strcasecmp(command, "STATS")) == 0 )
        {
            confg_details = 0;
            pdus_details = 0;
            stats_details = 1;

            wclear(win);
	}

/*  Summary screen commands                                                   */

    }
    else
    {
        page = PAGE_NEUTRAL;

        if (*command == '\0')
        {
            if ((par > 0) && (par <= MAX_CONN))
            {
                conn_num = par;
            }

        }
        else if ((strcasecmp(command, "F")) == 0)
        {
            page = PAGE_FORWARD;
        }
        else if ((strcasecmp(command, "B")) == 0 )
        {
            page = PAGE_BACKWARD;
        }
        else if ((strcasecmp(command, "CON")) == 0)
        {
            if (((par > 0) && (par <= MAX_CONN)) ||
                (par == -1))
            {
                conn_num = par;
            }
        }
    }
}

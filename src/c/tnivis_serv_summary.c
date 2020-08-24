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
/*====[TNIVIS_SERV_SUMMARY.C]=================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Serv_Summary()                                                    */
/*          Serv_Summary_Input(char *command, int par)                        */
/*                                                                            */
/*====[TNIVIS_SERV_SUMMARY.C]=================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Serv_Summary()                                                             */
/*                                                                            */
/* Purpose: This function displays the server summary snapshot.               */
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

void Serv_Summary() {

    char *tp;

    TNICON  *pp;
    TERM_STATS *term_stats_p;
    TERM_STATS *disp_term_stats_p;

    int host_id_idx;
    int st,line_no;
    int term_stats_idx;
    int tot_outstdg_term_cnt;
    int read_util_percentage;

    short int timelen;
    char timeret[9];
    char comm_mode[9];

    struct tm loc_time;
    struct tm *loc_time_p;

    loc_time_p = &loc_time;

    pp = tnicon_p;

    switch (pp->comm_mode) {

    case COMM_DISABLED:

        sprintf(comm_mode,"Disabled     ");
        break;

    case COMM_ENABLED:

        sprintf(comm_mode,"Enabled      ");
        break;

    case COMM_SHUTDOWN:

        sprintf(comm_mode,"Shutdown     ");
        break;

    case COMM_SYNC:

        sprintf(comm_mode,"Synchronizing");
        break;
    }

/*   snap shut title                                                          */

    wmove(win,0,0);
    wprintw(win,"MX SERVER SUMMARY SNAPSHOT");

    wmove(win,0, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );


    /* left column */


    line_no=2;
    wmove(win,line_no,2);
    wprintw(win,"Communication mode        : %s",comm_mode);

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Primary server notify mode: %d",pp->prim_notify_mode);

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Exchange Encryption Key   : %s",
            encryption_states[pp->exchange_enc_key_state]);

    line_no++;
    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Interval timers (sec) "); 

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"--------------------------"); 

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Connection check          : %d", pp->check_conn_invl);

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Takeover                  : %d", pp->takeover_invl);

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Statistics                : %d", pp->statistics_invl);

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"ES RPC request timeout    : %d", pp->def_rpc_rqst_timeout);

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Auth Lockout interval     : %d", pp->auth_lockout_invl);

    loc_time_p = localtime(&pp->last_stats_time.time);
    strftime(timeret, 9, "%H:%M:%S", loc_time_p);

    line_no++;
    line_no++;

    wmove(win,line_no,2);
    wprintw(win,"Last Stats Update         : %s", timeret);

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"--------------------------"); 

    if (pp->read_select_count_dis != 0) {

        read_util_percentage =(int)(
            ((float)pp->read_select_success_count_dis/
                    pp->read_select_count_dis)*100);
    } else {

        read_util_percentage = 0;
    }

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Read utilization          :  %3d%%", read_util_percentage);

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Outbound application msgs : %4d", 
            pp->oltp_msgs_from_game_dis +
            pp->rpc_msgs_from_game);

/*  Get pointer to statsistics global section                                 */

    term_stats_p = &tstats_p->term_stats;

    tot_outstdg_term_cnt = 0;

    for (term_stats_idx = 1;
         term_stats_idx <= max_terminal;
         term_stats_idx++)
    {

/*  Get pointer to this terminal's information in global section            */

        disp_term_stats_p = &term_stats_p[term_stats_idx];

        if (disp_term_stats_p->trans_in_game_cnt > 1)
        {
            tot_outstdg_term_cnt++;
        }
    }

    line_no++;
    wmove(win,line_no,2);
    wprintw(win,"Outstanding terms cnt     : %4d", 
            tot_outstdg_term_cnt);

    line_no++;

    /* right column */

    line_no=2;
    wmove(win,line_no,45);
    wprintw(win,"Maximum ES RPC requests   : %d",pp->max_es_requests);

    line_no++;
    wmove(win,line_no,45);
    wprintw(win,"TNI Protocol Version      : %d.%d",
            (DEFAULT_TNI_VERSION & 0xf0) >> 4,
            (DEFAULT_TNI_VERSION & 0x0f));

    line_no++;
    wmove(win,line_no,45);
    wprintw(win,"Auth Failure Threshold    : %d", pp->auth_failure_threshold);

    line_no++;
    line_no++;
    wmove(win,line_no,45);
    wprintw(win,"Interval timers (msec) ");

    line_no++;
    wmove(win,line_no,45);
    wprintw(win,"--------------------------"); 

    line_no++;
    wmove(win,line_no,45);
    wprintw(win,"Read attempt interval     : %d", 
            pp->read_atmpt_invl*10); 

    line_no++;
    wmove(win,line_no,45);
    wprintw(win,"Rd/Wrt timeout interval   : %d",
            pp->rd_wrt_tout_invl*10);

    line_no++;
    wmove(win,line_no,45);
    wprintw(win,"Target cycle rate         : %d",pp->cycle_rate);

    line_no++;
    wmove(win,line_no,45);

    switch (tnicon_p->wrt_select_tout_invl) {
    
    case -1:
        wprintw(win,"Wrt select timeout        : .001");
        break;
	
    case 0:
        wprintw(win,"Wrt select timeout        : N/A");
            break;
	    
    case 999:
        wprintw(win,"Wrt select timeout        : Block");
            break;
	    
    default:
        wprintw(win,"Wrt select timeout        : %d", tnicon_p->wrt_select_tout_invl);
        break;
    }

    line_no++;
    line_no++;

    line_no++;        
    wmove(win,line_no,45);
    wprintw(win,"OLTP response times (msec) ");

    line_no++;
    wmove(win,line_no,45);
    wprintw(win,"--------------------------"); 

    line_no++;
    wmove(win,line_no,45);
    clrtoeol();
    wprintw(win,"Minimum response : %7d", pp->min_oltp_resp_time_dis);

    line_no++;
    wmove(win,line_no,45);
    clrtoeol();
    wprintw(win,"Maximum response : %7d", pp->max_oltp_resp_time_dis);

    line_no++;
    wmove(win,line_no,45);
    clrtoeol();
    wprintw(win,"Average response : %7d", pp->ave_oltp_resp_time_dis);

    wmove(win,21,0);
    wprintw(win,"    MODE #, OVERRIDE, EXIT, MENU");
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
/* Serv_Summary_Input(char *command, int par)                                 */
/*                                                                            */
/* Purpose: This function processes all the commands specific to the          */
/*          server summary snapshot.                                          */
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
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Serv_Summary_Input(char *command, int par) {

    int status;

    if ((strcasecmp(command, "MODE")) == 0 ) {

        if (par != -1) {

            status = Generate_Vision_Cmd(CHANGE_COMM_MODE,
                                         par,
                                         0,
                                         NULL,
                                         NULL);
            Display_Cmd_Result(status);

        } else {

            Display_Cmd_Result(INV_ARG_INT1);
        }
    } else if ((strcasecmp(command, "OVERIDE")) == 0 ) {

        status = Generate_Vision_Cmd(OVERIDE_PRIM_NOTIFY,
                                     0,
                                     0,
                                     NULL,
                                     NULL);
        Display_Cmd_Result(status);
    }
    return;
}

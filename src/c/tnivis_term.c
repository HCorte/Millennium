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
/*====[TNIVIS_TERM.C]=========================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Term_Serv()                                                       */
/*          Term_Serv_Input(char *command, int par)                           */
/*                                                                            */
/*====[TNIVIS_TERM.C]=========================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

#define MAX_DISPLAYABLE_LINES 12

static unsigned long int terminal_num = 1;     /* Terminal number             */
static int term_stats_idx = -1;
static int page;

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Term_Serv()                                                                */
/*                                                                            */
/* Purpose: This function displays the terminal server is snapshot.           */
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

void Term_Serv() {

    unsigned int value;

    ubyte_4 rqst_msg_count;
    ubyte_4 resp_msg_count;
    ubyte_4 unso_msg_count;
    ubyte_4 host_rqst_msg_count;
    ubyte_4 host_resp_msg_count;
    ubyte_4 retry_count;

    int client_idx = 0;
    int clt_con = 0;
    int next_idx, hist_idx, input, ln, next, numparam, prmidx;
    int line_no;
    int last_left;
    int last_minute_idx;
    int tra_count_current;
    int status;
    int idx;                              /* Host ID index                    */
    int disp_cnt;
    int something_to_display;
    int index1;
    int host_id_idx;

    static unsigned long int last_disp_term_num = 0; /* Terminal number       */

    static int first_disp_app_idx = 0;
    static int last_disp_app_idx = 0;

    char *cp;
    struct INTERVAL_STATS *sp;
    /* the Host ID index                  */
    static char term_client_tag[MAX_TERM_CLIENT_TAG_LEN];

    unsigned char host_id_val;            /* Host ID value                    */
    TERM_STATS *term_stats_p;             /* Pointer to the statistics area   */
                                          /* for each network terminal        */

    if( last_disp_term_num != terminal_num ) {

        wclear(win);

        last_disp_term_num = terminal_num;
        first_disp_app_idx = 0;
        last_disp_app_idx = 0;
    }

    if (page != PAGE_NEUTRAL) {

        wclear(win);
    }

/*   snapshot title                                                           */

    wmove(win,0,0);
    wprintw(win,"TERMINAL SERVER ID SNAPSHOT");

    wmove(win,0, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    /* left column */

    line_no=2;
    wmove(win,line_no,2);

    if (term_stats_idx == -1)
    {
        wprintw(win,"Terminal Server Id (%d) UNKNOWN!",terminal_num);
    }
    else
    { 
        wprintw(win,"Terminal Server Id  : %d",terminal_num);

        term_stats_p = &tstats_p->term_stats;
        term_stats_p = &term_stats_p[term_stats_idx];

#       if defined(PROSYS_ENV_ALL)

            if (term_stats_p->trans_in_game_cnt <= 1)
            {
                retry_count = 0;
            }
            else
            {
                retry_count = term_stats_p->trans_in_game_cnt - 1;
            }

            wmove(win,line_no,45);
            wprintw(win,"Retry count: %5d  ",retry_count);

#       endif


        line_no++;
        line_no++;

#       if defined(PROSYS_ENV_ALL)

            wmove(win,line_no,2);
            wprintw(win,"Application  Rqst Msgs  Resp Msgs  Unso Msgs  Rqst Msgs  Resp Msgs");

            line_no++;
            wmove(win,line_no,2);
            wprintw(win,"   Name        Rcvd       Sent       Sent       Sent       Rcvd");

            line_no++;
            wmove(win,line_no,2);
            wprintw(win,"------------------------------------------------------------------");
#       endif

#       if defined(GOLS_ENV_ALL)

            wmove(win,line_no,2);
            wprintw(win,"%6s  %8s  %14s  %9s  %8s",
                    "Client","App msgs",
                    "Solicited msgs","Unso msgs","Bro msgs"); 

            line_no++;
            wmove(win,line_no,2);
            wprintw(win,"%6s  %8s  %14s  %9s  %8s",
                    " Id","rcvd","sent","sent","sent");
#endif

        switch (page)
        {
        case PAGE_FORWARD:

             if (last_disp_app_idx != MAX_APPS + MAX_NUM_CLIENT_IDS)
             {
                 first_disp_app_idx = last_disp_app_idx;
             }

             page = PAGE_NEUTRAL;
             break;

        case PAGE_BACKWARD:

             if ((first_disp_app_idx - MAX_DISPLAYABLE_LINES) > 0)
             {
                 first_disp_app_idx = first_disp_app_idx -
                                      MAX_DISPLAYABLE_LINES;
             }
             else
             {
                 first_disp_app_idx = 0;
             }

             page = PAGE_NEUTRAL;
             break;
        }

        idx = first_disp_app_idx;
        disp_cnt = 0;
        something_to_display = 1;

        while ((disp_cnt < MAX_DISPLAYABLE_LINES) &&
               (something_to_display) &&
               (terminal_num != 0 ))
        {

            if (idx < MAX_APPS)
            {
                if (tnicon_p->app[idx].state == APP_DEFINED)
                {
                    rqst_msg_count = term_stats_p->app_request_tot[idx];
                    resp_msg_count = term_stats_p->app_response_tot[idx];
                    unso_msg_count = term_stats_p->app_unso_tot[idx];
                    host_rqst_msg_count = term_stats_p->host_term_rqst_tot[idx];
                    host_resp_msg_count = term_stats_p->host_term_resp_tot[idx];

                    if ((rqst_msg_count != 0) ||
                        (resp_msg_count != 0) ||
                        (unso_msg_count != 0) ||
                        (host_rqst_msg_count != 0) ||
                        (host_resp_msg_count != 0))
                    {
                        line_no++;
                        wmove(win,line_no,2);
                        wprintw(win,"%-11s  ",tnicon_p->app[idx].name);

                        wmove(win,line_no,16);
                        wprintw(win,"%7d  ",rqst_msg_count);

                        wmove(win,line_no,27);
                        wprintw(win,"%7d  ",resp_msg_count);

                        wmove(win,line_no,38);
                        wprintw(win,"%7d  ",unso_msg_count);

                        wmove(win,line_no,49);
                        wprintw(win,"%7d  ",host_rqst_msg_count);

                        wmove(win,line_no,60);
                        wprintw(win,"%7d  ",host_resp_msg_count);

                        disp_cnt++;
                    }
                }
            }
            else
            {
                client_idx = idx - MAX_APPS;

                if (tnicon_p->client_idx_tbl[client_idx] != 0)
                {
                    rqst_msg_count = term_stats_p->app_request_tot[idx];
                    resp_msg_count = term_stats_p->app_response_tot[idx];
                    unso_msg_count = term_stats_p->app_unso_tot[idx];
                    host_rqst_msg_count = term_stats_p->host_term_rqst_tot[idx];
                    host_resp_msg_count = term_stats_p->host_term_resp_tot[idx];

                    if ((rqst_msg_count != 0) ||
                        (resp_msg_count != 0) ||
                        (unso_msg_count != 0) ||
                        (host_rqst_msg_count != 0) ||
                        (host_resp_msg_count != 0))
                    {
                        line_no++;
                        wmove(win,line_no,2);

                        clt_con = tnicon_p->client_conns[client_idx][0];

                        if (tnicon_p->connection[clt_con].tni_proto_ver ==
                            TNI_VERSION_01)
                        {
                            wprintw(win,
                                    "Clt Id %d",
                                    tnicon_p->client_idx_tbl[client_idx]);
                        }
                        else
                        {
                            if (tnicon_p->connection[clt_con].application_name
                                == NULL)
                            {
                                wprintw(win,
                                        "Clt Id %d",
                                        tnicon_p->client_idx_tbl[client_idx]);
                            }
                            else
                            {
                                wprintw(win,
                                        "%-11s",
                                tnicon_p->connection[clt_con].application_name);
                            }
                        }

                        wmove(win,line_no,16);
                        wprintw(win,"%7d  ",rqst_msg_count);

                        wmove(win,line_no,27);
                        wprintw(win,"%7d  ",resp_msg_count);

                        wmove(win,line_no,38);
                        wprintw(win,"%7d  ",unso_msg_count);

                        wmove(win,line_no,49);
                        wprintw(win,"%7d  ",host_rqst_msg_count);

                        wmove(win,line_no,60);
                        wprintw(win,"%7d  ",host_resp_msg_count);

                       disp_cnt++;
                    }
                }
            } 
            idx++;

            if (idx == MAX_APPS + MAX_NUM_CLIENT_IDS)
            {
                something_to_display = 0;
            }
        }                                   /* end for                            */
        last_disp_app_idx = idx;
    }
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
/* Term_Serv_Input(char *command, int par)                                    */
/*                                                                            */
/* Purpose: This function processes all the commands specific to the          */
/*          terminal server id snapshot.                                      */
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

void Term_Serv_Input(char *command, int par) {

    int temp_term_stats_idx = -1;

    page = PAGE_NEUTRAL;

    if ((strcasecmp(command, "F")) == 0) {

        page = PAGE_FORWARD;
    } else if ((strcasecmp(command, "B")) == 0 ) {

        page = PAGE_BACKWARD;
    } else {

#       if defined(PROSYS_ENV_PLATFORM)

            terminal_num = par;

            temp_term_stats_idx = retrieve_term_stats_idx((ubyte_4) par);

            if (temp_term_stats_idx != -1)
            {
                term_stats_idx = temp_term_stats_idx;
            }
            else
            {
                term_stats_idx = -1;
            }

#       else

            terminal_num = par;

            if ((par > 0) && (par <= max_terminal))
            {
                term_stats_idx = par;
            }
            else
            {
                term_stats_idx = -1;
            }

#       endif

    }
}

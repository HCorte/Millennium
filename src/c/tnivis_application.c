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
/*====[TNIVIS_APPLICATION.C]==================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Application()                                                     */
/*          App_Summary()                                                     */
/*          App_Detailed()                                                    */
/*          App_Input(char *command, int par)                                 */
/*                                                                            */
/*====[TNIVIS_APPLICATION.C]==================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

#define MAX_DISPLAYABLE_LINES 14
#define MAX_DISPLAY_DETAILED 10

static int app_num_val;
static int page;

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Application()                                                              */
/*                                                                            */
/* Purpose: This function determines which application snapshot screens to    */
/*          display.  If no application number is entered with the app        */
/*          command then the summary screen is displayed.  If the app command */
/*          is accompanied with an a pplication number, then the detailed     */
/*          screen is displayed.                                              */
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

void Application() {

    static int last_app_num_val = 0;

    if( last_app_num_val != app_num_val ) {

        wclear(win);
        last_app_num_val = app_num_val;
    }

    if (page != PAGE_NEUTRAL) {

        wclear(win);
    }

    if (app_num_val != -1) {

        App_Detailed();
    } else {

        App_Summary();
    }

    return;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* App_Detailed()                                                             */
/*                                                                            */
/* Purpose: This function displays the application detailed screen.           */
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

void App_Detailed() {

    int conn_idx = 0;
    int conn_num = 0;
    int n = 0;
    int defined_conns = 0;
    int line_no = 0 ;
    int disp_cnt;
    int something_to_display;

    static int first_disp_conn_idx = 0;
    static int last_disp_conn_idx = 0;


    wmove(win,line_no,0);
    wprintw(win,"MX APPLICATION (%d) SNAPSHOT ",
            app_num_val);

    wmove(win,line_no, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    line_no++;
    line_no++;

    if (tnicon_p->app[app_num_val].state == APP_UNDEFINED)
    {
        wmove(win,line_no,5);
        wprintw(win,"Application is undefined" );
    }
    else
    {

        wmove(win,line_no,5);
        wprintw(win,"Application Name: %s",
                tnicon_p->app[app_num_val].name);

        line_no++;
        line_no++;

        wmove(win,line_no,5);
        wprintw(win,"State         : %s",
                app_states[tnicon_p->app[app_num_val].state]);

        wmove(win,line_no,42);
        wprintw(win,"OLTP Unso Routing      : %s",
                oltp_unso_route[tnicon_p->app[app_num_val].oltp_unso_routing]);

        line_no++;

        defined_conns = tnicon_p->app[app_num_val].client_conn_cnt;

        wmove(win,line_no,5);
        wprintw(win,"Defined Conns : %d",
                defined_conns);

        wmove(win,line_no,42);
        wprintw(win,"OLTP Rqst Routing      : %s",
                oltp_unso_route[tnicon_p->app[app_num_val].oltp_rqst_routing]);

        line_no++;

        wmove(win,line_no,5);
        wprintw(win,"Hash Algorithm: %s",
                hash_algorithms[tnicon_p->app[app_num_val].auth_hash_algorithm]);

        wmove(win,line_no,42);
        wprintw(win,"ES RPC request timeout : %d (sec)",
                tnicon_p->app[app_num_val].rpc_rqst_timeout);

        line_no++;

        wmove(win,line_no,5);
	wprintw(win,"Authentication Required: %s",
		(tnicon_p->app[app_num_val].auth_required?"True":"False"));
		
        wmove(win,line_no,42);
        wprintw(win,"Authentication Locked  : %s",
                ((tnicon_p->app[app_num_val].auth_lockout_state == APP_AUTH_LOCKED)?"Yes":"No"));

        line_no++;

        wmove(win,line_no,5);
        wprintw(win,"ES RPC Encryption Mode : %s",
                encryption_states[tnicon_p->app[app_num_val].es_rpc_enc_mode]);

        wmove(win,line_no,42);
        wprintw(win,"ES RPC Encryption State: %s",
                encryption_states[tnicon_p->app[app_num_val].es_rpc_enc_state]);

        if (defined_conns > 0)
        {
            switch (page)
            {
                case PAGE_FORWARD:

                    if (last_disp_conn_idx != defined_conns)
                    {
                        first_disp_conn_idx = last_disp_conn_idx;
                    }

                    page = PAGE_NEUTRAL;
                    break;

                case PAGE_BACKWARD:

                    if ((last_disp_conn_idx - 2) > -1)
                    {
                        first_disp_conn_idx = last_disp_conn_idx - 2;
                    }
                    else
                    {
                        first_disp_conn_idx = last_disp_conn_idx - 1;
                    }
                    page = PAGE_NEUTRAL;
                    break;
            }

            conn_idx = first_disp_conn_idx;
            disp_cnt = 0;
            something_to_display = 1;

            line_no++; 
            line_no++; 

            for (n = 5; n < MAXCOL; n++ )
            {
                wmove(win,line_no,n);
                wprintw(win,"-");
            }

            while ((disp_cnt < MAX_DISPLAY_DETAILED) &&
                   (something_to_display))
            {
                line_no++;
                line_no++;

                conn_num = tnicon_p->app[app_num_val].client_conn[conn_idx];

                wmove(win,line_no,5);
                wprintw(win,"Client domain name   : %.51s",
                        tnicon_p->connection[conn_num].remote_domain_name);

                line_no++;

                wmove(win,line_no,5);
                wprintw(win,"Connection number    : %d",conn_num);

                wmove(win,line_no,45);
                wprintw(win,"Connection state    : %s",
                        states[tnicon_p->connection[conn_num].conn_state]);

                line_no++;

                wmove(win,line_no,5);
                clrtoeol();

                if (tnicon_p->connection[conn_num].session_tag[0] == '\0')
                {
                    wprintw(win,"Session Id           : ---------");
                }
                else
                {
                    wprintw(win,"Session Id           : %s",
                            tnicon_p->connection[conn_num].session_tag);
                }

                line_no++;

                wmove(win,line_no,5);
                wprintw(win,"Authentication state : %s",
                        auth_states[tnicon_p->connection[conn_num].auth_state]);

                wmove(win,line_no,45);
                wprintw(win,"Exchange Key Method : %s",
                        exchange_methods[tnicon_p->connection[conn_num].enc_key_x_method]);

                line_no++;

                wmove(win,line_no,5);
                wprintw(win,"Encryption Key Type  : %s",
                        encryption_key_types[tnicon_p->connection[conn_num].enc_key_type]);

                wmove(win,line_no,45);
                wprintw(win,"Encryption State    : %s",
                        encryption_states[tnicon_p->connection[conn_num].enc_state]);

                disp_cnt+= 6;
                conn_idx++;

                if (conn_idx == defined_conns)
                {
                    something_to_display = 0;
                }
                else  /* Only display one connection per page because more than one will not fit */
                {
                    something_to_display = 0;
                }
            }
            last_disp_conn_idx = conn_idx;

            while (disp_cnt < MAX_DISPLAY_DETAILED)
            {
                line_no++;
                wmove(win,line_no,1);
                clrtoeol();
                disp_cnt++;
            }
        }
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

    return;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* App_Summary()                                                              */
/*                                                                            */
/* Purpose: This function displays the application summary screen.            */
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

void App_Summary() {

    int line_no = 0 ;
    int idx;
    int conn;
    int conn_idx;
    int id_val;
    int num_defined_conns;
    int num_active_conns;
    int disp_cnt;
    int something_to_display;

    static int first_disp_app_idx = 0;
    static int last_disp_app_idx = 0;

    wmove(win,line_no,0);
    wprintw(win,"MX APPLICATION SUMMARY SNAPSHOT ");

    wmove(win,line_no, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    line_no++;
    line_no++;

    wmove(win,line_no,5);
    wprintw(win,"Number  Name         Connections");

    line_no++;
    wmove(win,line_no,5);
    wprintw(win,"                       Def/Act");

    line_no++;
    wmove(win,line_no,5);
    wprintw(win,"-----------------------------------------------------------");

    switch (page)
    {
        case PAGE_FORWARD:

             if (last_disp_app_idx != MAX_APPS) {

                 first_disp_app_idx = last_disp_app_idx;
             }

             page = PAGE_NEUTRAL;
             break;

        case PAGE_BACKWARD:

             if ((first_disp_app_idx - MAX_DISPLAYABLE_LINES) > 0) {

                 first_disp_app_idx = first_disp_app_idx -
                                      MAX_DISPLAYABLE_LINES;
             } else {
                 first_disp_app_idx = 0;
             }

             page = PAGE_NEUTRAL;
             break;
    }

    idx = first_disp_app_idx;
    disp_cnt = 0;
    something_to_display = 1;

    while ((disp_cnt < MAX_DISPLAYABLE_LINES) &&
           (something_to_display))
    {
        if (tnicon_p->app[idx].state == APP_DEFINED)
        {
            line_no++;
            wmove(win,line_no,6);
            wprintw(win,"%3d", idx);

            wmove(win,line_no,13);
            wprintw(win,"%.11s",
                    tnicon_p->app[idx].name);
 
            num_defined_conns = tnicon_p->app[idx].client_conn_cnt;
            num_active_conns = 0;

            for (conn_idx = 0; conn_idx < num_defined_conns; conn_idx++)
            {
                conn = tnicon_p->app[idx].client_conn[conn_idx];

                if (tnicon_p->connection[conn].conn_state >= 
                    CONN_CONNECTED) {

                    num_active_conns++;
                }
            }

            wmove(win,line_no,28);
            wprintw(win,"%3d/%3d",num_defined_conns, num_active_conns);

            disp_cnt++;    
        }
        idx++;

        if (idx == MAX_APPS) {
            something_to_display = 0;
        }
    }

    last_disp_app_idx = idx;

    while (disp_cnt < MAX_DISPLAYABLE_LINES)
    {
        line_no++;
        wmove(win,line_no,1);
        clrtoeol();
        disp_cnt++;
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
/* App_Input(char *command, int par)                                          */
/*                                                                            */
/* Purpose: This function processes all the commands specific to the          */
/*          application screens.                                              */
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
/* Assumptions: app_num_val is -1 when command is entered from summary screen */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void App_Input(char *command, int par) {

    int status;

/*  Detailed screen commands                                                  */

    if (app_num_val != -1)
    {
        if (*command == '\0')
        {
            if ((par >= 0) && (par < MAX_APPS))
            {
                app_num_val = par;
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
        else if ((strcasecmp(command, "APP")) == 0)
        {
            if (((par >= 0) && (par < MAX_APPS)) ||
                (par == -1))
            {
                app_num_val = par;
            }
        }

/*  Summary screen commands                                                   */

    }
    else
    {
        page = PAGE_NEUTRAL;

        if (*command == '\0')
        {
            if ((par >= 0) && (par < MAX_APPS))
            {
                app_num_val = par;
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
        else if ((strcasecmp(command, "APP")) == 0)
        {
            if (((par >= 0) && (par < MAX_APPS)) ||
                (par == -1))
            {
                app_num_val = par;
            }
        }
    }
}

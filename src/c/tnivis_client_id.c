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
/*====[TNIVIS_CLIENT_ID.C]====================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Client_Id()                                                       */
/*          Client_Id_Summary()                                               */
/*          Client_Id_Detailed()                                              */
/*          Client_Id_Input(char *command, int par)                           */
/*                                                                            */
/*====[TNIVIS_CLIENT_ID.C]====================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

#define MAX_DISPLAYABLE_LINES 14
#define MAX_DISPLAY_DETAILED 18

static int client_id_val;
static int page;

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Client_Id()                                                                */
/*                                                                            */
/* Purpose: This function determines which client snapshot screens to         */
/*          display.  If no client id is entered with the client command,     */
/*          then the summary screen is displayed.  If the client command is   */
/*          accompanied with a client id, then the detailed screen is         */
/*          displayed.                                                        */
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

void
Client_Id ()
{

    static int last_client_id_val = 0;

    if( last_client_id_val != client_id_val ) {

        wclear (win);
        last_client_id_val = client_id_val;
    }

    if (page != PAGE_NEUTRAL) {

        wclear (win);
    }

    if (client_id_val != -1) {

        Client_Id_Detailed ();
    } else {

        Client_Id_Summary ();
    }

    return;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Client_Id_Detailed()                                                       */
/*                                                                            */
/* Purpose: This function displays the client id detailed screen.             */
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

void
Client_Id_Detailed()
{
    int conn_idx = 0;
    int conn_num = 0;
    int i, n;
    int defined_conns_per_client_id;
    int line_no = 0 ;
    int client_id_idx;
    int disp_cnt;
    int something_to_display;

    static int first_disp_conn_idx = 0;
    static int last_disp_conn_idx = 0;

/*  Since there is currently no mapping between host id index and client id   */
/*  value, determine the client id index by searching all of the defined      */
/*  connections until we find the client id value                             */

    client_id_idx = Validate_Client_Id (client_id_val);

    wmove (win, line_no, 0);
    wprintw (win, "MX CLIENT ID SNAPSHOT ");

    wmove (win, line_no, (MAXCOL - strlen(Get_Time())));
    clrtoeol ();
    wprintw (win, "%s", Get_Time ());

    line_no++;
    line_no++;

    wmove (win, line_no, 5);
    wprintw (win, "Client Id: %d", client_id_val);

    defined_conns_per_client_id = 0;

    /*  No client id index found for the input client id value                */

    if (client_id_idx == -1)
    {
        line_no++;

        wmove (win, line_no, 5);
        wprintw (win, "No connections defined ");

        line_no++;

    }
    else
    {
        for (i = 0; i < MAX_CONNS_PER_CLIENT; i++)
        {
            conn_num = tnicon_p->client_conns[client_id_idx][i];

            if (tnicon_p->connection[conn_num].conn_state >= CONN_DEFINED)
            {
                defined_conns_per_client_id++;
            }
        }
    }

    if (defined_conns_per_client_id > 0)
    {
        switch (page)
        {
            case PAGE_FORWARD:

                if (last_disp_conn_idx != defined_conns_per_client_id)
                {
                    first_disp_conn_idx = last_disp_conn_idx;
                }

                page = PAGE_NEUTRAL;
                break;

            case PAGE_BACKWARD:

                if ((first_disp_conn_idx - MAX_DISPLAY_DETAILED) > 0)
                {
                    first_disp_conn_idx = first_disp_conn_idx -
                                          MAX_DISPLAY_DETAILED;
                }
                else
                {
                    first_disp_conn_idx = 0;
                }

                page = PAGE_NEUTRAL;
                break;
        }

        conn_idx = first_disp_conn_idx;
        disp_cnt = 0;
        something_to_display = 1;

        wmove (win, line_no, 45);
        wprintw (win, "Defined Conns : %d",
                 defined_conns_per_client_id);

        line_no++;
        line_no++;

        for (n = 5; n < MAXCOL; n++)
        {
            wmove (win, line_no, n);
            wprintw (win, "-");
        }

        while ((disp_cnt < MAX_DISPLAY_DETAILED) &&
               (something_to_display))
        {
            line_no++;
            line_no++;

            conn_num = tnicon_p->client_conns[client_id_idx][conn_idx];

            wmove (win, line_no, 5);
            wprintw (win, "Connection number    : %d",conn_num);

            wmove (win, line_no, 45);
            wprintw (win, "Connection state: %s",
                     states[tnicon_p->connection[conn_num].conn_state]);

            line_no++;

            wmove (win, line_no, 5);
            wprintw (win, "Alternate connection : %d",
                     tnicon_p->connection[conn_num].alt_conn);

            line_no++;

            wmove (win, line_no, 5);
            wprintw (win, "Local domain name    : %.51s",
                     tnicon_p->connection[conn_num].local_domain_name);

            line_no++;

            wmove (win, line_no, 5);
            wprintw (win, "Client domain name   : %.51s",
                     tnicon_p->connection[conn_num].remote_domain_name);

            line_no++;

            wmove (win, line_no, 5);
            wprintw (win, "Client node name     : %.51s",
                     tnicon_p->connection[conn_num].remote_node_name);

            disp_cnt+= 6;
            conn_idx++;

            if (conn_idx == defined_conns_per_client_id)
            {
                something_to_display = 0;
            }
        }

        last_disp_conn_idx = conn_idx;
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
/* Client_Id_Summary()                                                        */
/*                                                                            */
/* Purpose: This function displays the client id summary screen.              */
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

void
Client_Id_Summary ()
{
    int line_no = 0;
    int i;
    int idx;
    int conn;
    int n_conn;
    int client_id_idx;
    int client_id;
    int num_defined_conns;
    int num_active_conns;
    int disp_cnt;
    int something_to_display;

    static int first_disp_client_idx = 0;
    static int last_disp_client_idx = 0;

    wmove (win, line_no, 0);
    wprintw (win, "MX CLIENT ID SUMMARY SNAPSHOT ");

    wmove (win, line_no, (MAXCOL - strlen (Get_Time ())));
    clrtoeol ();
    wprintw (win, "%s", Get_Time());

    line_no++;
    line_no++;

    wmove (win, line_no, 5);
    wprintw (win, "Client  Connections  Node Name  Messaging  Application");

    line_no++;
    wmove (win, line_no, 5);
    wprintw (win, "  Id      Def/Act                 Type        Name");

    line_no++;
    wmove (win, line_no, 5);
    wprintw (win, "-----------------------------------------------------------");

    switch (page)
    {
        case PAGE_FORWARD:

             if (last_disp_client_idx != MAX_NUM_CLIENT_IDS)
             {
                 first_disp_client_idx = last_disp_client_idx;
             }

             page = PAGE_NEUTRAL;
             break;

        case PAGE_BACKWARD:

             if ((first_disp_client_idx - MAX_DISPLAYABLE_LINES) > 0)
             {
                 first_disp_client_idx = first_disp_client_idx -
                                         MAX_DISPLAYABLE_LINES;
             }
             else
             {
                 first_disp_client_idx = 0;
             }

             page = PAGE_NEUTRAL;
             break;
    }

    idx = first_disp_client_idx;
    disp_cnt = 0;
    something_to_display = 1;

    while ((disp_cnt < MAX_DISPLAYABLE_LINES) &&
           (something_to_display))
    {
        client_id = tnicon_p->sorted_client_ids[idx];

        if (client_id != 0)
        {
            line_no++;
            wmove (win, line_no, 6);
            wprintw (win, "%3d", client_id);
        
            num_defined_conns = 0;
            num_active_conns = 0;
            n_conn = 0;

            client_id_idx = Validate_Client_Id (client_id);

            for (i = 0; i < MAX_CONNS_PER_CLIENT; i++)
            {
                conn = tnicon_p->client_conns[client_id_idx][i];

                if (tnicon_p->connection[conn].conn_state >= CONN_DEFINED)
                {
                    n_conn = conn;
                    num_defined_conns++;
                }

                if (tnicon_p->connection[conn].conn_state >= CONN_CONNECTED)
                {
                    num_active_conns++;
                }
            }

            wmove (win, line_no, 15);
            wprintw (win, "%3d/%3d", num_defined_conns, num_active_conns);

            wmove (win, line_no, 26);
            wprintw (win, "%.10s",
                     tnicon_p->connection[n_conn].remote_node_name);

            if (num_active_conns > 0)
            {
                client_id_idx = tnicon_p->connection[n_conn].client_id_idx;

                wmove (win, line_no, 39);
                wprintw (win, "%s",
                        msg_types[tnicon_p->connection[n_conn].messaging_type]);

                if (tnicon_p->connection[n_conn].tni_proto_ver ==
                    TNI_VERSION_01)
                {
                    wmove (win, line_no, 48);
                    wprintw (win, "%-15s", "---------------");
                }
                else
                {
                    if (tnicon_p->connection[n_conn].application_name == 
                        NULL)
                    {
                        wmove (win, line_no, 48);
                        wprintw (win, "%-15s", "---------------");
                    }
                    else
                    {
                    wmove (win, line_no, 48);
                    wprintw (win, "%-15s", 
                             tnicon_p->connection[n_conn].application_name);
                    }
                }
            }
            else
            {
                wmove (win, line_no, 39);
                wprintw (win, "%s", "----");

                wmove (win, line_no, 48);
                wprintw (win, "%-15s", "---------------");
            }

            disp_cnt++;    
        }
        idx++;

        if (idx == MAX_NUM_CLIENT_IDS)
        {
            something_to_display = 0;
        }
    }

    last_disp_client_idx = idx;

    wmove (win, 21, 0);
    wprintw (win,"    F (page forward), B (page back), EXIT, MENU");

    wmove (win, 23, 0);
    wprintw (win, "Selection: ");

    refresh ();
    wrefresh (win);

    wmove (win, 23, 11);

    input_y = 23;
    input_x = 11;

    return;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Client_Id_Input(char *command, int par)                                    */
/*                                                                            */
/* Purpose: This function processes all the commands specific to the          */
/*          client id screens.                                                */
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
/* Assumptions: client_id_val is -1 when command is entered from summary screen */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void
Client_Id_Input(char *command, int par)
{
    int status;

/*  Detailed screen commands                                                  */

    if (client_id_val != -1)
    {
        if (*command == '\0')
        {
            if ((par > 0) && (par <= MAX_CLIENT_ID_VAL))
            {
                client_id_val = par;
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
        else if ((strcasecmp(command, "CLIENT")) == 0)
        {
            if (((par > 0) && (par <= MAX_CLIENT_ID_VAL)) ||
                (par == -1))
            {
                client_id_val = par;
            }
        }

/*  Summary screen commands                                                   */

    }
    else
    {
        page = PAGE_NEUTRAL;

        if (*command == '\0')
        {
            if ((par > 0) && (par <= MAX_CLIENT_ID_VAL))
            {
                client_id_val = par;
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
        else if ((strcasecmp(command, "CLIENT")) == 0)
        {
            if (((par > 0) && (par <= MAX_CLIENT_ID_VAL)) ||
                (par == -1))
            {
                client_id_val = par;
            }
        }
    }
}

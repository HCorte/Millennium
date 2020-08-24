static const char *fileid = "";

/*
 * ===[tnivis_events.c]===================================================
 *
 * Description:
 *
 * Functions to process the event queues snapshot.
 *
 * Functions:
 *
 * Event_Queues       - Display event queue data
 * Event_Queues_Input - Process event queue snapshot commands
 *
 * -----------------------------------------------------------------------
 * This item is the property of GTECH Corporation, Providence,
 * Rhode Island, and contains confidential and trade secret information.
 * It may not be transfered from the custody or control of GTECH except
 * as authorized in writing by an officer of GTECH.  Neither this item
 * nor the information it contains may be used, transfered, reproduced,
 * published, or disclosed, in whole or in part, and directly or
 * indirectly, except as expressly authorized by an officer of GTECH,
 * pursuant to written agreement.
 *
 * Any and all modifications to this item must have the prior written
 * authorization of GTECH's Enterprise Series Platform Team.  GTECH shall
 * not be liable in any way for any direct or indirect damages,  whatsoever,
 * as a result of any unauthorized modifications.  The Enterprise Series
 * Platform Team reserves the right to refuse support as a result of
 * unauthorized modification.
 *
 * Copyright (c) 2005 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * =======================================================================
 */

#include <string.h>

#include "includes_vision.h"

/* [Event_Queues]
 *
 * Summary:
 *
 * Event_Queues()
 *
 * Description:
 *
 * This function displays event queues data and specific event element
 * data, if selected.
 *
 * Returns Values: None
 *
 */

static int event_num = 0;

void
Event_Queues()
{
    int                 line_no = 0;
    int                 queue_entries = 0;
    static int          display_init = 1;
    static int          display_cnt;
    static int          last_event_num = 0;

    RQST_EVENT         *free_que_entry;
    RQST_EVENT         *rqst_que_entry;
    RQST_EVENT         *event_data_dis;

    if (display_init)
    {
        if (tnicon_p->max_es_requests < 3)
        {
            display_cnt = tnicon_p->max_es_requests;
        }
        else
        {
            display_cnt = 3;
        }
        display_init = 0;
    }

    if (last_event_num != event_num) {

        wclear (win);
        last_event_num = event_num;
    }
/*   snap shot title                                                          */

    wmove (win, 0, 0);
    wprintw (win, "EVENT QUEUES SNAPSHOT");

    wmove (win, 0, (MAXCOL - strlen(Get_Time())));
    clrtoeol ();
    wprintw (win, "%s", Get_Time() );

    line_no = 2;

    wmove (win, line_no, 2);
    wprintw (win, "Free Event Queue");

    wmove (win, line_no, 25);
    wprintw (win, "Request Event Queue");

    line_no++;

    wmove (win, line_no, 4);
    wprintw (win, "(%3.3d of %3.3d)",
             event_free_queue->length,
             tnicon_p->max_es_requests);

    wmove (win, line_no,32);
    wprintw (win, "(%3.3d)", rpc_events_queue->length);

    line_no++;

    wmove (win, line_no, 2);
    wprintw (win, "----------------");

    wmove (win, line_no, 25);
    wprintw (win, "-------------------");

    if (event_free_queue->head != -1)
    {
        free_que_entry = request_events + (event_free_queue->head - 1);
    }
    else
    {
        free_que_entry = NULL;
    }

    if (rpc_events_queue->head != -1)
    {
        rqst_que_entry = request_events + (rpc_events_queue->head - 1);
    }
    else
    {
        rqst_que_entry = NULL;
    }

    for (queue_entries = 0; queue_entries < display_cnt; queue_entries++)
    {
        line_no++;

        wmove (win, line_no, 8);

        if (free_que_entry != NULL)
        {
            wprintw (win, "%3.3d", free_que_entry->event_num);

            if (free_que_entry->next_event != -1)
            {
                free_que_entry = request_events + 
                                 (free_que_entry->next_event - 1);
            }
            else
            {
                free_que_entry = NULL;
            }
        }
        else
        {
            wprintw (win, "   ");
        }

        wmove (win, line_no, 33);

        if (rqst_que_entry != NULL)
        {
            wprintw (win, "%3.3d", rqst_que_entry->event_num);

            if (rqst_que_entry->next_event != -1)
            {
                rqst_que_entry = request_events + 
                                 (rqst_que_entry->next_event - 1); 
            }
            else 
            {
                rqst_que_entry = NULL;
            }
        }
        else
        {
            wprintw (win, "   ");
        }
    }

    if (event_num > 0)
    {
        event_data_dis = request_events + (event_num - 1);

        line_no++;
        line_no++;
        line_no++;

        wmove (win, line_no, (MAXCOL - 18)/2);
        wprintw (win, "Data for Event %3.3d", event_num);

        line_no++;

        wmove (win, line_no, (MAXCOL - 18)/2);
        wprintw (win, "------------------");

        line_no++;

        wmove (win, line_no, 2);
        wprintw (win, "Queue name          : %s",
                 queue_names[event_data_dis->queue_num]);

        wmove (win, line_no, 32);
        wprintw (win, "Cdc sent  : %d",
                 event_data_dis->cdc);

        wmove (win, line_no, 58);
        wprintw (win, "Prev. event : %3.3d",
                 event_data_dis->previous_event);

        line_no++;

        wmove (win, line_no, 2);
        wprintw (win, "Requesting product  : %3.3d",
                 event_data_dis->product_num);

        wmove (win, line_no, 32);
        wprintw (win, "Time sent : %2.2d:%2.2d.%2.2d",
                 event_data_dis->hours,
                 event_data_dis->minutes,
                 event_data_dis->seconds);

        wmove (win, line_no, 58);
        wprintw (win, "Next event  : %3.3d",
                 event_data_dis->next_event);

        line_no++;

        wmove (win, line_no, 2);
        wprintw (win, "GTMS transaction Id : %5.5d",
                 event_data_dis->transaction_id);

        wmove (win, line_no, 32);
        wprintw (win, "App Num   : %d",
                 event_data_dis->app_num);
    }
    wmove (win, 21, 0);
    wprintw (win, "    CLeaR, EXIT, MENU");

    wmove (win, 23, 0);
    wprintw (win, "Selection: ");

    refresh ();
    wrefresh (win);

    wmove (win, 23, 11);

    input_y = 23;
    input_x = 11;
}

/* [Event_Queues_Input]
 *
 * Summary:
 *
 * Event_Queues_Input(char *command, int par)
 *
 * Description:
 *
 * This function processes any event queue snapshot commands entered
 * at the prompt.
 *
 * Returns Values: None
 *
 */

void
Event_Queues_Input(char *command, int par)
{
    if ((strcasecmp (command, "CLR")) == 0 )
    {
        event_num = 0;
    }

    if (*command == '\0') {

        if ((par > 0) && (par <= tnicon_p->max_es_requests)) {

           event_num = par;
        }
    }
}

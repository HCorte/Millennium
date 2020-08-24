static const char *fileid = "";

/*
 * ===[mx_event_handler.c]================================================ 
 *
 * Description:
 *
 * Functions to process the event queues and event messages.
 *
 * Functions:
 *
 * Initialize_Event_Queue           - initialize event queue header
 * Initialize_Request_Event_List    - initialize all events in the request 
 *                                    events list
 * Add_Event_To_Free_Queue          - add event to event free queue
 * Get_Free_Event                   - get event from event free queue
 * Add_Event_To_Rpc_Rqst_Queue      - add event to rpc request event queue
 * Remove_Event_From_Rpc_Rqst_Q     - remove event from rpc request event queue
 * Get_Rqst_Event                   - find specified request event in events
 *                                    lists
 * Allocate_Rpc_Rqst_Event          - get event from free queue, place rpc
 *                                    request data in it, put rpc request
 *                                    event on rpc request queue
 * Find_Rpc_Rqst_Event              - find rpc request event on rpc request 
 *                                    queue with matching rpc request id
 * Deallocate_Rpc_Rqst_Event        - remove specified rpc request event from
 *                                    rpc request queue, return request event 
 *                                    to free queue 
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

#include "includes.h"

/* [Initialize_Event_Queue]
 *
 * Summary:
 *
 * Initialize_Event_Queue(int queue_number, EVENT_QUE_HDR *queue_hdr)
 *
 * queue_number    - queue number to assign to event queue
 * queue_hdr       - pointer to the event queue header to initialize
 *
 * Description:
 *
 * Initialize event queue header.
 *
 * Returns Values: None
 *
 */

void
Initialize_Event_Queue(int queue_number, EVENT_QUE_HDR *queue_hdr)
{
    queue_hdr->queue_num = queue_number;

    queue_hdr->length = 0;

    queue_hdr->head = -1;

    queue_hdr->tail = -1;

    return;
}

/* [Initialize_Request_Event_List]
 *
 * Summary:
 * 
 * Initialize_Request_Event_List(RQST_EVENT *event)
 *
 * event           - pointer to the first event in the request events list
 *
 * Description:
 *
 * Initialize all events in the request events list.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Initialize_Request_Event_List(RQST_EVENT *event)
{
    int                 rc = P_SUCCESS;
    int                 event_count = 0;

    event_count = 1;

    while ((event_count <= tnicon_p->max_es_requests) && (rc == P_SUCCESS))
    {
        event->event_num = event_count;
        event->queue_num = QUEUE_NOT_ASSIGNED;
        event->product_num = -1;
        event->transaction_id = -1;
        event->cdc = -1;
        event->hours = -1;
        event->minutes = -1;
        event->seconds = -1;
        event->app_num = -1;
        event->sent_time.time = 0;
        event->sent_time.millitm = 0;
        event->previous_event = -1;
        event->next_event = -1;

        rc = Add_Event_To_Free_Queue (event);

        event_count++;
        event++;
    }
    return(rc);
}

/* [Add_Event_To_Free_Queue]
 *
 * Summary:
 *
 * Add_Event_To_Free_Queue(RQST_EVENT *new_event)
 *
 * new_event        - pointer to event to be place on free queue
 *
 * Description:
 *
 * Initialize all data elements in event.  Place event on free queue.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Add_Event_To_Free_Queue(RQST_EVENT *new_event)
{
    int                 rc = P_SUCCESS;
    RQST_EVENT         *last_event;

    err_string = null_err_string;

    if (new_event != NULL)
    {
        if ((event_free_queue->length + 1) <=
             tnicon_p->max_es_requests)
        {
/* Free queue is empty */

            if (event_free_queue->tail == -1)
            {
                event_free_queue->head = new_event->event_num;
                event_free_queue->tail = new_event->event_num;
            }

/* Free queue is not empty */

            else
            {
                last_event = request_events + (event_free_queue->tail - 1);
                event_free_queue->tail = new_event->event_num;

                last_event->next_event = new_event->event_num;
                new_event->previous_event = last_event->event_num;
                new_event->next_event = -1;
            }

            new_event->queue_num = EVENT_FREE_QUEUE;
            new_event->product_num = -1;
            new_event->transaction_id = -1;
            new_event->cdc = -1;
            new_event->hours = -1;
            new_event->minutes = -1;
            new_event->seconds = -1;
            new_event->app_num = -1;
            new_event->sent_time.time = 0;
            new_event->sent_time.millitm = 0;

            event_free_queue->length++;
        }
        else
        {
            sprintf (err_string.par1, "Event free");
            sprintf (err_string.par2, "%d", tnicon_p->max_es_requests);

            output_err ("add_event_to_free_queue",
                        MI_TNI_MAX_QLEN,
                        MX_ERR_LVL_FATAL,
                        err_string);

            rc = P_FAILURE;
        }
    }
    else
    {
        sprintf (err_string.par1, "NULL event");
        sprintf (err_string.par2, "Event free");

        output_err ("add_event_to_free_queue",
                    MI_TNI_NULL_ELEMENT,
                    MX_ERR_LVL_FATAL,
                    err_string);

        rc = P_FAILURE;
    }

    return(rc);
}

/* [Get_Free_Event]
 *
 * Summary:
 *
 * Get_Free_Event(RQST_EVENT **new_event)
 *
 * new_event        - pointer which cotains the address of the request event
 *                   taken from the free queue
 *
 * Description:
 *
 * Remove the event at the top of the free queue and return is address in
 * new_event.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Get_Free_Event(RQST_EVENT **new_event)
{
    int                 rc = P_SUCCESS;
    RQST_EVENT         *new_head_event;

    err_string = null_err_string;

    if (event_free_queue->length > 0)
    {
        if ((event_free_queue->head != -1) &&
            (event_free_queue->tail != -1))
        {
            *new_event = request_events + (event_free_queue->head - 1);

/* Last event on queue */

            if ((*new_event)->event_num == event_free_queue->tail)
            {
                event_free_queue->head = -1;
                event_free_queue->tail = -1;
            }
            else
            {
                event_free_queue->head = (*new_event)->next_event;
                new_head_event = request_events + (event_free_queue->head - 1);
 
                new_head_event->previous_event = -1;
            }

            (*new_event)->queue_num = QUEUE_NOT_ASSIGNED;
            (*new_event)->previous_event = -1;
            (*new_event)->next_event = -1;

            event_free_queue->length--;
        }
        else
        {
            sprintf (err_string.par1, "Event free");
            sprintf (err_string.par2, "(empty)");

            output_err ("get_free_event",
                        MI_TNI_Q_CORRUPT,
                        MX_ERR_LVL_FATAL,
                        err_string);

            rc = P_FAILURE;
        }
    }
    else
    {

/* Only report error is RPC request queue is not full */

        if (rpc_events_queue->length != tnicon_p->max_es_requests)
        {
            sprintf (err_string.par1, "Event free");

            output_err ("get_free_event",
                        MI_TNI_Q_EMPTY,
                        MX_ERR_LVL_FATAL,
                        err_string);
        }

        rc = P_FAILURE;
    }

    return(rc);
}

/* [Add_Event_To_Rpc_Rqst_Queue]
 *
 * Summary:
 *
 * Add_Event_To_Rpc_Rqst_Queue(RQST_EVENT *new_event)
 *
 * new_event       - pointer to event to be added to the rpc request queue
 *
 * Description:
 *
 * Add event to the bottom of the rpc request queue.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Add_Event_To_Rpc_Rqst_Queue(RQST_EVENT *new_event)
{
    int                 rc = P_SUCCESS;
    RQST_EVENT         *last_event;

    err_string = null_err_string;

    if (new_event != NULL)
    {
        if ((rpc_events_queue->length + 1) <=
            tnicon_p->max_es_requests)
        {
/* Rpc request queue is empty */

            if (rpc_events_queue->tail == -1)
            {
                rpc_events_queue->head = new_event->event_num;
                rpc_events_queue->tail = new_event->event_num;

                new_event->previous_event = -1;
                new_event->next_event = -1;
            }

/* Rpc request queue is not empty */

            else
            {
                last_event = request_events + (rpc_events_queue->tail - 1);
                rpc_events_queue->tail = new_event->event_num;

                last_event->next_event = new_event->event_num;
                new_event->previous_event = last_event->event_num;
                new_event->next_event = -1;
            }

            new_event->queue_num = RPC_REQUEST_QUEUE;
            new_event->sent_time = current_time;

            rpc_events_queue->length++;
        }
        else
        {
            sprintf (err_string.par1, "RPC request");
            sprintf (err_string.par2, "%d", tnicon_p->max_es_requests);

            output_err ("add_event_to_rpc_rqst_queue",
                        MI_TNI_MAX_QLEN,
                        MX_ERR_LVL_FATAL,
                        err_string);

            rc = P_FAILURE;
        }
    }
    else
    {
        sprintf (err_string.par1, "NULL event");
        sprintf (err_string.par2, "RPC request");

        output_err ("add_event_to_rpc_rqst_queue",
                    MI_TNI_NULL_ELEMENT,
                    MX_ERR_LVL_FATAL,
                    err_string);

        rc = P_FAILURE;
    }

    return(rc);
}

/* [Remove_Event_From_Rpc_Rqst_Q]
 *
 * Summary:
 *
 * Remove_Event_From_Rpc_Rqst_Q(RQST_EVENT *remove_event)
 *
 * remove_event     - pointer to the event to be removed from request queue
 *
 * Description:
 *
 * Remove specified event from rpc request queue.  The event can located
 * anywhere in the queue.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Remove_Event_From_Rpc_Rqst_Q(RQST_EVENT *remove_event)
{
    int                 rc = P_SUCCESS;
    RQST_EVENT         *new_head_event;
    RQST_EVENT         *new_last_event;
    RQST_EVENT         *above_event;
    RQST_EVENT         *below_event;

    err_string = null_err_string;

    if (rpc_events_queue->length > 0)
    {
        if ((rpc_events_queue->head != -1) &&
            (rpc_events_queue->tail != -1))
        {
            if (remove_event->queue_num == RPC_REQUEST_QUEUE)
            {

/* First event on queue */

                if (remove_event->event_num == rpc_events_queue->head)
                {

/* Only event on the queue */

                    if (remove_event->event_num != rpc_events_queue->tail)
                    {
                        rpc_events_queue->head = remove_event->next_event;
                        new_head_event = request_events + 
                                         (rpc_events_queue->head - 1);

                        new_head_event->previous_event = -1;
                    }
                    else
                    {
                        rpc_events_queue->head = -1;
                        rpc_events_queue->tail = -1;
                    }
                }

/* Last event on queue */

                else if (remove_event->event_num == rpc_events_queue->tail)
                {
                    rpc_events_queue->tail = remove_event->previous_event;
                    new_last_event = request_events +
                                     (remove_event->previous_event - 1);
                    new_last_event->next_event = -1;
                }

/* Somewhere in the middle */

                else
                {
                    above_event = request_events + 
                                  (remove_event->previous_event - 1);
                    below_event = request_events + 
                                  (remove_event->next_event - 1);

                    above_event->next_event = below_event->event_num;
                    below_event->previous_event = above_event->event_num;
                }

                remove_event->queue_num = QUEUE_NOT_ASSIGNED;
                remove_event->previous_event = -1;
                remove_event->next_event = -1;

                rpc_events_queue->length--;
            }
            else
            {
                sprintf (err_string.par1, "Event");
                sprintf (err_string.par2, "%d", remove_event->event_num);
                sprintf (err_string.par3, "RPC request");

                output_err ("remove_event_from_rpc_rqst_queue",
                            MI_TNI_NOT_IN_Q,
                            MX_ERR_LVL_FATAL,
                            err_string);

                rc = P_FAILURE;
            }
        }
        else
        {
            sprintf (err_string.par1, "RPC request");
            sprintf (err_string.par2, "(empty)");

            output_err ("remove_event_from_rpc_rqst_queue",
                        MI_TNI_Q_CORRUPT,
                        MX_ERR_LVL_FATAL,
                        err_string);

            rc = P_FAILURE;
        }
    }
    else
    {
        sprintf (err_string.par1, "RPC request");

        output_err ("remove_event_from_rpc_rqst_queue",
                    MI_TNI_Q_EMPTY,
                    MX_ERR_LVL_FATAL,
                    err_string);

        rc = P_FAILURE;
    }

    return(rc);
}

/* [Get_Rqst_Event]
 *
 * Summary:
 *
 * Get_Rqst_Event(int event_number, RQST_EVENT **requested_event)
 *
 * event_number     - number of the event to find
 * requested_event  - pointer which contains the address of the event if found
 *
 * Description:
 *
 * Finds the specified event in the request event list.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Get_Rqst_Event(int event_number, RQST_EVENT **requested_event)
{
    int                 rc = P_SUCCESS;
    int                 event_idx = 0;

    err_string = null_err_string;
    *requested_event = NULL;

    event_idx = event_number - 1;

    if ((event_idx >= 0) && (event_idx < tnicon_p->max_es_requests))
    {
        *requested_event = request_events + event_idx;
    }
    else
    {
        sprintf (err_string.par1, "%d", event_idx);
        sprintf (err_string.par2, "%d", 0);
        sprintf (err_string.par3, "%d", tnicon_p->max_es_requests);

        output_err ("get_rqst_event",
                    MI_TNI_BAD_EVNT_NUM,
                    MX_ERR_LVL_FATAL,
                    err_string);

        rc = P_FAILURE;
    }

    return(rc);
}

/* [Allocate_Rpc_Rqst_Event]
 *
 * Summary:
 *
 * Allocate_Rpc_Rqst_Event(int app_number, int transaction_id, int cdc,
 *                         int hours, int minutes, int seconds,
 *                         int product_number, int *event_number)
 *
 * app_number      - RPC's destination application number
 * transaction_id  - GTMS transaction id of the product-to-product message
 * cdc             - cdc the request was received on
 * hours           - hour the rpc request was received in
 * minutes         - minute the rpc request was received in
 * seconds         - second the rpc request was received in
 * product_number  - product number the rpc request was received from
 * event_number    - number of the event that has been allocated
 *
 * Description:
 *
 * Gets an event from the free queue.  Populates all the data elements within
 * the event.  Puts event on the rpc request event queue.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Allocate_Rpc_Rqst_Event(int app_number, 
                        int transaction_id,
                        int cdc,
                        int hours,
                        int minutes,
                        int seconds,
                        int product_number,
                        int *event_number)
{
    int                 rc = P_SUCCESS;
    RQST_EVENT         *event = NULL;

    rc = Get_Free_Event (&event);

    if (rc == P_SUCCESS)
    {
        event->app_num = app_number;
        event->transaction_id = transaction_id;
        event->cdc = cdc;
        event->hours = hours;
        event->minutes = minutes;
        event->seconds = seconds;
        event->product_num = product_number;

        rc = Add_Event_To_Rpc_Rqst_Queue (event);

        if (rc == P_FAILURE)
        {
            Add_Event_To_Free_Queue (event);
        }
    }

    if (rc == P_SUCCESS)
    {
       *event_number = event->event_num; 
    }

    return(rc);
}

/* [Find_Rpc_Rqst_Event]
 *
 * Summary:
 *
 * Find_Rpc_Rqst_Event(int event_number, int app_number,
 *                     int transaction_id, int cdc,
 *                     int hours, int minutes, int seconds,
 *                     RQST_EVENT *event)
 *
 * event_number    - event number extracted from rpc request tag
 * app_number      - RPC's destination application number
 * transaction_id  - GTMS transaction id
 * cdc             - cdc extracted from rpc request tag
 * hours           - hour extracted from rpc request tag
 * minutes         - minute extracted from rpc request tag
 * seconds         - second extracted from rpc request tag
 * event           - pointer to event data associated with event number
 *
 * Description:
 *
 * Returns the event associated with the event number when the application 
 * number, GTMS transaction id, cdc, hours, minutes, and seconds values
 * extracted from the rpc request tag match the data in the event. Also
 * the event must presently located on the rpc request queue.  If a match
 * is not found the function returns an error with a NULL event.  A match 
 * would not be found if the MX Server receives a rpc response to a request
 * that has been timed-out.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Find_Rpc_Rqst_Event(int event_number,
                    int app_number,
                    int transaction_id,
                    int cdc,
                    int hours,
                    int minutes,
                    int seconds,
                    RQST_EVENT **event)

{
    int                 rc = P_FAILURE;             /* return code */

    *event = NULL;

    if (event_number != 0)
    {
        rc = Get_Rqst_Event (event_number, event);

        if (rc == P_SUCCESS)
        {
            if (((*event)->app_num == app_number) &&
                ((*event)->transaction_id == transaction_id) &&
                ((*event)->cdc == cdc) &&
                ((*event)->hours == hours) &&
                ((*event)->minutes == minutes) &&
                ((*event)->seconds == seconds) &&
                ((*event)->queue_num == RPC_REQUEST_QUEUE))
            {
                rc = P_SUCCESS;
            }
            else
            {
                rc = P_FAILURE;
            }
        }
    }

    return(rc);
}

/* [Deallocate_Rpc_Rqst_Event]
 *
 * Summary:
 *
 * Deallocate_Rpc_Rqst_Event(RQST_EVENT *event)
 *
 * event           - pointer to event to be deallocated 
 *
 * Description:
 *
 * Remove event from the rpc request queue and return to the free queue.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Deallocate_Rpc_Rqst_Event(RQST_EVENT *event)
{
    int                 rc = P_FAILURE;
    int                 response_time = 0;

    rc = Remove_Event_From_Rpc_Rqst_Q (event);

    if (rc == P_SUCCESS)
    {
        rc = Add_Event_To_Free_Queue (event);
    }

    return(rc);
}

void
print_event_queues()
{
    int                 rc = P_SUCCESS;
    int                 event_idx = 0;
    static int          last_free_count = 0;
    static int          last_cmd_count = 0;
    RQST_EVENT         *event = NULL;

    if (event_free_queue->length != last_free_count)
    {
        last_free_count = event_free_queue->length;

        printf("Free queue length = %d\n",event_free_queue->length);

        if (event_free_queue->length != 0)
        {
            event_idx = 0;
            event = request_events + (event_free_queue->head - 1);

            printf("Queue Index    Event Number\n");
            printf("  %d             %d\n", event_idx, event->event_num);

            while (event->next_event != -1)
            {
                event =  request_events + (event->next_event - 1);
                event_idx++;
                printf("  %d             %d\n", event_idx, event->event_num);
            }
        }
    }

    if (rpc_events_queue->length != last_cmd_count)
    {
        last_cmd_count = rpc_events_queue->length;

        printf("RPC queue length = %d\n",rpc_events_queue->length);

        if (rpc_events_queue->length != 0)
        {
            event_idx = 0;
            event = request_events + (rpc_events_queue->head - 1);

            printf("Queue Index    Event Number\n");
            printf("  %d             %d\n", event_idx, event->event_num);

            while (event->next_event != -1)
            {
                event = request_events + (event->next_event - 1);
                event_idx++;
                printf("  %d             %d\n", event_idx, event->event_num);
            }
        }
    }
}

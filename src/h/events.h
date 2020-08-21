/*  */

/*
 * ===[ events.h ]========================================================
 *
 * Description:
 *
 * Data structure definitions for event handling
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
 * Copyright (c) 2004 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * ======================================================================= */

#ifndef EVENTS_H
#define EVENTS_H

/* Global section name */

#define EVENTS_SECT "events"

/* Maximum outstanding events */

#define DEF_MAX_ES_REQUESTS 50
#define MIN_MAX_ES_REQUESTS 1

/* Message queue defines */

#define MAX_MSG_QUEUE_NAME_LEN 32

#define QUEUE_NOT_ASSIGNED 0
#define EVENT_FREE_QUEUE 1
#define RPC_REQUEST_QUEUE 2

typedef struct RQST_EVENT
{
    int                 event_num;
    int                 event_type;
    int                 queue_num;
    int                 app_num;
    int                 transaction_id;
    int                 cdc;
    int                 hours;
    int                 minutes;
    int                 seconds;
    int                 product_num;
    int                 previous_event;
    int                 next_event;
    struct timeb        sent_time;
}
RQST_EVENT;

typedef struct EVENT_QUE_HDR
{
    int                 queue_num;
    int                 length;
    int                 head;
    int                 tail;
}
EVENT_QUE_HDR;

#endif

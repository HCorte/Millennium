/*  */

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
/*====[TSTATS.H]==============================================================*/
/*                                                                            */
/* Purpose: This header file contains all data structures definitions which   */
/*          comprise the TNI global section.                                  */
/*                                                                            */
/*====[TSTATS.H]==============================================================*/
/*                                                                            */

#ifndef TNI_STATSEC_LOADED

#   define TNI_STATSEC_LOADED 1

#   if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#       include "tni_global.h"

#   elif defined(XOS_VMS)

#       include "tni_global.h"

#   else

#       error - OS-specific logic not handled.

#   endif

#   define TSTATS_SECT "tstats"

    typedef struct {

/* TNI TERMINAL STATISTICS STRUCTURE                                          */
/*                                                                            */
/* This structure defines all network terminal-based statistics.  These       */
/* statistics are kept in a separate global section so they can be            */
/* dynamically sized at runtime based upon the number of terminals            */
/*                                                                            */
/* Statistics may be kept over an entire GSWITCH day or only over the         */
/* statistics interval.  The following naming conventions are used;           */
/*                                                                            */
/* 1. Variables ending with "_tot" are the statistics kept for an entire day. */
/*    These are only initialized when the GSWITCH task is initialized and at  */
/*    day end.  If these statistics are displayed in TNI Vision, then their   */
/*    actual variables are displayed.                                         */

/* total number of terminal application messages the MX Server has received   */
/* the network terminal destined for a particular application or client id    */

        ubyte_4 app_request_tot[MAX_APPS + MAX_NUM_CLIENT_IDS];

/* total number of terminal application response messages the MX Server sent  */
/* to PX2XPRO from a particular application or client id destined for a       */
/* network terminal                                                           */

        ubyte_4 app_response_tot[MAX_APPS + MAX_NUM_CLIENT_IDS];

/* total number of terminal unsolicited application messages the MX Server    */
/* has sent to a particular application or client id destined for a network   */
/* terminal                                                                   */

        ubyte_4 app_unso_tot[MAX_APPS + MAX_NUM_CLIENT_IDS];

/* total number of transactions that are outstanding (waiting for response)   */
/* in OLTP.  This number should never be greater than 1.                      */

        ubyte_4 trans_in_game_cnt;

/* connection number on which the request message was received on             */

        unsigned char requesting_conn_num;

/* correlation tag received with request                                      */

        char correlation_tag[MAX_CORRELATION_TAG_LEN + 1];

/* time last transaction was received from game                               */

        struct timeb last_trans_recv_time;

/* time last transaction was sent to game                                     */

        struct timeb last_trans_sent_time;

/* time last transaction spent in game in milliseconds                        */

        int last_trans_in_game_time;

/* total number of terminal application request messages sent by the MX       */
/* Server                                                                     */

        ubyte_4 host_term_rqst_tot[MAX_APPS + MAX_NUM_CLIENT_IDS];

/* total number of terminal application response messages received by the MX  */
/* Server                                                                     */

        ubyte_4 host_term_resp_tot[MAX_APPS + MAX_NUM_CLIENT_IDS];

    } TERM_STATS;


    typedef struct {                  /* TNI term statistics global section   */
        TERM_STATS term_stats;        /* structure                            */
    } TSTATS;

#endif                                /* TNI_STATSEC_LOADED                   */

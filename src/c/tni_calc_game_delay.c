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
/*====[TNI_CALC_GAME_DELAY.C]=================================================*/
/*                                                                            */
/* Purpose: These functions are used to calculate the amount of time the game */
/*          takes to process a transactions.                                  */
/*                                                                            */
/* Functions:                                                                 */
/*          Start_Game_Delay_Timer (unsigned int term_stats_idx)              */
/*          Calc_Game_Delay (unsigned int term_stats_idx)                     */
/*                                                                            */
/*====[TNI_CALC_GAME_DELAY.C]=================================================*/
/*                                                                            */

#include <stdio.h>

#include "includes.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include <sys/timeb.h>

#elif defined(XOS_VMS)

#   include <timeb.h>

#else

#   error - OS-specific logic not handled.

#endif

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Start_Game_Delay_Timer (unsigned int term_stats_idx)                       */
/*                                                                            */
/* Purpose: This function                                                     */
/*                                                                            */
/* Input Arguments:                                                           */
/*          term_stats_idx   Terminal's index to the terminal statistics      */
/*                           global section.                                  */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: Terminal statistics index has already been validated.         */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Start_Game_Delay_Timer (unsigned int term_stats_idx)
{

int st;         

TERM_STATS *term_stats_p;                /* Pointer to the term statistics    */

/* Get pointer to this part of the global section                             */

   term_stats_p = &tstats_p->term_stats;

/* Get pointer to this terminal's information in global section               */

   term_stats_p = &term_stats_p[term_stats_idx];

   term_stats_p->last_trans_sent_time = current_time;

   return;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Calc_Game_Delay (unsigned int term_stats_idx)                              */
/*                                                                            */
/* Purpose: This function                                                     */
/*                                                                            */
/* Input Arguments:                                                           */
/*          term_stats_idx   Terminal's index to the terminal statistics      */
/*                           global section.                                  */ 
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: Terminal statistics index has already been validated.         */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void  Calc_Game_Delay (unsigned int term_stats_idx)
{

int st;         

struct timeb elapsed_time;

TERM_STATS *term_stats_p;                /* Pointer to the term statistics    */

/* Get pointer to this part of the global section                             */

   term_stats_p = &tstats_p->term_stats;

/* Get pointer to this terminal's information in global section               */

   term_stats_p = &term_stats_p[term_stats_idx];

   term_stats_p->last_trans_recv_time = current_time;

/* find elasped time                                                          */

   subtimes(&term_stats_p->last_trans_recv_time,
            &term_stats_p->last_trans_sent_time,
            &elapsed_time);

   term_stats_p->last_trans_in_game_time = (elapsed_time.time * 1000) +
       elapsed_time.millitm;

   tnicon_p->tot_oltp_resp_time +=
       term_stats_p->last_trans_in_game_time;

   tnicon_p->num_oltp_resps++;

   Update_Min_Max_Resp_Times(term_stats_p->last_trans_in_game_time,
                             &tnicon_p->min_oltp_resp_time,
                             &tnicon_p->max_oltp_resp_time);
   return;
}

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
/* Copyright 2003 GTECH Corporation. All rights reserved.                     */
/*                                                                            */
/*====[PRODGLOB.H]============================================================*/
/*                                                                            */
/* Purpose: External data definitions/declarations for PTNI.                  */
/*                                                                            */
/*====[PRODGLOB.H]============================================================*/
/*                                                                            */

#ifdef LIB_DECLARE_DATA

#   define GLOBAL

#else

#   define GLOBAL extern

#endif

GLOBAL TNICON         *tnicon_p;

#if defined(PROSYS_ENV_ALL)

    GLOBAL VOLCOM     *volcom_p;

    GLOBAL GBLCOM     *gblcom_p;

#endif

GLOBAL TSTATS         *tstats_p;

#if defined(PROSYS_ENV_PLATFORM)

    GLOBAL void *term_hash_p;

#endif

GLOBAL EVENT_QUE_HDR  *event_free_queue;

GLOBAL EVENT_QUE_HDR  *rpc_events_queue;

GLOBAL RQST_EVENT *request_events;

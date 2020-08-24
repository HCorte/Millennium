/*  */

/*
 * ===[mxsrv_platform_proto.h]============================================
 *
 * Description: Function prototypes specific to the AlphaGOLS platform.
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

void Build_Es_Rpc_Gols_Reply (int procom_buffer_num,
                              int reply_queue_num,
                              int result_code,
                              char *result_text,
                              char *result_data,
                              long len_of_result_data);

void Check_Game_Output (void);

int create_events_section (int num_events);

int create_core_sections (void);

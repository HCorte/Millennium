static const char *fileid = "";

/*
 * ===[log_to_tmf.c]======================================================
 *
 * Description:
 *
 * Functions to log transactions to the Master Journal File.
 *
 * Functions:
 *
 * log_rpc_transaction
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
 * Copyright (c) 2004 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * =======================================================================
 */

#include <descrip.h>
#include <string.h>

#include "includes.h"

/* [log_rpc_transaction]
 *
 * Summary:
 * 
 * log_rpc_transaction(int error_code,
 *                     char *rpc_tag,
 *                     int transaction_length,
 *                     char *transaction)
 *
 * error_code         - reason for logging RPC message (input)
 * rpc_tag            - pointer to the RPC tag assoicated with 
 *                      the RPC message (input)
 * transaction_length - length in bytes of RPC message (input)
 * transaction        - pointer to the RPC message (input)
 *
 * Description:
 *
 * Copies the RPC message into TRABUF, converts the TRABUF into a LOGTRA, and
 * then logs the message to the Master Journal File.
 *
 * Returns Values: None
 *
 */

void
log_rpc_transaction(int error_code,
                    char *rpc_tag,
                    int transaction_length,
                    char *transaction)
{
    char            local_rpc_tag[MAX_RPC_RQST_TAG_LEN];
 
#ifdef __alpha                                   
#pragma message disable (ADDRCONSTEXT)
#endif

    $DESCRIPTOR  (rpc_string, local_rpc_tag);

    if (tnicon_p->mf_log_state == LOG_ACTIVE)
    {
        memset(local_rpc_tag, ' ', MAX_RPC_RQST_TAG_LEN);

        strncpy(local_rpc_tag, rpc_tag, strlen(rpc_tag));

        LOGRPC(&error_code,
               &rpc_string,
               transaction,
               &transaction_length);
    }

   return;
}


/* [log_authentication_attempts]
 *
 * Summary:
 * 
 * log_authentication_attempts(int conn, int result_code)
 *
 * conn - Connection on which authentication attempt is made
 * result_code - Authentication result code
 *
 * Description:
 * log autthentication attempt status 
 *
 * Returns Values: None
 *
 */

void
log_authentication_attempts(int conn,
                            int result_code)
{
    return;
}

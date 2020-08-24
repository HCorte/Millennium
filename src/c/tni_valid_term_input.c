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
/*====[TNI_VALID_TERM_INPUT.C]================================================*/
/*                                                                            */
/* int Valid_Term_Input (long int term_server_id,                             */
/*                       int del_mode,                                        */
/*                       long int *term_client_id,                            */
/*                       char *term_client_tag)                               */
/*                                                                            */
/* Purpose: This function validates the host id, method                       */
/*          of terminal identification,  and availability of a PRIMARY        */
/*          connection to the host.  If the host is using a method of         */
/*          terminal identification (TAG or ID) and we do not have a TAG      */
/*          or HOST ID for the terminal index and host id, then this is a     */
/*          terminal method identification validation failure.  If we find a  */
/*          valid host id or host terminal id, then return them.              */
/*                                                                            */
/* Input Arguments:                                                           */
/*          term_server_id   terminal index as known by the PROSYS, same as   */
/*                           Terminal Server ID                               */
/*          del_mode         delivery mode of message                         */
/*                                                                            */
/* Output Arguments:                                                          */
/*          *term_client_id  Terminal Client Id assocaited with the Terminal  */
/*                           Server Id                                        */
/*          *term_client_tag Terminal Client Tag ssocaited with the Termiinal */
/*                           Server Id                                        */
/*                                                                            */
/* Return Value:                                                              */
/*          status           EXT_SUCCESS - valid TNI Protocol header          */
/*                           EXT_INV_CONN - invalid connection number         */
/*                           EXT_NO_TERM_METHOD - no terminal method defined  */
/*                                                for connection              */
/*                           EXT_NO_CONN_AVAIL - no connections exist to Host */
/*                                               ID                           */
/*                                                                            */
/* Assumptions: The term_ndx has been validated prior to calling this         */
/*              function.                                                     */
/*                                                                            */
/*                                                                            */
/*====[TNI_VALID_TERM_INPUT.C]================================================*/
/*                                                                            */

#include "includes.h"

int Valid_Term_Input (long int term_server_id, 
                      int del_mode,
                      int conn_num,
                      long int *term_client_id,
                      char *term_client_tag) {

    int status = EXT_SUCCESS;              /* returned value                  */
    int app_num = 0;
    int mx_err_lvl = MX_ERR_LVL_ERROR;

    err_string = null_err_string;

/* Check for valid connection */

    if( (conn_num < 1)||
        (conn_num > MAX_CONFIGURABLE_CONN) ) {

        sprintf(err_string.par1, "%d", conn_num);
        sprintf(err_string.par2, "%d", 1);
        sprintf(err_string.par3, "%d", MAX_CONFIGURABLE_CONN);
        sprintf(err_string.par4, "%ld", term_server_id);

        output_err("Valid_Term_Input",
                   MI_TNI_INV_CONN,
                   MX_ERR_LVL_ERROR,
                   err_string);

        status = EXT_INV_CONN;
        return(status);
    }

/* Make sure connection is in the PRIMARY state */

    if (tnicon_p->connection[conn_num].conn_state != CONN_PRIMARY)
    {
        app_num = tnicon_p->connection[conn_num].app_idx;

        switch (del_mode)
        {
            case REQUEST:

                sprintf(err_string.par1,"%s","Request");
                break;

            case RESPONSE:

                sprintf(err_string.par1,"%s","Response");
                break;

            case UNSOLICITED:

                sprintf(err_string.par1,"%s","Unsolicited");
                mx_err_lvl = MX_ERR_LVL_WARNING;
                break;

            case BROADCAST:

                sprintf(err_string.par1,"%s","Broadcast");
                mx_err_lvl = MX_ERR_LVL_WARNING;
                break;
        }

        sprintf(err_string.par2, "%ld", term_server_id);
        sprintf(err_string.par3, "%d", conn_num);

        switch (tnicon_p->connection[conn_num].tni_proto_ver)
        {
        case TNI_VERSION_23:
        case TNI_VERSION_22:
        case TNI_VERSION_21:

            sprintf(err_string.par4,"App %s",tnicon_p->app[app_num].name);

            break;

        default:

            if (tnicon_p->connection[conn_num].messaging_type ==
                MSG_TYPE_ES_RPC)
            {
                sprintf(err_string.par4,
                        "App %s",
                        tnicon_p->connection[conn_num].application_name);
            }
            else
            {
                sprintf(err_string.par4,
                        "Client Id %d",
                        tnicon_p->connection[conn_num].client_id_val);
            }
            break;
        }

        output_err("Valid_Term_Input",
                   MI_TNI_NO_DELIVER,
                   mx_err_lvl,
                   err_string);

        status = EXT_NO_CONN_AVAIL;
        return(status);
    }

/* Check for valid terminal identification method                             */

    switch( tnicon_p->connection[conn_num].term_id_method)
    {
    case METH_TERM_SERVER_ID:

        *term_client_id = -1;

        if (term_server_id == -1)
        {
            sprintf (err_string.par3, "%s", "Term Srv Id");
            status = EXT_NO_TERM_METHOD;
        }
        break;

    case METH_TERM_CLIENT_ID:

#       if defined(PROSYS_ENV_PLATFORM)

            if (*term_client_id == -1)
            {
                sprintf (err_string.par3, "%s", "Term Clt Id");
                status = EXT_NO_TERM_METHOD;
            }
#       else

#           if defined(GOLS_ENV_ALL)

                term_server_id = -1;

                if (*term_client_id == -1)
                {
                    sprintf (err_string.par3, "%s", "Term Clt Id");
                    status = EXT_NO_TERM_METHOD;
                }
#           else

                *term_client_id = 
                get_term_ndx_from_term_net_id (term_server_id);

                if (*term_client_id == -1)
                {
                    status = EXT_BAD_CONVERSION;
                    return(status);
                }

#           endif

#       endif

        break;

    case METH_TERM_CLIENT_TAG:

        sprintf (err_string.par3, "%s", "Term Clt Id");
        status = EXT_NO_TERM_METHOD;
        break;
    }                                                 /* end switch           */

    if (status == EXT_NO_TERM_METHOD)
    {
        switch (del_mode)
        {
        case REQUEST:

            sprintf (err_string.par1, "%s", "Request");
            break;

        case RESPONSE:

            sprintf (err_string.par1, "%s", "Response");
            break;

        case UNSOLICITED:

            sprintf (err_string.par1, "%s", "Unsolicited");
            break;

        case BROADCAST:

            sprintf (err_string.par1, "%s", "Broadcast");
            break;
        }

        sprintf (err_string.par2, "%d", conn_num);

        output_err ("Valid_Term_Input",
                    MI_TNI_NO_TERM_ID,
                    MX_ERR_LVL_ERROR,
                    err_string);

        return(status);
    }

    status = EXT_SUCCESS;                             
    return(status);

}                                                     /* end valid_term_input */

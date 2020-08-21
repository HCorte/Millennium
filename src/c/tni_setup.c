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
/*====[TNI_SETUP.C]===========================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Tni_Setup()                                                       */
/*                                                                            */
/*====[TNI_SETUP.C]===========================================================*/
/*                                                                            */

#include <stdio.h>

#include "includes.h"
#include "tnicfg.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Tni_Setup()                                                                */
/*                                                                            */
/* Purpose: This function reads and validates the TNI server configuration    */
/*          file.  All appropriate data structures are populated.             */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              1 TNI configuration successful                   */
/*                           0 TNI configuration unsuccessful                 */
/*                                                                            */
/* Assumptions: If the TNI configuration is unsuccessful, then the TNI task   */
/*              will be terminated by the calling function.                   */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Tni_Setup()
{
    int                 mx_config_last_assigned_conn = 0;
    int                 mx_config_open_rc = P_FAILURE;
    int                 mx_config_rc = P_FAILURE;
    int                 rc = P_SUCCESS;
    int                 tni_config_conns = 0;
    int                 tni_config_first_assignable_conn = 1;
    int                 tni_config_open_rc = P_FAILURE;
    int                 tni_config_rc = P_FAILURE;

    err_string = null_err_string;

    mx_config_open_rc = Open_Mx_Config_File();

    if (mx_config_open_rc == P_SUCCESS)
    {
        mx_config_rc = P_SUCCESS;
    }

    if (mx_config_rc == P_SUCCESS)
    {
        mx_config_rc = Process_Control_Section();
    }

    if (mx_config_rc == P_SUCCESS)
    {
        mx_config_rc = Process_Tni_Section();
    }

    if (mx_config_rc == P_SUCCESS)
    {
        mx_config_rc = Process_App_Section(&mx_config_last_assigned_conn);
    }

    if (mx_config_rc == P_SUCCESS)
    {
        mx_config_rc = Process_Buffer_Section();
    }

/* Now attempt to read the tni_server.fil file.                               */
 
    tni_config_open_rc = Open_Tni_Config_File();

    if (tni_config_open_rc == P_SUCCESS)
    {
        tni_config_rc = P_SUCCESS;

        if (mx_config_rc == P_SUCCESS)
        {
            if (mx_config_last_assigned_conn < MAX_CONFIGURABLE_CONN)
            {
                tni_config_first_assignable_conn = 
                    mx_config_last_assigned_conn + 1;
            }
            else
            {
                sprintf(err_string.par1,"%s",TNI_SERVER_CFG_NAME);

                output_err("Tni_Setup",
                           MI_TNI_NOREAD_TNICFG,
                           MX_ERR_LVL_ERROR,
                           err_string);

                tni_config_rc = P_FAILURE;
            }
        }
    }
    else    /* tni_server.fil could not be read */
    {
        /* Check if at least one connection is defined */
        if (mx_config_open_rc == P_SUCCESS && mx_config_last_assigned_conn == 0)
        {
            sprintf(err_string.par1, "%s", SERVER_CFG_NAME);

            output_err("Tni_Setup",
                        MI_TNI_NO_CONNS,
                        MX_ERR_LVL_ERROR,
                        err_string);
            
            mx_config_rc = P_FAILURE;
        }
    }

    if (tni_config_rc == P_SUCCESS)
    {
        tni_config_rc = Process_Control_Section_Tni (mx_config_rc,
                                                     &tni_config_conns);
    }

    if (tni_config_rc == P_SUCCESS)
    {
       tni_config_rc = Process_Tni_Section_Tni (mx_config_rc);
    }

    if (tni_config_rc == P_SUCCESS)
    {
       tni_config_rc = 
               Process_Conn_Section_Tni (tni_config_conns,
                                         tni_config_first_assignable_conn);
    }

    if (tni_config_rc == P_SUCCESS)
    {
       tni_config_rc =
           Validate_Tni_Config_Conns (tni_config_first_assignable_conn);
    }

    if (tni_config_rc == P_SUCCESS)
    {
        memcpy ((void *) tnicon_p->sorted_client_ids,
                (void *) tnicon_p->client_idx_tbl,
                sizeof(tnicon_p->client_idx_tbl));

        Q_Sort_Client_Ids (0, MAX_NUM_CLIENT_IDS - 1);
    }

    if ((mx_config_open_rc == P_FILE_NOT_FOUND) &&
        (tni_config_open_rc == P_FILE_NOT_FOUND))
    {
        sprintf (err_string.par1, "unable to find");
        sprintf (err_string.par2, "%s", SERVER_CFG_NAME);

        output_err ("Tni_Setup",
                    ME_FILE_ERROR,
                    MX_ERR_LVL_ERROR,
                    err_string);

        sprintf (err_string.par1, "unable to find");
        sprintf (err_string.par2, "%s", TNI_SERVER_CFG_NAME);
        output_err ("Tni_Setup",
                    ME_FILE_ERROR,
                    MX_ERR_LVL_ERROR,
                    err_string);

        rc = P_FAILURE;
    }
    else if ((mx_config_open_rc != P_SUCCESS) &&
             (mx_config_open_rc != P_FILE_NOT_FOUND))
    {
        rc = P_FAILURE;
    }
    else if ((tni_config_open_rc != P_SUCCESS) &&
             (tni_config_open_rc != P_FILE_NOT_FOUND))
    {
        rc = P_FAILURE;
    }
    else if ((mx_config_open_rc == P_SUCCESS) &&
             (mx_config_rc == P_FAILURE))
    {
        rc = P_FAILURE;
    }
    else if ((tni_config_open_rc == P_SUCCESS) &&
             (tni_config_rc == P_FAILURE))
    {
        rc = P_FAILURE;
    }

    return(rc);

} /* end Tni_Setup */

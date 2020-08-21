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
/*====[MX_COMM_MODE.C]========================================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Check_Comm_mode()                                                 */
/*                                                                            */
/*====[MX_COMM_MODE.C]========================================================*/
/*                                                                            */

#include "includes.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Check_Comm_Mode()                                                          */
/*                                                                            */
/* Purpose: This function check the MX Server communication mode.  If the     */
/*          communication mode set to COMM_SYNC (enable MX communications     */
/*          only after PX2X/X2X has enabled), then  the communication status  */
/*          of the PX2X/X2X subsystem is retrieved  on a periodic bases.  In  */
/*          PRO:SYS a product-to-product is sent to retrieve the status of    */
/*          PX2X.  In GOLS the status is retrieved directly from the X2XCOM   */
/*          shared image.  Once PX2X/X2X has enabled communications, then the */
/*          MX Server communications will be enabled.  Subsystem status will  */
/*          be retrieved every 15 seconds.                                    */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Check_Comm_Mode()
{
    int i = 0;
    int x2x_comm_state;
    int current_state;

    struct timeb elapsed_time;

    static int init_check_comm_mode = 1;

    static struct timeb check_interval;
    static struct timeb last_checked_time;

    err_string = null_err_string;

    if (init_check_comm_mode) {

        check_interval = Set_Timervalue (15 * TICS_PER_SEC);
        last_checked_time = current_time;

        switch (tnicon_p->start_mode) {

        case COMM_DISABLED:

            for (i = 0; i < MAX_INTERFACES_PER_TNI; i++)
            {
                listen_socket[i] = -1;
            }

            tnicon_p->comm_mode = COMM_DISABLED;
            break;

        case COMM_ENABLE:

            Listen_Socket_Init ();
            tnicon_p->comm_mode = COMM_ENABLED;
            break;

        case COMM_SYNC:

            Listen_Socket_Init ();
            tnicon_p->comm_mode = COMM_SYNC;
            break;
        }

        init_check_comm_mode = 0;
    }

#   if defined(GOLS_ENV_ALL)

        GET_X2X_COMM_STATE(&x2x_comm_state);

        if (x2x_comm_state == -1)
        {
            tnicon_p->comm_mode = COMM_SHUTDOWN;
        }

#   endif

    switch (tnicon_p->comm_mode) {

    case COMM_INSYNC:

        output_err("Check_Comm_Mode",
                   MI_TNI_COMM_ENABLE,
                   MX_ERR_LVL_INFO,
                   err_string);

        sprintf(err_string.par1,"Communication mode");
        sprintf(err_string.par2,"SYNCHRONIZING");
        sprintf(err_string.par3,"ENABLED");

        output_err("Check_Comm_Mode",
                   MI_TNI_CMD_CHNG,
                   MX_ERR_LVL_INFO,
                   err_string);

        tnicon_p->comm_mode = COMM_ENABLED;
        break;

    case COMM_ENABLE:

        Listen_Socket_Init ();
        tnicon_p->comm_mode = COMM_ENABLED;
        break;

    case COMM_SHUTDOWN:

        Close_All_Connections();

        Close_Listen_Sockets();

        if (tnicon_p->dbg_state == DBG_ACTIVE) {

            Close_Dbg_file();
        }

        if (tnicon_p->log_state == LOG_ACTIVE) {

            Close_Log_file();
        }
        break;

    case COMM_SYNC:

        subtimes(&current_time, &last_checked_time,
                 &elapsed_time);

        if ((elapsed_time.time > check_interval.time) ||
            ((elapsed_time.time == check_interval.time) &&
             (elapsed_time.millitm >= check_interval.millitm))) {

#           if defined(PROSYS_ENV_ALL)

                if ((volcom_p->sys.gtms_idn == volcom_p->prod.prime_id) &&
                    (volcom_p->prod.prime_id != -1))
                {
                    current_state = PRIMARY;

                }
                else
                {
                    current_state = NOT_PRIMARY;
                }

#           endif

#           if defined(GOLS_ENV_ALL)

                GET_GAME_STATE(&current_state);

#           endif

#           if defined(PROSYS_ENV_ALL)

                Send_Px2x_Enable_Req();

                if (current_state == PRIMARY)
                {
                    sprintf(err_string.par1,
                            "PX2X");

                    output_err("Check_Comm_Mode",
                               MI_TNI_COMM_WAIT,
                               MX_ERR_LVL_WARNING,
                               err_string);
                }

#           endif

#           if defined(GOLS_ENV_ALL)

                if (x2x_comm_state == 1)
                {
                    tnicon_p->comm_mode = COMM_INSYNC;
                }
                else
                {
                    if (current_state == PRIMARY)
                    {
                        sprintf(err_string.par1,
                                "X2X");

                        output_err("Check_Comm_Mode",
                                   MI_TNI_COMM_WAIT,
                                   MX_ERR_LVL_WARNING,
                                   err_string);
                    }
                }

#           endif

            last_checked_time = current_time;
        }
        break;
    }
}

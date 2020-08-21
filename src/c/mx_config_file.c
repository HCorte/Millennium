static const char *fileid = "";

/*
 * ===[mx_config_file.c]================================================== 
 *
 * Description:
 *
 * Functions to parse the mx_server.fil file.
 *
 * Functions:
 *
 * Open_Mx_Config_File
 * Process_Control_Section
 * Process_Tni_Section
 * Process_App_Section
 * Process_Buffer_Section
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
 * ======================================================================= */

#include <errno.h>
#include <string.h>

#include "includes_mbuf.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include "tnicfg.h"
#   include "bufcfg.h"

#elif defined(XOS_VMS)

#   include <limits.h>
#   include <ssdef.h>

#   include "tnicfg.h"
#   include "bufcfg.h"

#else

#   error - OS-specific logic not handled.

#endif

/*
** External variables
*/

    static FILE        *fp;
    static char        *cfgbufp;
    static char         cfgbuf[TNI_CFGBUFLEN];

/* [Open_Mx_Config_File]
 *
 * Summary:
 *
 * Open_Mx_Config_File ()
 *
 * Description:
 *
 * This function opens the MX Server configuration file.  The contents of
 * the file are read into cfgbuf.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Open_Mx_Config_File (void) 
{
    int                 rc = P_SUCCESS;
    int                 cfglen = -1;
    char               *cfgnamep;
    char                strbuf[256];

    err_string = null_err_string;

    cfgnamep = SERVER_CFG_NAME;

#   if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

        g_translate (cfgnamep, strbuf);
        cfgnamep = strbuf;

#   endif

    /*
    ** Open MX Server configuration file
    */

    if ((fp = fopen (cfgnamep, "r")) == NULL)
    {
        if (errno != ENOENT)
        {
            sprintf (err_string.par1, "Opening");
            sprintf (err_string.par2, "%s", cfgnamep);

            output_err ("Open_Mx_Config_File",
                        ME_FILE_ERROR,
                        MX_ERR_LVL_ERROR,
                        err_string);

            rc = P_FAILURE;
        }
        else
        {
            rc = P_FILE_NOT_FOUND;
        }
    }

    /*
    ** Read in MX Server configuration file
    */

    if (rc == P_SUCCESS)
    {
        if ((cfglen = tni_cfgread (fp, cfgbuf, sizeof (cfgbuf))) < 0)
        {
            sprintf (err_string.par1, "%s", cfgnamep);

            output_err ("Open_Mx_Config_File",
                        MI_TNI_INV_FORMAT,
                        MX_ERR_LVL_ERROR,
                        err_string);

            rc = P_FAILURE;
        }
        fclose (fp);
    }

    return(rc);
}

/* [Process_Control_Section]
 *
 * Summary:
 *
 * Process_Control_Section ()
 *
 * Description:
 *
 * This function parse threw the CONTROL section of the MX Server
 * configuration file and extracts all the keywords. 
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Process_Control_Section ()
{
    int                 rc = P_SUCCESS;
    int                 prim_notify_mode;

    char               *app_name = NULL;
    char               *start_mode = NULL;
    char               *logging = NULL;
    char               *str_value = NULL;
    char               *keystore = NULL;
    char               *publickey = NULL;
    char                tmp_str[MAX_FILE_PATH_NAME];

    struct timeb        local_time;

    err_string = null_err_string;
    memset (tmp_str, 0x00, sizeof (tmp_str));

    ftime (&local_time);

    /*
    ** Set config pointer to the top of the buffer
    */
    
    cfgbufp = cfgbuf;

    /*
    ** Find to control section
    */

    if (tni_cfgsect (TNI_CFGCTL, cfgbufp) == NULL) {

        sprintf (err_string.par1, "%s", TNI_CFGCTL);

        output_err ("Tni_Cfg_Cont",
                    MI_TNI_NO_SECTION,
                    MX_ERR_LVL_ERROR,
                    err_string);

        rc = P_FAILURE;
    }
 
    /*
    ** Process Keywords
    */

    if (rc == P_SUCCESS)
    {
        /*
        ** Takeover resolution interval (seconds)
        */

        tnicon_p->takeover_invl = tni_cfgint (TNI_CFGTAKOVR, NULL, 10, 'O');

        if (tnicon_p->takeover_invl == -1)
        {
            tnicon_p->takeover_invl = DEF_TAKEOVER_TIME;
        }
        else if ((tnicon_p->takeover_invl < MIN_TAKEOVER_TIME_VAL)||
                 (tnicon_p->takeover_invl > MAX_TAKEOVER_TIME_VAL))
        {
            tnicon_p->takeover_invl = DEF_TAKEOVER_TIME;

            sprintf (err_string.par1, "%d", DEF_TAKEOVER_TIME);
            sprintf (err_string.par2, "%s", TNI_CFGTAKOVR);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        tnicon_p->takeover_time =
            Set_Timervalue (tnicon_p->takeover_invl * TICS_PER_SEC);

        /*
        ** Check for connection interval (seconds)
        */

        tnicon_p->check_conn_invl =
            tni_cfgint (TNI_CFGCHKCONN, NULL, 10, 'O');

        if (tnicon_p->check_conn_invl == -1)
        {
            tnicon_p->check_conn_invl = DEF_CHECK_CONN_TIME;
        }
        else if ((tnicon_p->check_conn_invl < MIN_CHECK_CONN_TIME_VAL)||
                 (tnicon_p->check_conn_invl > MAX_CHECK_CONN_TIME_VAL))
        {
            tnicon_p->check_conn_invl = DEF_CHECK_CONN_TIME;

            sprintf(err_string.par1,"%d",DEF_CHECK_CONN_TIME);
            sprintf(err_string.par2,"%s",TNI_CFGCHKCONN);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        tnicon_p->check_conn_time =
            Set_Timervalue (tnicon_p->check_conn_invl * TICS_PER_SEC);

        /*
        ** Check for read attempt interval (10ms tics)
        */

        tnicon_p->read_atmpt_invl =
           tni_cfgint (TNI_CFGRDATMPT, NULL, 10, 'O');

        if (tnicon_p->read_atmpt_invl == -1)
        {
            tnicon_p->read_atmpt_invl = DEF_READ_ATMPT_TIME;

        }
        else if ((tnicon_p->read_atmpt_invl < MIN_READ_ATMPT_TIME_VAL)||
                 (tnicon_p->read_atmpt_invl > MAX_READ_ATMPT_TIME_VAL))
        {
            tnicon_p->read_atmpt_invl = DEF_READ_ATMPT_TIME;

            sprintf (err_string.par1, "%d", DEF_READ_ATMPT_TIME);
            sprintf (err_string.par2, "%s", TNI_CFGRDATMPT);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        tnicon_p->read_atmpt_time =
            Set_Timervalue (tnicon_p->read_atmpt_invl);

        /*
        ** Check for target cycle rate (cycles per second)
        */

        tnicon_p->cycle_rate =
            tni_cfgint (TNI_CFGCYCLE, NULL, 10, 'O');

        if (tnicon_p->cycle_rate == -1)
        {
            tnicon_p->cycle_rate = DEF_CYCLE_RATE;

        } else if ((tnicon_p->cycle_rate < MIN_CYCLE_RATE_VAL)||
                   (tnicon_p->cycle_rate > MAX_CYCLE_RATE_VAL))
        {
            tnicon_p->cycle_rate = DEF_CYCLE_RATE;

            sprintf (err_string.par1, "%d", DEF_CYCLE_RATE);
            sprintf (err_string.par2, "%s", TNI_CFGCYCLE);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        /*
        ** Check for statistics time (seconds)
        */

        tnicon_p->statistics_invl =
           tni_cfgint (TNI_CFGSTATS, NULL, 10, 'O');

        if (tnicon_p->statistics_invl == -1)
        {
            tnicon_p->statistics_invl = DEF_STATS_TIME;

        } else if ((tnicon_p->statistics_invl < MIN_STATS_TIME_VAL)||
                    (tnicon_p->statistics_invl > MAX_STATS_TIME_VAL))
        {
            tnicon_p->statistics_invl = DEF_STATS_TIME;

            sprintf (err_string.par1, "%d", DEF_STATS_TIME);
            sprintf (err_string.par2, "%s", TNI_CFGSTATS);
 
            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        tnicon_p->statistics_time =
            Set_Timervalue (tnicon_p->statistics_invl * TICS_PER_SEC);

        /* Update time stamp for last statistics time */

         tnicon_p->last_stats_time = local_time;

        /*
        ** Check for TCP send buffer size (bytes)
        */

        tnicon_p->send_buf_size =
            tni_cfgint (TNI_CFGSNDBUF, NULL, 10, 'O');

        if (tnicon_p->send_buf_size == -1)
        {
            tnicon_p->send_buf_size = DEF_TCP_BUF_SIZE;

        } else if ((tnicon_p->send_buf_size < MIN_TCP_BUF_SIZE_VAL)||
                   (tnicon_p->send_buf_size > MAX_TCP_BUF_SIZE_VAL))
        {
            tnicon_p->send_buf_size = DEF_TCP_BUF_SIZE;

            sprintf (err_string.par1, "%d", DEF_TCP_BUF_SIZE);
            sprintf (err_string.par2, "%s", TNI_CFGSNDBUF);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        /*
        ** Check for TCP receive buffer size (bytes)
        */

        tnicon_p->rcv_buf_size =
            tni_cfgint (TNI_CFGRCVBUF, NULL, 10, 'O');

        if (tnicon_p->rcv_buf_size == -1)
        {
            tnicon_p->rcv_buf_size = DEF_TCP_BUF_SIZE;

        } else if ((tnicon_p->rcv_buf_size < MIN_TCP_BUF_SIZE_VAL)||
                   (tnicon_p->rcv_buf_size > MAX_TCP_BUF_SIZE_VAL))
        {
            tnicon_p->rcv_buf_size = DEF_TCP_BUF_SIZE;

            sprintf (err_string.par1, "%d", DEF_TCP_BUF_SIZE);
            sprintf (err_string.par2, "%s", TNI_CFGRCVBUF);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        /*
        ** Check for socket read/write timeout interval (10ms tics)
        */

        tnicon_p->rd_wrt_tout_invl =
            tni_cfgint (TNI_CFGRDWRTOUT, NULL, 10, 'O');

        if (tnicon_p->rd_wrt_tout_invl == -1) {

            tnicon_p->rd_wrt_tout_invl = DEF_RDWRT_TOUT_INVL;

        } else if ((tnicon_p->rd_wrt_tout_invl < MIN_RDWRT_TOUT_INVL)||
                   (tnicon_p->rd_wrt_tout_invl > MAX_RDWRT_TOUT_INVL))
        {
            tnicon_p->rd_wrt_tout_invl = DEF_RDWRT_TOUT_INVL;

            sprintf (err_string.par1, "%d", DEF_RDWRT_TOUT_INVL);
            sprintf (err_string.par2, "%s", TNI_CFGRDWRTOUT);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

#       if defined(XOS_VMS)

            tnicon_p->rd_wrt_tout_time =
                Set_Timervalue_Vms (tnicon_p->rd_wrt_tout_invl);

#       endif

        /*
        ** Check for start mode
        */

        start_mode = tni_cfgstr (TNI_CFGSTARTMODE, NULL);

        if (start_mode != NULL) {

            if (strcmp (start_mode, "MANUAL") ==0)
            {
                tnicon_p->start_mode = COMM_DISABLED;
            }

            else if (strcmp (start_mode, "AUTO") ==0)
            {
                tnicon_p->start_mode = COMM_ENABLE;
            }

            else if (strcmp (start_mode, "SYNC_COMM") ==0)
            {
                tnicon_p->start_mode = COMM_SYNC;
            }

            else
            {
                tnicon_p->start_mode = DEF_START_MODE;

                sprintf (err_string.par1, "MANUAL");
                sprintf (err_string.par2, "%s", TNI_CFGSTARTMODE);

                output_err ("Process_Control_Section",
                            MI_TNI_DEF_VALUE,
                            MX_ERR_LVL_INFO,
                            err_string);
            }
        }
        else
        {
            tnicon_p->start_mode = DEF_START_MODE;
        }

        /*
        ** Check for primary notification mode
        */

        prim_notify_mode = tni_cfgint (TNI_CFGNOTIFYMODE, NULL, 10, 'O');

        switch (prim_notify_mode)
        {
            case NOTIFY_IMMEDIATELY:
            case NOTIFY_WAIT:

                 tnicon_p->prim_notify_mode = prim_notify_mode;
                 break;
            default:

                 tnicon_p->prim_notify_mode = DEF_PRIM_NOTIFY_MODE;
                 break;
        }

        /*
        ** Check for event logging
        */

        logging = tni_cfgstr (TNI_CFGEVENTLOG, NULL);

        if (logging != NULL)
        {
            if (strcmp (logging, "ENABLE") == 0)
            {
                tnicon_p->log_state = LOG_INIT_FILE;
            }

            else if (strcmp (logging, "DISABLE") == 0)
            {
                tnicon_p->log_state = LOG_OFF;
            }

            else
            {
                tnicon_p->log_state = LOG_OFF;

                sprintf (err_string.par1, "DISABLE");
                sprintf (err_string.par2, "%s", TNI_CFGEVENTLOG);

                output_err ("Process_Control_Section",
                            MI_TNI_DEF_VALUE,
                            MX_ERR_LVL_INFO,
                            err_string);
            }
        }
        else
        {
            tnicon_p->log_state = LOG_OFF;
        }

        /*
        ** Check for MJF or TMF logging
        */

#       if defined(PROSYS_ENV_ALL)

            logging = tni_cfgstr (TNI_CFGLOGTOMJF, NULL);
            sprintf (err_string.par2, "%s", TNI_CFGLOGTOMJF);

#       else

            logging = tni_cfgstr (TNI_CFGLOGTOTMF, NULL);
            sprintf (err_string.par2, "%s", TNI_CFGLOGTOTMF);

#       endif

        if (logging != NULL)
        {
            if (strcmp (logging, "ENABLE") == 0)
            {
                tnicon_p->mf_log_state = LOG_ACTIVE;
            }

            else if (strcmp (logging, "DISABLE") == 0)
            {
                tnicon_p->mf_log_state = LOG_OFF;
            }

            else
            {
                tnicon_p->mf_log_state = LOG_OFF;

                sprintf (err_string.par1, "DISABLE");

                output_err ("Process_Control_Section",
                            MI_TNI_DEF_VALUE,
                            MX_ERR_LVL_INFO,
                            err_string);
            }
        }
        else
        {
            tnicon_p->mf_log_state = LOG_OFF;
        }

        /*
        ** Maximum outstanding RPC requests
        */

        if ((tnicon_p->max_es_requests =
             tni_cfgint (TNI_CFGESRQSTS, NULL, 10, 'O')) < MIN_MAX_ES_REQUESTS)
        {
            tnicon_p->max_es_requests = DEF_MAX_ES_REQUESTS;
        }

        /*
        ** Default RPC request timeout
        */

        if ((tnicon_p->def_rpc_rqst_timeout =
             tni_cfgint (TNI_CFGDEFRPCRQST, NULL, 10, 'O')) < 1)
        {
            tnicon_p->def_rpc_rqst_timeout = DEF_RPC_RQST_TIMEOUT;
        }

        tnicon_p->check_rpc_timeout =
          Set_Timervalue ((tnicon_p->def_rpc_rqst_timeout * TICS_PER_SEC)/3);

        /*
        ** Authentication Failure Threshold
        */

        tnicon_p->auth_failure_threshold =
            tni_cfgint (TNI_CFGAUTHTSHOLD, NULL, 10, 'O');

        if (tnicon_p->auth_failure_threshold == -1) {

            tnicon_p->auth_failure_threshold = DEF_AUTH_FAIL_TSHOLD;

        } else if ((tnicon_p->auth_failure_threshold < MIN_AUTH_FAIL_TSHOLD )||
                   (tnicon_p->auth_failure_threshold > MAX_AUTH_FAIL_TSHOLD ))
        {
            tnicon_p->auth_failure_threshold = DEF_AUTH_FAIL_TSHOLD;

            sprintf (err_string.par1, "%d", DEF_AUTH_FAIL_TSHOLD);
            sprintf (err_string.par2, "%s", TNI_CFGAUTHTSHOLD);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        /*
        ** Authentication Lockout Interval
        */

        tnicon_p->auth_lockout_invl =
            tni_cfgint (TNI_CFGAUTHLOINVL, NULL, 10, 'O');

        if (tnicon_p->auth_lockout_invl == -1) {

            tnicon_p->auth_lockout_invl = DEF_AUTH_LOCKOUT_INVL;

        } else if ((tnicon_p->auth_lockout_invl < MIN_AUTH_LOCKOUT_INVL )||
                   (tnicon_p->auth_lockout_invl > MAX_AUTH_LOCKOUT_INVL ))
        {
            tnicon_p->auth_lockout_invl = DEF_AUTH_LOCKOUT_INVL;

            sprintf (err_string.par1, "%d", DEF_AUTH_LOCKOUT_INVL);
            sprintf (err_string.par2, "%s", TNI_CFGAUTHLOINVL);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        tnicon_p->auth_lockout_invl *= 60; /* Convert the value read into seconds */

        /*
        ** Check default terminal request message destination (ES application name)
        */

        app_name = tni_cfgstr (TNI_CFGRQSTDEST, NULL);

        if (app_name != NULL)
        {
            memcpy((void *)tnicon_p->def_term_rqst_dest,
                   (void *) app_name,
                   (size_t) _MIN(sizeof(tnicon_p->def_term_rqst_dest)-1,
                    strlen (app_name)));
        }

        /*
        ** Check for write select timeout interval (milliseconds)
        */

        tnicon_p->wrt_select_tout_invl =
           tni_cfgint (TNI_CFGWRTSELECT, NULL, 10, 'O');

        if (tnicon_p->wrt_select_tout_invl == -1)
        {                                           /* keyword not present */
            tnicon_p->wrt_select_timeout.tv_sec = 0;  
            tnicon_p->wrt_select_timeout.tv_usec = DEF_WRT_SELECT_TIMEOUT;
        }                                           /* keyword not present */
        else if ((tnicon_p->wrt_select_tout_invl == 0) ||
                 (tnicon_p->wrt_select_tout_invl == 999))
        {                                           /* 0 => don't perform Write Select, 999 => Do Select, wait indefinitely */
            tnicon_p->wrt_select_timeout.tv_sec = 0;
            tnicon_p->wrt_select_timeout.tv_usec = 0;
        }                                           /* 0 => don't perform Write Select, 999 => Do Select, wait indefinitely */
        else if ((tnicon_p->wrt_select_tout_invl < MIN_WRT_SELECT_TOUT_INVL)||
                    (tnicon_p->wrt_select_tout_invl > MAX_WRT_SELECT_TOUT_INVL))
        {                                           /* not within valid Select Timeout range - impose default */
            tnicon_p->wrt_select_tout_invl = -1;
            tnicon_p->wrt_select_timeout.tv_sec = 0;
            tnicon_p->wrt_select_timeout.tv_usec = DEF_WRT_SELECT_TIMEOUT;

            sprintf (err_string.par1, ".001");
            sprintf (err_string.par2, "%s", TNI_CFGWRTSELECT);

            output_err ("Process_Control_Section",
                        MI_TNI_DEF_VALUE,
                        MX_ERR_LVL_INFO,
                        err_string);
        }                                           /* not within valid Select Timeout range - impose default */
        else 
        {                                           /* valid Select Timeout range */
            tnicon_p->wrt_select_timeout.tv_sec = 0;
            tnicon_p->wrt_select_timeout.tv_usec = tnicon_p->wrt_select_tout_invl * 1000;
        }                                           /* valid Select Timeout range */

        /*
        ** Check for unsolicited message delivery failure notification
        */

        str_value = tni_cfgstr (TNI_CFGUNSONOTIFY, NULL);

        if (str_value != NULL) {

            if (strcasecmp (str_value, "ENABLE") == 0)
            {
                tnicon_p->unso_failure_notify2 = MX_ENABLED;
            }

            else if (strcasecmp (str_value, "DISABLE") == 0)
            {
                tnicon_p->unso_failure_notify2 = MX_DISABLED;
            }

           else
            {
                tnicon_p->unso_failure_notify2 = MX_ENABLED;

                sprintf (err_string.par1, "ENABLE");
                sprintf (err_string.par2, "%s", TNI_CFGUNSONOTIFY);

                output_err ("Process_Control_Section",
                            MI_TNI_DEF_VALUE,
                            MX_ERR_LVL_INFO,
                            err_string);
            }
        }
        else
        {
            tnicon_p->unso_failure_notify2 = MX_ENABLED;
        }

        /*
        ** Keystore
        */

        keystore = tni_cfgstr (TNI_CFGKEYSTORE, NULL);

        if (keystore != NULL)
        {

#           if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

                g_translate (keystore, tmp_str);
                keystore = tmp_str;

#           endif

            memcpy((void *)tnicon_p->keystore_file,
                   (void *) keystore,
                   (size_t) _MIN(sizeof(tnicon_p->keystore_file)-1,
                   strlen (keystore)));
            tnicon_p->exchange_enc_key_state = ENC_CONFIGURED;
        }
        else
        {
            tnicon_p->exchange_enc_key_state = ENC_NOT_CONFIGURED;

            sprintf (err_string.par1,
                     "Data encryption not possible: %s not configured",
                     TNI_CFGKEYSTORE);
            output_err ("Process_Control_Section",
                        MI_TNI_STRING,
                        MX_ERR_LVL_INFO,
                        err_string);
        }

        /*
        ** Public Key (Keystore keyword must be read first!!)
        */

        publickey = tni_cfgstr (TNI_CFG_PUBLICKEY, NULL);

        if (publickey != NULL)
        {

#           if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

                g_translate (publickey, tmp_str);
                publickey = tmp_str;

#           endif

            memcpy((void *)tnicon_p->publickey_file,
                   (void *) publickey,
                   (size_t) _MIN(sizeof(tnicon_p->publickey_file)-1,
                   strlen (publickey)));

            /* Both the keystore and public key must be present to */
            /* consider the the exchanged key configured */

            if (tnicon_p->exchange_enc_key_state == ENC_CONFIGURED)
            {
                tnicon_p->exchange_enc_key_state = ENC_CONFIGURED;
            }
        }
        else
        {
            if (tnicon_p->exchange_enc_key_state == ENC_CONFIGURED)
            {
                sprintf (err_string.par1,
                         "Data encryption not possible: %s not configured",
                         TNI_CFG_PUBLICKEY);
                output_err ("Process_Control_Section",
                            MI_TNI_STRING,
                            MX_ERR_LVL_INFO,
                            err_string);
            }
            tnicon_p->exchange_enc_key_state = ENC_NOT_CONFIGURED;
        }
    }
    return(rc);
}

/* [Process_Tni_Section]
 *
 * Summary:
 *
 * Process_Tni_Section ()
 *
 * Description:
 *
 * This function parse threw the TNI_CONFIG section of the MX Server
 * configuration file and extracts all the keywords. 
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Process_Tni_Section ()
{
    int                 rc = P_SUCCESS;
    int                 assign_method_def = 0;
    int                 interface_idx = 0;
    int                 temp_int = 0;

    char               *local_domain;
    char               *term_id_method;

    err_string = null_err_string;

    /*
    ** Set config pointer to the top of the buffer
    */
    
    cfgbufp = cfgbuf;

    /*
    ** Find to TNI configuration section
    */

    cfgbufp = tni_cfgsect (TNI_CONFIG, cfgbufp);

    if (cfgbufp == NULL)
    {
        sprintf (err_string.par1, "%s", TNI_CONFIG);

        output_err ("Process_Tni_Section",
                    MI_TNI_NO_SECTION,
                    MX_ERR_LVL_ERROR,
                    err_string);

        rc = P_FAILURE;
    }

    /*
    ** Process Keywords
    */

    if (rc == P_SUCCESS)
    {
        /*
        ** Local UCX host name
        */

        if ((local_domain = tni_cfgstr (TNI_CFGLOCNAM, NULL)) != NULL)
        {
            interface_idx = 0;

            while ((interface_idx < MAX_INTERFACES_PER_TNI) &&
                   (local_domain != NULL))
            {
                if (Conn_Name_Valid (local_domain) == P_SUCCESS)
                {
                    rc = Add_Local_Interface(local_domain);
                }
                else
                {

                    sprintf (err_string.par1, "%s", TNI_CFGLOCNAM);
                    sprintf (err_string.par2, "%s", local_domain);

                    output_err ("Process_Tni_Section",
                                MI_TNI_UNKNOWN,
                                MX_ERR_LVL_ERROR,
                                err_string);
                }
                interface_idx++;
                local_domain = tni_cfgstr (TNI_CFGLOCNAM, local_domain);
            }
        }
        else
        {
            sprintf (err_string.par1, "%s", TNI_CFGLOCNAM);

            output_err ("Tni_Cfg_Conn",
                        MI_TNI_NOKEYWORD,
                        MX_ERR_LVL_ERROR,
                        err_string);
        }
    }

    if (rc == P_SUCCESS)
    {
        /*
        ** Listening port -- if not specified, use default
        */

        temp_int = tni_cfgint (TNI_CFGLISTENPORT, NULL, 10, 'O');

        if (temp_int < 0)
        {
            tnicon_p->listening_port = DEFAULT_LISTEN_PORT;
        }
        else
        {
            tnicon_p->listening_port = temp_int;
        }

        /*
        ** Check for default terminal identification method
        */

        assign_method_def = 0;

        term_id_method = tni_cfgstr (TNI_CFGDTIDMTHD, NULL);

        if (term_id_method != NULL)
        {
            if (strcmp (term_id_method, "TERMINAL_CLIENT_ID") == 0)
            {
                tnicon_p->def_term_id_meth =
                    METH_TERM_CLIENT_ID;
            }
            else if (strcmp (term_id_method, "TERMINAL_CLIENT_TAG") == 0)
            {
                tnicon_p->def_term_id_meth =
                    METH_TERM_CLIENT_TAG;
            }
            else if (strcmp (term_id_method, "TERMINAL_SERVER_ID") == 0)
            {
                tnicon_p->def_term_id_meth =
                    METH_TERM_SERVER_ID;
            }
            else
            {
                assign_method_def = 1;
            }
        }
        else
        {
            assign_method_def = 1;
        }

        if (assign_method_def)
        {
            assign_method_def = 0;
            tnicon_p->def_term_id_meth = DEF_TERM_ID_METH;
        }
    }

    return(rc);
}

/* [Process_App_Section]
 *
 * Summary:
 *
 * Process_App_Section (int *last_conn_assigned)
 *
 * Description:
 *
 * This function parse threw the CLUSTERED_APP section of the MX Server
 * configuration file and extracts all the keywords.
 *
 * Output Arguments:
 *
 * last_conn_assigned - Connection number last assigned 
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Process_App_Section (int *last_conn_assigned)
{
    int                 rc;
    int                 app_num;
    int                 conn_cnt = 0;
    int                 conn_num;
    int                 client_cnt;
    int                 section_cnt = 0;
    int                 more_sections = 1;
    int                 num_good_sections = 0;
    int                 salt_length;

    char               *app_nume;
    char               *remote_name;
    char               *oltp_routing;
    char               *auth_required;
    char               *hash_algorithm;
    char               *es_rpc_enc_mode;


    err_string = null_err_string;

    /*
    ** Set config pointer to the top of the buffer
    */
    
    cfgbufp = cfgbuf;

    while (more_sections)
    {
        rc = P_SUCCESS;
        app_num = 0;
        conn_num = 0;
        client_cnt = 0;
        app_nume = NULL;
        oltp_routing = NULL;
        auth_required = NULL;
        hash_algorithm = NULL;
        es_rpc_enc_mode = NULL;

        /*
        ** Find Clustered Application section
        */

        cfgbufp = tni_cfgsect (TNI_CFGCLUSTAPP, cfgbufp);

        section_cnt++;

        if (cfgbufp == NULL)
        {
            more_sections = 0;

            if (section_cnt == 1)
            {
                sprintf (err_string.par1, "%s", TNI_CFGCLUSTAPP);

                output_err ("Process_App_Section",
                            MI_TNI_NO_SECTION,
                            MX_ERR_LVL_ERROR,
                            err_string);
            }

            rc = P_FAILURE;
        }

        /*
        ** Find avaiable application
        */

        if (rc == P_SUCCESS)
        {
            app_num = Allocate_Application ();

            if (app_num == -1)
            {
                rc = P_FAILURE;
            }
            else
            {
                conn_cnt = 0;
            }
        }

        /*
        ** Process Keywords
        */

        if (rc == P_SUCCESS)
        {
            /*
            ** Application name
            */

            app_nume = tni_cfgstr (TNI_CFGAPPNAME, NULL);

            if (app_nume != NULL)
            {
                if (!Application_Defined (app_nume))
                {
                    memcpy((void *)tnicon_p->app[app_num].name,
                           (void *) app_nume,
                           (size_t) _MIN(sizeof(tnicon_p->app[app_num].name)-1,
                            strlen (app_nume)));
                }
                else
                {
                    Deallocate_Application (app_num);
                    rc = P_FAILURE;
                }
            }
            else
            {
                sprintf(err_string.par1, "%s", TNI_CFGAPPNAME);

                output_err ("Process_App_Section",
                            MI_TNI_NOKEYWORD,
                            MX_ERR_LVL_ERROR,
                            err_string);

                Deallocate_Application (app_num);
                rc = P_FAILURE;
            }
        }

        if (rc == P_SUCCESS)
        {

            /*
            ** OLTP unsolicited messaging routing
            */

            oltp_routing = tni_cfgstr (TNI_CFGOLTPROUT, NULL);

            if (oltp_routing != NULL)
            {
                if (strcmp (oltp_routing, "ROUND_ROBIN") == 0)
                {
                    tnicon_p->app[app_num].oltp_unso_routing =
                        OLTP_ROUTE_ROUND_ROBIN;
                }
                else if (strcmp (oltp_routing, "ALL_CLIENTS") == 0)
                {
                    tnicon_p->app[app_num].oltp_unso_routing =
                        OLTP_ROUTE_ALL_CLIENTS;
                }
                else if (strcmp (oltp_routing, "NONE") == 0)
                {
                    tnicon_p->app[app_num].oltp_unso_routing =
                        OLTP_ROUTE_NONE;
                }
                else
                {
                    sprintf (err_string.par1, "%s", TNI_CFGOLTPROUT);
                    sprintf (err_string.par2, "%s", oltp_routing);

                    output_err ("Process_App_Section",
                            MI_TNI_BADKEYWORD,
                            MX_ERR_LVL_ERROR,
                            err_string);

                    Deallocate_Application (app_num);
                    rc = P_FAILURE;
                }
            }
            else
            {
                tnicon_p->app[app_num].oltp_unso_routing =
                    OLTP_ROUTE_NONE;
            }
        }

        if (rc == P_SUCCESS)
        {
            /*
            ** OLTP request messaging routing
            */

            oltp_routing = tni_cfgstr (TNI_CFGRQSTROUT, NULL);

            if (oltp_routing != NULL)
            {
                if (strcmp (oltp_routing, "ROUND_ROBIN") == 0)
                {
                    tnicon_p->app[app_num].oltp_rqst_routing =
                        OLTP_ROUTE_ROUND_ROBIN;
                }
                else if (strcmp (oltp_routing, "ALL_CLIENTS") == 0)
                {
                    tnicon_p->app[app_num].oltp_rqst_routing =
                        OLTP_ROUTE_ALL_CLIENTS;
                }
                else if (strcmp (oltp_routing, "NONE") == 0)
                {
                    tnicon_p->app[app_num].oltp_rqst_routing =
                        OLTP_ROUTE_NONE;
                }
                else
                {
                    sprintf (err_string.par1, "%s", TNI_CFGRQSTROUT);
                    sprintf (err_string.par2, "%s", oltp_routing);

                    output_err ("Process_App_Section",
                            MI_TNI_BADKEYWORD,
                            MX_ERR_LVL_ERROR,
                            err_string);

                    Deallocate_Application (app_num);
                    rc = P_FAILURE;
                }
            }
            else
            {
                tnicon_p->app[app_num].oltp_rqst_routing =
                    OLTP_ROUTE_NONE;
            }
        }

        if (rc == P_SUCCESS)
        {

            /*
            ** RPC request timeout
            */

            if ((tnicon_p->app[app_num].rpc_rqst_timeout =
                 tni_cfgint (TNI_CFGRPCRQST, NULL, 10, 'O')) < 1)
            {
                tnicon_p->app[app_num].rpc_rqst_timeout = 
                    tnicon_p->def_rpc_rqst_timeout;
            }

            /*
            ** Remote domain names 
            */

            remote_name = NULL;

            while ((remote_name = tni_cfgstr (TNI_CFGREMNAM, remote_name))
                   != NULL)
            {
                conn_cnt++;

                if (Conn_Name_Valid (remote_name) == P_SUCCESS)
                {
                    conn_num = Find_Unused_Connection ();

                    if (conn_num != -1)
                    {
                        rc = Assign_Conn_To_App (conn_num, app_num);

                        if (rc == P_SUCCESS)
                        {
                            memcpy((void *)tnicon_p->connection[conn_num].remote_domain_name,
                                   (void *) remote_name,
                                   (size_t) _MIN(sizeof(tnicon_p->connection[conn_num].remote_domain_name)-1,
                                   strlen (remote_name)));

                            tnicon_p->connection[conn_num].conn_state = CONN_DEFINED;

                            tnicon_p->connection[conn_num].managed_by =
                                CONNS_APP_NAME;

                            client_cnt++;
                            *last_conn_assigned = conn_num;
                        }
                        else
                        {
                            sprintf (err_string.par1, "client connection");
                            sprintf (err_string.par2, "%d", conn_cnt);
                            sprintf (err_string.par3,
                                     "Application %s",
                                     tnicon_p->app[app_num].name);

                            output_err ("Process_App_Section",
                                        MI_TNI_ERASE_CONN,
                                        MX_ERR_LVL_WARNING,
                                        err_string);
                        } 
                    }
                    else
                    {
                        sprintf (err_string.par1, tnicon_p->app[app_num].name);
                        sprintf (err_string.par2, "%d", conn_cnt);

                        output_err ("Process_App_Section",
                                    MI_TNI_NOCONFIG_CONN,
                                    MX_ERR_LVL_WARNING,
                                    err_string);
                    }
                }
                else
                {
                    sprintf (err_string.par1, "%s", TNI_CFGREMNAM);
                    sprintf (err_string.par2, "%s", remote_name);

                    output_err ("Process_App_Section",
                                MI_TNI_UNKNOWN,
                                MX_ERR_LVL_ERROR,
                                err_string);

                    sprintf (err_string.par1, "client connection");
                    sprintf (err_string.par2, "%d", conn_cnt);
                    sprintf (err_string.par3,
                             "Application %s",
                             tnicon_p->app[app_num].name);

                    output_err ("Process_App_Section",
                                MI_TNI_ERASE_CONN,
                                MX_ERR_LVL_WARNING,
                                err_string); 
                }
            }
 
            if (client_cnt == 0)
            {
                Deallocate_Application (app_num);

                sprintf (err_string.par1, tnicon_p->app[app_num].name);

                output_err ("Process_App_Section",
                            MI_TNI_NODEF_CONNS,
                            MX_ERR_LVL_WARNING,
                            err_string);

                rc = P_FAILURE;
            }
            else
            {
                rc = P_SUCCESS;
            } 
        }

        if (rc == P_SUCCESS)
        {
            /*
            ** AUTHENTICATION REQUIRED PARAMETER
            */
            auth_required = tni_cfgstr (TNI_CFGAUTHREQ, NULL);

            if (auth_required != NULL)
            {
                if (strcasecmp (auth_required , "TRUE") == 0)
                {
                    tnicon_p->app[app_num].auth_required = 1;
                }
                else if (strcasecmp (auth_required, "FALSE") == 0)
                {
                    tnicon_p->app[app_num].auth_required = 0;
                }
                else
                {
                    sprintf (err_string.par1, "%s", TNI_CFGAUTHREQ);
                    sprintf (err_string.par2, "%s", auth_required);

                    output_err ("Process_App_Section",
                            MI_TNI_BADKEYWORD,
                            MX_ERR_LVL_ERROR,
                            err_string);

                    Deallocate_Application (app_num);
                    rc = P_FAILURE;
                }
            }
            else
            {
                /* Default is TRUE */
                tnicon_p->app[app_num].auth_required = 1;
            }

        }

        if (rc == P_SUCCESS)
        {
            /*
            ** HASH ALGORITHM PARAMETER
            */
            hash_algorithm = tni_cfgstr (TNI_CFGHASHALG, NULL);

            if (hash_algorithm != NULL)
            {
                if (strcasecmp (hash_algorithm, "SHA1") == 0)
                {
                    tnicon_p->app[app_num].auth_hash_algorithm = SHA1_HASH;
                }
                else if (strcasecmp (hash_algorithm, "MD5") == 0)
                {
                    tnicon_p->app[app_num].auth_hash_algorithm = MD5_HASH;
                }
                else if (strcasecmp (hash_algorithm, "SHA256") == 0)
                {
                    tnicon_p->app[app_num].auth_hash_algorithm = SHA256_HASH;
                }
                else if (strcasecmp (hash_algorithm, "SHA384") == 0)
                {
                    tnicon_p->app[app_num].auth_hash_algorithm = SHA384_HASH;
                }
                else if (strcasecmp (hash_algorithm, "SHA512") == 0)
                {
                    tnicon_p->app[app_num].auth_hash_algorithm = SHA512_HASH;
                }
                else
                {
                    sprintf (err_string.par1, "%s", TNI_CFGHASHALG);
                    sprintf (err_string.par2, "%s", hash_algorithm);

                    output_err ("Process_App_Section",
                            MI_TNI_BADKEYWORD,
                            MX_ERR_LVL_ERROR,
                            err_string);

                    Deallocate_Application (app_num);
                    rc = P_FAILURE;
                }
            }
            else
            {
                tnicon_p->app[app_num].auth_hash_algorithm = DEF_AUTH_HASH_ALGORITHM;
            }

        }

        if (rc == P_SUCCESS)
        {
            /*
            ** SALT LENGTH PARAMETER
            */

            salt_length = tni_cfgint (TNI_CFGSALTLEN, NULL, 10, 'O');

            if (salt_length == -1)
            {
                tnicon_p->app[app_num].auth_salt_length = DEF_AUTH_SALT_LENGTH;
            }
            else if ((salt_length < MIN_AUTH_SALT_LENGTH) ||
                    (salt_length > MAX_AUTH_SALT_LENGTH))
            {

                sprintf (err_string.par1, "%s", TNI_CFGSALTLEN);
                sprintf (err_string.par2, "%d", salt_length);

                output_err ("Process_App_Section",
                        MI_TNI_BADKEYWORD,
                        MX_ERR_LVL_ERROR,
                        err_string);

                Deallocate_Application (app_num);
                rc = P_FAILURE;
            }
            else
            {
                tnicon_p->app[app_num].auth_salt_length = salt_length;
            }

        }

        if (rc == P_SUCCESS)
        {
            /*
            ** ES RPC encryption mode
            */

            es_rpc_enc_mode = tni_cfgstr (TNI_CFGESRPCENC, NULL);

            if (es_rpc_enc_mode != NULL)
            {
                if (strcasecmp (es_rpc_enc_mode, "DISABLE") == 0)
                {
                    tnicon_p->app[app_num].es_rpc_enc_mode = ENC_DISABLED;
                    tnicon_p->app[app_num].es_rpc_enc_state = ENC_NOT_CONFIGURED;
                }
                else if (strcasecmp (es_rpc_enc_mode, "ON_DEMAND") == 0)
                {
                    tnicon_p->app[app_num].es_rpc_enc_mode = ENC_ON_DEMAND;
                    tnicon_p->app[app_num].es_rpc_enc_state = ENC_CONFIGURED;
                }
                else if (strcasecmp (es_rpc_enc_mode, "ALL_MESSAGES") == 0)
                {
                    tnicon_p->app[app_num].es_rpc_enc_mode = ENC_ALL_MSGS;
                    tnicon_p->app[app_num].es_rpc_enc_state = ENC_CONFIGURED;
                }
                else
                {
                    sprintf (err_string.par1, "%s", TNI_CFGESRPCENC);
                    sprintf (err_string.par2, "%s", es_rpc_enc_mode);

                    output_err ("Process_App_Section",
                            MI_TNI_BADKEYWORD,
                            MX_ERR_LVL_ERROR,
                            err_string);

                    Deallocate_Application (app_num);
                    rc = P_FAILURE;
                }
            }
            else
            {
                tnicon_p->app[app_num].es_rpc_enc_mode = ENC_DISABLED;
                tnicon_p->app[app_num].es_rpc_enc_state = ENC_NOT_CONFIGURED;
            }
        }

        if (rc == P_SUCCESS)
        {
            num_good_sections++;
        }
        else
        {
            if (more_sections == 1)
            {
                sprintf (err_string.par1, "%d", section_cnt);

                output_err ("Process_App_Section",
                            MI_TNI_REMOVE_APP,
                            MX_ERR_LVL_WARNING,
                            err_string);
            }
        }
    }

    /* Check if at least one connection is defined is done after tni_server.fil is processed */
    
    rc = P_SUCCESS;

    return(rc);
}


/* [Process_Buffer_Section]
 *
 * Summary:
 *
 * Process_Buffer_Section(void)
 *
 * Description:
 *
 * This function parses the BUFFER_PARAMETERS section  of the MX Server
 * configuration file and extracts all the keywords.
 *
 * Output Arguments:
 *
 * None
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */
int
Process_Buffer_Section(void)
{
    int     rc = P_SUCCESS;
    int     temp_size, temp_num, temp_thold;

    /*
    ** Set config pointer to the top of the buffer
    */
    
    cfgbufp = cfgbuf;

    /* Read BUFFER_PARAMETERS Section if present */

    if (tni_cfgsect (TNI_CFGBUFPARAMS, cfgbufp) != NULL) 
    {

        /* Read POOL1 parameters */
        temp_size = tni_cfgint (TNI_CFGP1BSIZE,  NULL, 10, 'O');
        temp_num = tni_cfgint (TNI_CFGP1BNUM,  NULL, 10, 'O');
        temp_thold = tni_cfgint (TNI_CFGP1BTHOLD,  NULL, 10, 'O');

        if(temp_size >= 0)
        {
            pool1_buf_size = temp_size;
        }

        if(temp_num >= 0)
        {
            pool1_buf_num = temp_num;
        }

        if(temp_thold >= 0)
        {
            pool1_buf_thold = temp_thold;
        }

        if(pool1_buf_thold >= pool1_buf_num)
        {
            sprintf(err_string.par1,
                    "%s",
                    "Invalid pool1 buffer parameters, Assigning default values");

            output_err ("Process_Buffer_Section",
                       MI_TNI_STRING,
                       MX_ERR_LVL_ERROR,
                       err_string);

            pool1_buf_size  = DEF_POOL1_BUF_SIZE;
            pool1_buf_num   = DEF_POOL1_BUF_NUM;
            pool1_buf_thold = DEF_POOL1_BUF_THOLD;
        }

        /* Read POOL2 parameters */
        temp_size = tni_cfgint (TNI_CFGP2BSIZE,  NULL, 10, 'O');
        temp_num = tni_cfgint (TNI_CFGP2BNUM,  NULL, 10, 'O');
        temp_thold = tni_cfgint (TNI_CFGP2BTHOLD,  NULL, 10, 'O');

        if(temp_size >= 0)
        {
            pool2_buf_size = temp_size;
        }

        if(temp_num >= 0)
        {
            pool2_buf_num = temp_num;
        }

        if(temp_thold >= 0)
        {
            pool2_buf_thold = temp_thold;
        }

        if(pool2_buf_thold >= pool2_buf_num)
        {
            sprintf(err_string.par1,
                    "%s",
                    "Invalid pool2 buffer parameters, Assigning default values");

            output_err ("Process_Buffer_Section",
                       MI_TNI_STRING,
                       MX_ERR_LVL_ERROR,
                       err_string);

            pool2_buf_size  = DEF_POOL2_BUF_SIZE;
            pool2_buf_num   = DEF_POOL2_BUF_NUM;
            pool2_buf_thold = DEF_POOL2_BUF_THOLD;
        }

        /* Read POOL3 parameters */
        temp_size = tni_cfgint (TNI_CFGP3BSIZE,  NULL, 10, 'O');
        temp_num = tni_cfgint (TNI_CFGP3BNUM,  NULL, 10, 'O');
        temp_thold = tni_cfgint (TNI_CFGP3BTHOLD,  NULL, 10, 'O');

        if(temp_size >= 0)
        {
            pool3_buf_size = temp_size;
        }

        if(temp_num >= 0)
        {
            pool3_buf_num = temp_num;
        }

        if(temp_thold >= 0)
        {
            pool3_buf_thold = temp_thold;
        }

        if(pool3_buf_thold >= pool3_buf_num)
        {
            sprintf(err_string.par1,
                    "%s",
                    "Invalid pool3 buffer parameters, Assigning default values");

            output_err ("Process_Buffer_Section",
                       MI_TNI_STRING,
                       MX_ERR_LVL_ERROR,
                       err_string);

            pool3_buf_size  = DEF_POOL3_BUF_SIZE;
            pool3_buf_num   = DEF_POOL3_BUF_NUM;
            pool3_buf_thold = DEF_POOL3_BUF_THOLD;
        }

        /* Read POOL4 parameters */
        temp_size = tni_cfgint (TNI_CFGP4BSIZE,  NULL, 10, 'O');
        temp_num = tni_cfgint (TNI_CFGP4BNUM,  NULL, 10, 'O');
        temp_thold = tni_cfgint (TNI_CFGP4BTHOLD,  NULL, 10, 'O');

        if(temp_size >= 0)
        {
            pool4_buf_size = temp_size;
        }

        if(temp_num >= 0)
        {
            pool4_buf_num = temp_num;
        }

        if(temp_thold >= 0)
        {
            pool4_buf_thold = temp_thold;
        }

        if(pool4_buf_thold >= pool4_buf_num)
        {
            sprintf(err_string.par4,
                    "%s",
                    "Invalid pool4 buffer parameters, Assigning default values");

            output_err ("Process_Buffer_Section",
                       MI_TNI_STRING,
                       MX_ERR_LVL_ERROR,
                       err_string);

            pool4_buf_size  = DEF_POOL4_BUF_SIZE;
            pool4_buf_num   = DEF_POOL4_BUF_NUM;
            pool4_buf_thold = DEF_POOL4_BUF_THOLD;
        }

        /* Read POOL5 parameters */
        temp_size = tni_cfgint (TNI_CFGP5BSIZE,  NULL, 10, 'O');
        temp_num = tni_cfgint (TNI_CFGP5BNUM,  NULL, 10, 'O');
        temp_thold = tni_cfgint (TNI_CFGP5BTHOLD,  NULL, 10, 'O');

        if(temp_size >= 0)
        {
            pool5_buf_size = temp_size;
        }

        if(temp_num >= 0)
        {
            pool5_buf_num = temp_num;
        }

        if(temp_thold >= 0)
        {
            pool5_buf_thold = temp_thold;
        }

        if((pool5_buf_thold >= pool5_buf_num) || (pool5_buf_thold < (MAX_CONN/2)))
        {
            sprintf(err_string.par1,
                    "%s",
                    "Invalid pool5 buffer parameters, Assigning default values");

            output_err ("Process_Buffer_Section",
                       MI_TNI_STRING,
                       MX_ERR_LVL_ERROR,
                       err_string);

            pool5_buf_size  = DEF_POOL5_BUF_SIZE;
            pool5_buf_num   = DEF_POOL5_BUF_NUM;
            pool5_buf_thold = DEF_POOL5_BUF_THOLD;
        }

        /*set max_pdu_size_val to POOL5 size */

        if(pool5_buf_size > 0)
        {
            max_pdu_size_val = pool5_buf_size;
        }
        else
        {
            max_pdu_size_val = DEF_MAX_PDU_SIZE_VAL;
        }
    }
    else
    {
        /* If the BUFFER_PARAMETERS section not found, its understood we use default parameters */
        /* No messages to ELOG */
#if 0
        /* Send information to ELOG that default parameters will be used */

        sprintf (err_string.par1, "%s", TNI_CFGBUFPARAMS);

        output_err ("Process_Buffer_Section",
                       MI_TNI_NO_SECTION,
                       MX_ERR_LVL_INFO,
                       err_string);

        sprintf(err_string.par1, "%s", "Assigning default values for buffer parameters");

        output_err ("Process_Buffer_Section",
                       MI_TNI_STRING,
                       MX_ERR_LVL_INFO,
                       err_string);
#endif
    }

    return rc;
}



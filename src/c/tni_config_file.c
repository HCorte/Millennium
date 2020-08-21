static const char *fileid = "";

/*
 * ===[tni_config_file.c]================================================= 
 *
 * Description:
 *
 * Functions to parse the tni_server.fil file.
 *
 * Functions:
 *
 * Open_Tni_Config_File
 * Process_Control_Section_Tni
 * Process_Tni_Section_Tni
 * Process_Conn_Section_Tni
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

#include "includes.h"
#include "tnicfg.h"

#if defined(XOS_VMS)

#   include <limits.h>
#   include <ssdef.h>

#endif

/*
** External variables
*/

    static FILE        *fp;
    static char        *cfgbufp;
    static char         cfgbuf[TNI_CFGBUFLEN];

/* [Open_Tni_Config_File]
 *
 * Summary:
 *
 * Open_Tni_Config_File ()
 *
 * Description:
 *
 * This function opens the TNI Server configuration file.  The contents of
 * the file are read into cfgbuf.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Open_Tni_Config_File () 
{
    int                 rc = P_SUCCESS;
    int                 cfglen = -1;
    char               *cfgnamep;
    char                strbuf[256];

    err_string = null_err_string;

    cfgnamep = TNI_SERVER_CFG_NAME;

#   if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

        g_translate (cfgnamep, strbuf);
        cfgnamep = strbuf;

#   endif

    /*
    ** Set up pointer to configuration buffer
    */

    cfgbufp = cfgbuf;

    /*
    ** Open TNI Server configuration file
    */

    if ((fp = fopen (cfgnamep, "r")) == NULL)
    {
        if (errno != ENOENT)
        {
            sprintf (err_string.par1, "Opening");
            sprintf (err_string.par2, "%s", cfgnamep);

            output_err ("Open_Tni_Config_File",
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
    ** Read in TNI Server configuration file
    */

    if (rc == P_SUCCESS)
    {
        if ((cfglen = tni_cfgread (fp, cfgbuf, sizeof (cfgbuf))) < 0)
        {
            sprintf (err_string.par1, "%s", cfgnamep);

            output_err ("Open_Tni_Config_File",
                        MI_TNI_INV_FORMAT,
                        MX_ERR_LVL_ERROR,
                        err_string);

            rc = P_FAILURE;
        }
        fclose (fp);
    }

    return(rc);
}

/* [Process_Control_Section_Tni]
 *
 * Summary:
 *
 * Process_Control_Section_Tni (int mx_config_valid,
 *                              int *num_conns)
 *
 * Description:
 *
 * This function parse threw the CONTROL section of the TNI Server
 * configuration file and extracts all the keywords. 
 *
 * Input Arguments:
 *
 * mx_config_valid  -  Boolean value indicating the mx_server.fil was
 *                     processed successfully
 *
 * Output Arguments:
 *
 * num_conns        -  Number of connections to be initialized
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Process_Control_Section_Tni (int mx_config_valid,
                             int *num_conns)
{
    int                 rc = P_SUCCESS;
    char               *str_value;

    err_string = null_err_string;

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
        ** Number of connections
        */

        *num_conns = tni_cfgint (TNI_CFGCONCNT, NULL, 10, 'M');

        if (*num_conns < 0)
        {
            rc = P_FAILURE;
        }

        /*
        ** Check for unsolicited message delivery failure notification
        */

        str_value = tni_cfgstr (TNI_CFGUNSONOTIFY, NULL);

        if (str_value != NULL) {

            if (strcasecmp (str_value, "ENABLE") == 0)
            {
                tnicon_p->unso_failure_notify1 = MX_ENABLED;
            }

            else if (strcasecmp (str_value, "DISABLE") == 0)
            {
                tnicon_p->unso_failure_notify1 = MX_DISABLED;
            }

           else
            {
                tnicon_p->unso_failure_notify1 = MX_ENABLED;

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
            tnicon_p->unso_failure_notify1 = MX_ENABLED;
        }

        /*
        ** Only read remaining keywords if the processing of the mx_server.fil
        ** file was unsuccessful.
        */

        if ((rc == P_SUCCESS) && 
            (mx_config_valid == P_FAILURE))
        {
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
            ** Check for write select timeout interval (milliseconds)
            */
            tnicon_p->wrt_select_tout_invl =
                tni_cfgint (TNI_CFGWRTSELECT, NULL, 10, 'O');

            if (tnicon_p->wrt_select_tout_invl == -1)
            {                                       /* keyword not present */
                tnicon_p->wrt_select_timeout.tv_sec = 0;
                tnicon_p->wrt_select_timeout.tv_usec = DEF_WRT_SELECT_TIMEOUT;
            }                                       /* keyword not present */
            else if ((tnicon_p->wrt_select_tout_invl == 0) ||
                     (tnicon_p->wrt_select_tout_invl == 999))
            {                                       /* 0 => don't perform Write Select, 999 => Do Select, wait indefinitely */
                tnicon_p->wrt_select_timeout.tv_sec = 0;
                tnicon_p->wrt_select_timeout.tv_usec = 0;
            }                                       /* 0 => don't perform Write Select, 999 => Do Select, wait indefinitely */
            else if ((tnicon_p->wrt_select_tout_invl < MIN_WRT_SELECT_TOUT_INVL)||
                     (tnicon_p->wrt_select_tout_invl > MAX_WRT_SELECT_TOUT_INVL))
            {                                       /* not within valid Select Timeout range - impose default */
                tnicon_p->wrt_select_tout_invl = -1;
                tnicon_p->wrt_select_timeout.tv_sec = 0;
                tnicon_p->wrt_select_timeout.tv_usec = DEF_WRT_SELECT_TIMEOUT;

                sprintf (err_string.par1, ".001");
                sprintf (err_string.par2, "%s", TNI_CFGWRTSELECT);

                output_err ("Process_Control_Section",
                            MI_TNI_DEF_VALUE,
                            MX_ERR_LVL_INFO,
                            err_string);
            }                                       /* not within valid Select Timeout range - impose default */
            else
            {                                       /* valid Select Timeout range */
                tnicon_p->wrt_select_timeout.tv_sec = 0;
                tnicon_p->wrt_select_timeout.tv_usec = tnicon_p->wrt_select_tout_invl * 1000;
            }                                       /* valid Select Timeout range */
        } 
    }

    /*
    ** Set pointer back to the top of the buffer
    */

    cfgbufp = cfgbuf;

    return (rc);
}

/* [Process_Tni_Section_Tni]
 *
 * Summary:
 *
 * Process_Tni_Section_Tni (int mx_config_valid)
 *
 * Description:
 *
 * This function parse threw the TNI_CONFIG section of the MX Server
 * configuration file and extracts all the keywords.
 *
 * Input Arguments:
 *
 * mx_config_valid  -  Boolean value indicating the mx_server.fil was
 *                     processed successfully
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Process_Tni_Section_Tni (int mx_config_valid)
{
    int                 rc = P_SUCCESS;
    int                 assign_method_def = 0;
    int                 prim_notify_mode;
    int                 temp_int = 0;

    char               *start_mode;
    char               *logging;
    char               *term_id_method;

    struct timeb        local_time;

    err_string = null_err_string;

    /*
    ** Find to configuration section
    */

    if (tni_cfgsect (TNI_CONFIG, cfgbufp) == NULL) {

        sprintf (err_string.par1, "%s", TNI_CONFIG);

        output_err ("Process_Tni_Section_Tni",
                    MI_TNI_NO_SECTION,
                    MX_ERR_LVL_ERROR,
                    err_string);

        rc = P_FAILURE;
    }

    /*
    ** Only read remaining keywords if the processing of the mx_server.fil
    ** file was unsuccessful and the section header is present.
    */

    if ((rc == P_SUCCESS) && 
        (mx_config_valid == P_FAILURE))
    {
        ftime (&local_time);

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

            output_err ("Process_Tni_Section_Tni",
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

            output_err ("Process_Tni_Section_Tni",
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

            output_err ("Process_Tni_Section_Tni",
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

            output_err ("Process_Tni_Section_Tni",
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
 
            output_err ("Process_Tni_Section_Tni",
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

            output_err ("Process_Tni_Section_Tni",
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

            output_err ("Process_Tni_Section_Tni",
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

            output_err ("Process_Tni_Section_Tni",
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

                output_err ("Process_Tni_Section_Tni",
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

                output_err ("Process_Tni_Section_Tni",
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

                output_err ("Process_Tni_Section_Tni",
                            MI_TNI_DEF_VALUE,
                            MX_ERR_LVL_INFO,
                            err_string);
            }
        }
        else
        {
            tnicon_p->mf_log_state = LOG_OFF;
        }

    }
 
    /*
    ** Set pointer back to the top of the buffer
    */
    
    cfgbufp = cfgbuf;

    return(rc);
}

/* [Process_Conn_Section_Tni]
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
Process_Conn_Section_Tni (int num_conns,
                          int first_available_conn_num)
{
    int                 rc = P_SUCCESS;
    int                 client_id = 0;
    int                 configurable_conns = 0;
    int                 configured_conns = 0;
    int                 conn_idx = 0;
    int                 conn_cnt = 0;
    int                 num_available_conns = 0;

    char               *local_domain;
    char               *remote_domain;
    char               *remote_system;

    err_string = null_err_string;

    /*
    ** Determine if there is enough room in the connection table to place
    ** the number of connections defined in the tni_server.fil file.  If
    ** there is not enough room, fit as many as you can.
    */

    num_available_conns = MAX_CONFIGURABLE_CONN - first_available_conn_num + 1;

    if (num_conns > num_available_conns)
    {
        configurable_conns = num_available_conns;
    }
    else
    {
        configurable_conns = num_conns;
    }

    conn_idx = first_available_conn_num;

    for (conn_cnt = 0; conn_cnt < configurable_conns; conn_cnt++)
    {
        /*
        ** Find to connection configuration section
        */

        cfgbufp = tni_cfgsect (TNI_CFGCONN, cfgbufp);

        if (cfgbufp == NULL)
        {
            sprintf (err_string.par1, "%s", TNI_CFGCONN);

            output_err ("Process_Conn_Section_Tni",
                        MI_TNI_NO_SECTION,
                        MX_ERR_LVL_ERROR,
                        err_string);

            rc = P_FAILURE;
        }
        else
        {
            rc = P_SUCCESS;
        }

        /*
        ** Process Keywords
        */

        if (rc == P_SUCCESS)
        {
            /*
            ** Client Id
            */

            client_id = tni_cfgint (TNI_CFGHOSTID, NULL, 10, 'M');

            if (client_id == -1)
            {
                rc = P_FAILURE;
            }
            else if ((client_id < MIN_CLIENT_ID_VAL) ||
                     (client_id > MAX_CLIENT_ID_VAL))
            {
                sprintf(err_string.par1,"%s",TNI_CFGHOSTID);
                sprintf(err_string.par2,"%d",client_id);
                sprintf(err_string.par3,"%d",MIN_CLIENT_ID_VAL);
                sprintf(err_string.par4,"%d",MAX_CLIENT_ID_VAL);

                output_err("Process_Conn_Section_Tni",
                           MI_TNI_BADPARAMETER,
                           MX_ERR_LVL_ERROR,
                           err_string);

                rc = P_FAILURE;
            }
            else
            {
                tnicon_p->connection[conn_idx].client_id_val = client_id;
            }

            /*
            ** Local domain name
            */

            if (rc == P_SUCCESS)
            {
                if ((local_domain = tni_cfgstr (TNI_CFGLOCNAM, NULL)) != NULL)
                {
                    if (Conn_Name_Valid (local_domain) == P_SUCCESS)
                    {
                        rc = Add_Local_Interface(local_domain);

                        if (rc == P_SUCCESS)
                        {
                            memcpy((void *) tnicon_p->connection[conn_idx].local_domain_name,
                                   (void *) local_domain,
                                   (size_t) _MIN(sizeof(tnicon_p->connection[conn_idx].local_domain_name)-1,
                                                        strlen(local_domain))); 
                        }
                    }
                    else
                    {
                        sprintf (err_string.par1, "%s", TNI_CFGLOCNAM);
                        sprintf (err_string.par2, "%s", local_domain);

                        output_err ("Process_Conn_Section_Tni",
                                    MI_TNI_UNKNOWN,
                                    MX_ERR_LVL_ERROR,
                                    err_string);

                        rc = P_FAILURE;
                    }
                }
                else
                {
                    sprintf (err_string.par1, "%s", TNI_CFGLOCNAM);

                    output_err ("Process_Conn_Section_Tni",
                                MI_TNI_NOKEYWORD,
                                MX_ERR_LVL_ERROR,
                                err_string);

                    rc = P_FAILURE;
                }
            }

            /*
            ** Remote domain name
            */

            if (rc == P_SUCCESS)
            {
                if ((remote_domain = tni_cfgstr (TNI_CFGREMNAM, NULL)) != NULL)
                {
                    if (Conn_Name_Valid (remote_domain) == P_SUCCESS)
                    {
                        memcpy((void *)tnicon_p->connection[conn_idx].remote_domain_name,
                               (void *) remote_domain,
                               (size_t) _MIN(sizeof(tnicon_p->connection[conn_idx].remote_domain_name)-1,
                                                    strlen (remote_domain)));
                    }
                    else
                    {
                        sprintf (err_string.par1, "%s", TNI_CFGREMNAM);
                        sprintf (err_string.par2, "%s", remote_domain);

                        output_err ("Process_Conn_Section_Tni",
                                    MI_TNI_UNKNOWN,
                                    MX_ERR_LVL_ERROR,
                                    err_string);

                        rc = P_FAILURE;
                    }
                }
                else
                {
                    sprintf (err_string.par1, "%s", TNI_CFGREMNAM);

                    output_err ("Process_Conn_Section_Tni",
                                MI_TNI_NOKEYWORD,
                                MX_ERR_LVL_ERROR,
                                err_string);

                    rc = P_FAILURE;
                }
            }

            /*
            ** Remote system name
            */

            if (rc == P_SUCCESS)
            {
                if ((remote_system = tni_cfgstr (TNI_CFGREMSYS,NULL)) == NULL)
                {
                    sprintf(err_string.par1,"%s",TNI_CFGREMSYS);

                    output_err("Process_Conn_Section_Tni",
                               MI_TNI_NOKEYWORD,
                               MX_ERR_LVL_ERROR,
                               err_string);

                    rc = P_FAILURE;
                }
                else
                {
                    remote_system = strupper(remote_system);

                    memcpy((void *)tnicon_p->connection[conn_idx].remote_node_name,
                           (void *) remote_system,
                           (size_t) _MIN(sizeof(tnicon_p->connection[conn_idx].remote_node_name)-1,
                                                strlen (remote_system)));
                }
            }

            if (rc == P_SUCCESS)
            {
                conn_idx++;
            }
            else
            {
                tnicon_p->connection[conn_idx].client_id_val = -1;

                sprintf(err_string.par1, "connection");
                sprintf(err_string.par2,
                        "%d",
                        first_available_conn_num + conn_cnt);
                sprintf(err_string.par3, "%s", TNI_SERVER_CFG_NAME);

                output_err("Process_Conn_Section_Tni",
                           MI_TNI_ERASE_CONN,
                           MX_ERR_LVL_WARNING,
                           err_string);

                /*
                ** Making sure at least one connection has been defined is done after processing
                */
                rc = P_SUCCESS;
                
            }
        }
    }

    /*
    ** Make sure at least one connection has been defined
    */

    if (conn_idx == first_available_conn_num && first_available_conn_num == 1)
    {
        sprintf(err_string.par1, "%s", SERVER_CFG_NAME);

        output_err("Process_Conn_Section_Tni",
                   MI_TNI_NO_CONNS,
                   MX_ERR_LVL_ERROR,
                   err_string);
        
        sprintf(err_string.par1, "%s", TNI_SERVER_CFG_NAME);

        output_err("Process_Conn_Section_Tni",
                   MI_TNI_NO_CONNS,
                   MX_ERR_LVL_ERROR,
                   err_string);

        rc = P_FAILURE;
    }

    return(rc);
}

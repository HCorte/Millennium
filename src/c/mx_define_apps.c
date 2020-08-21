static const char *fileid = "";

/*
 * ===[mx_define_apps.c]==================================================
 *
 * Description:
 *
 * Functions to parse the mx_server.fil file.
 *
 * Functions:
 *
 * Allocate_Application
 * Deallocate_Application
 * Assign_Conn_To_App
 * Application_Defined 
 * Find_Application_Number
 * Deassigned_Conn_From_App
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

#include <string.h>

#include "includes.h"

/* [Allocate_Application]
 *
 * Summary:
 *
 * Allocate_Application ()
 *
 * Description:
 *
 * This function returns the number of an application in the UNDEFINDED state.
 * If all applications are DEFINED a -1 is returned.
 *
 * Returns Values:
 *
 * Number of available application or -1 if none are available
 *
 */

int
Allocate_Application ()
{
    int                 app = -1;
    int                 app_idx = 0;
    int                 found_app = 0;

    err_string = null_err_string;

    while ((app_idx < MAX_APPS) && (!found_app))
    {
        if (tnicon_p->app[app_idx].state == APP_UNDEFINED)
        {
             tnicon_p->app[app_idx].state = APP_DEFINED;
             app = app_idx;
             found_app = 1;
        }

        app_idx++;
    }

    if (app == -1)
    {
        output_err ("Allocate_Application",
                    MI_TNI_NOALLO_APP,
                    MX_ERR_LVL_ERROR,
                    err_string);
    }

    return(app);
}

/* [Deallocate_Application]
 *
 * Summary:
 *
 * Deallocate_Application ()
 *
 * Description:
 *
 * This function returns the inputted application to the UNDEFINDED state.
 *
 * Returns Values:
 *
 * None
 *
 */

void
Deallocate_Application (int app_idx)
{
    int                 client_idx = 0;

    if ((app_idx >= 0) && (app_idx < MAX_APPS))
    {
        tnicon_p->app[app_idx].state = APP_UNDEFINED;

        memset (tnicon_p->app[app_idx].name,
                '\0',
                sizeof (tnicon_p->app[app_idx].name));

        for (client_idx = 0; client_idx < MAX_CONN_PER_APP; client_idx++)
        {
            tnicon_p->app[app_idx].client_conn[client_idx] = 0;
        }
    }

    return;
}

/* [Assign_Conn_To_App]
 *
 * Summary:
 *
 * Assign_Conn_To_App (int conn_num,
 *                   int app_num )
 *
 * Input Arguments:
 *
 * conn_num    - Connection number
 * app_num     - Application number
 *
 * Description:
 *
 * This function assigned a connection to an application.  The
 * application must be in the DEFINED state.  The connection must
 * not be assigned to any other application.  If the connect is found
 * to already be assgined to the requested application, then no
 * error is returned.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Assign_Conn_To_App (int conn_num, 
                    int app_num)
{
    int                 rc = P_SUCCESS;
    int                 app_assigned_to_conn = -1;
    int                 client_idx = 0;
    int                 app_idx = 0;
    int                 found_conn = 0;

    err_string = null_err_string;

    /*
    ** Verify connection number is within the proper range
    */

    if ((conn_num < 1) || (conn_num > MAX_CONFIGURABLE_CONN))
    {
        sprintf (err_string.par1, "%d", conn_num);
        sprintf (err_string.par2, "%d", app_num);
        sprintf (err_string.par3, "%d", 1);
        sprintf (err_string.par4, "%d", MAX_CONFIGURABLE_CONN);

        output_err ("Assign_Conn_To_App",
                    MI_TNI_BAD_CONN_RNG,
                    MX_ERR_LVL_ERROR,
                    err_string);

        rc = P_FAILURE; 
    }

    /*
    ** Verify application number is within the proper range
    */

    if (rc == P_SUCCESS)
    {
        if ((app_num < 0) || (app_num > MAX_APPS))
        {
            sprintf (err_string.par1, "%d", conn_num);
            sprintf (err_string.par2, "%d", app_num);
            sprintf (err_string.par3, "%d", 0);
            sprintf (err_string.par4, "%d", MAX_APPS);

            output_err ("Assign_Conn_To_App",
                        MI_TNI_BAD_APP_RNG,
                        MX_ERR_LVL_ERROR,
                        err_string);

            rc = P_FAILURE;
        }
    }

    /*
    ** Verify application is in the DEFINED state
    */

    if (rc == P_SUCCESS)
    {
        if (tnicon_p->app[app_num].state != APP_DEFINED)
        {
            sprintf (err_string.par1, "%d", conn_num);
            sprintf (err_string.par2, "%d", app_num);

            output_err ("Assign_Conn_To_App",
                        MI_TNI_APP_NOTDEF,
                        MX_ERR_LVL_ERROR,
                        err_string);
        }
    }

    /*
    ** Verify the connection is not already assign to any application
    */

    if (rc == P_SUCCESS)
    {
        app_idx = 0;

        while ((app_idx < MAX_APPS) && (!found_conn))
        {
            client_idx = 0;

            while ((client_idx < MAX_CONN_PER_APP) && (!found_conn))
            {
                if (tnicon_p->app[app_idx].client_conn[client_idx] == conn_num)
                {
                    app_assigned_to_conn = app_idx;
                    found_conn = 1;
                }
                client_idx++;
            }

            app_idx++;
        }

        if ((found_conn == 1) && (app_assigned_to_conn != app_num))
        {
            sprintf (err_string.par1, "%d", conn_num);
            sprintf (err_string.par2, "%d", app_num);
            sprintf (err_string.par3, "%d", app_idx - 1);

            output_err ("Assign_Conn_To_App",
                        MI_TNI_CONN_ASSGND,
                        MX_ERR_LVL_ERROR,
                        err_string);

            rc = P_FAILURE;
        }
    }

    /*
    ** Assign connection to application
    */

    if ((rc == P_SUCCESS) && (app_assigned_to_conn != app_num))
    {
        if (tnicon_p->app[app_num].client_conn_cnt == 0)
        {
            client_idx = 0;
        }
        else if (tnicon_p->app[app_num].client_conn_cnt < MAX_CONN_PER_APP)
        {
            client_idx = tnicon_p->app[app_num].client_conn_cnt;
        }
        else
        {
            client_idx = -1;
        }

        if (client_idx != -1)
        {
            tnicon_p->app[app_num].client_conn[client_idx] = conn_num;
            tnicon_p->app[app_num].client_conn_cnt++;
            tnicon_p->connection[conn_num].app_idx = app_num;
        } 
        else
        {
            sprintf (err_string.par1, "%d", conn_num);
            sprintf (err_string.par2, "%s", tnicon_p->app[app_num].name);

            output_err ("Assign_Conn_To_App",
                        MI_TNI_MAX_ASSGND,
                        MX_ERR_LVL_ERROR,
                        err_string);

            rc = P_FAILURE;
        }
    }

    return(rc);
}

/* [Application_Defined]
 *
 * Summary:
 *
 * Application_Defined (char *app_name )
 *
 * Input Argument:
 *
 * *app_name   - Application Name  
 *
 * Description:
 *
 * This function returns 1 if the application name is already assigned to
 * another application.  Otherwise a 0 is returned.
 *
 * Returns Values:
 *
 * 1 - application name found
 * 0 - application name not found
 *
 */

int
Application_Defined (char *app_name)
{
    int                 app_idx = 0;
    int                 found_app = 0;

    while ((app_idx < MAX_APPS) && (!found_app))
    {
        if (tnicon_p->app[app_idx].state == APP_DEFINED)
        {
            if (!strcmp (tnicon_p->app[app_idx].name, app_name))
            {
                found_app = 1;
            }
        }

        app_idx++;
    }


    if (found_app)
    {
        sprintf (err_string.par1, app_name);

        output_err ("Application_Defined",
                    MI_TNI_APP_DEFINED,
                    MX_ERR_LVL_ERROR,
                    err_string);
    }

    return(found_app);
}

/* [Find_Application_Number]
 *
 * Summary:
 *
 * Find_Application_Number (char *app_name)
 * 
 * Input Arguments:
 *
 * app_name   - Application Name
 *
 * Description:
 *
 * This function returns the application number associated with the
 * inputted application name.  If no application number is defined a
 * -1 is returned.
 *
 * Returns Values:
 *
 *  # - application name found
 * -1 - application name not found
 *
 */

int
Find_Application_Number (char *app_name)
{
    int                 app_idx = 0;
    int                 app_num = -1;
    int                 found_app = 0;

    while ((app_idx < MAX_APPS) && (!found_app))
    {
        if (tnicon_p->app[app_idx].state == APP_DEFINED)
        {
            if (!strcmp (tnicon_p->app[app_idx].name, app_name))
            {
                app_num = app_idx;
                found_app = 1;
            }
        }

        app_idx++;
    }

    return(app_num);
}

/* [Deassigned_Conn_From_App]
 *
 * Summary:
 *
 * Deassigned_Conn_From_App (int conn_num,
 *                           int app_num)
 *
 * Input Argument:
 *
 * conn_num    - Connection number
 * app_num     - Application number
 *
 * Description:
 *
 * This function deassigns a connection for an applicaton.  It is only called
 * when RPC connections using TNI protocol verions 2.0 are closed.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Deassigned_Conn_From_App (int conn_num,
                          int app_num)
{
    int                 rc = P_SUCCESS;
    int                 client_idx = 0;
    int                 conn_idx = 0;
    int                 found_conn = 0;
    int                 last_conn_idx = 0;

    err_string = null_err_string;

    /*
    ** Verify connection number is within the proper range
    */

    if ((conn_num < 1) || (conn_num > MAX_CONFIGURABLE_CONN))
    {
        sprintf (err_string.par1, "%d", conn_num);
        sprintf (err_string.par2, "%d", app_num);
        sprintf (err_string.par3, "%d", 1);
        sprintf (err_string.par4, "%d", MAX_CONFIGURABLE_CONN);

        output_err ("Deassigned_Conn_From_App",
                    MI_TNI_BAD_CONN_RNG,
                    MX_ERR_LVL_ERROR,
                    err_string);

        rc = P_FAILURE; 
    }

    /*
    ** Verify application number is within the proper range
    */

    if (rc == P_SUCCESS)
    {
        if ((app_num < 0) || (app_num > MAX_APPS))
        {
            rc = P_FAILURE;
        }
    }

    /*
    ** Verify application is in the DEFINED state
    */

    if (rc == P_SUCCESS)
    {
        if (tnicon_p->app[app_num].state != APP_DEFINED)
        {
            sprintf (err_string.par1, "%d", conn_num);
            sprintf (err_string.par2, "%d", app_num);

            output_err ("Deassigned_Conn_From_App",
                        MI_TNI_APP_NOTDEF,
                        MX_ERR_LVL_ERROR,
                        err_string);

            rc = P_FAILURE;
        }
    }

    /*
    ** Search for connection
    */

    if (rc == P_SUCCESS)
    {
        while ((client_idx < MAX_CONN_PER_APP) && (!found_conn))
        {
            if (tnicon_p->app[app_num].client_conn[client_idx] == conn_num)
            {
                found_conn = 1;
            }
            else
            {
                client_idx++;
            }
        }

        /*
        ** Remove connection from application's client connection table.
        */

        if (found_conn == 1)
        {
            tnicon_p->app[app_num].client_conn[client_idx] = 0;
            tnicon_p->app[app_num].next_unso_client_idx = 0;

            if (tnicon_p->app[app_num].client_conn_cnt == 1)
            {
                tnicon_p->app[app_num].client_conn_cnt = 0;
                tnicon_p->app[app_num].rpc_rqst_timeout = 0;
                tnicon_p->app[app_num].oltp_unso_routing = OLTP_ROUTE_NONE;
                tnicon_p->app[app_num].oltp_rqst_routing = OLTP_ROUTE_NONE;

                Deallocate_Application (app_num);
            }
            else
            {
                last_conn_idx = tnicon_p->app[app_num].client_conn_cnt - 1;

                if (last_conn_idx == client_idx)
                {
                    tnicon_p->app[app_num].client_conn_cnt--;
                }
                else
                {
                    for (conn_idx = client_idx; 
                         conn_idx < last_conn_idx;
                         conn_idx++)
                    {
                         tnicon_p->app[app_num].client_conn[conn_idx] =
                             tnicon_p->app[app_num].client_conn[conn_idx + 1];
                    }

                    tnicon_p->app[app_num].client_conn[last_conn_idx] = 0;
                    tnicon_p->app[app_num].client_conn_cnt--;
                }
            } 
        }
    }
 
    return (rc);
}

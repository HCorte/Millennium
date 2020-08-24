static const char *fileid = "";

/*
 * ===[mx_define_conns.c]=================================================
 *
 * Description:
 *
 * Functions used to define an application's connection.
 *
 * Functions:
 *
 * Add_Local_Interface 
 * Find_Unused_Connection
 * Conn_Name_Valid
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
#include <netdb.h>

#include "includes.h"

/* [Find_Unused_Connection]
 *
 * Summary:
 *
 * Find_Unused_Connection ()
 *
 * Description:
 *
 * This function returns the number of a connection in the UNDEFINDED state.
 * If all configurable connection are DEFINED a -1 is returned.
 *
 * Returns Values:
 *
 * Number of available connextion or -1 if none are available
 *
 */

int
Find_Unused_Connection ()
{
    int                 conn = -1;
    int                 conn_idx = 1;
    int                 found_conn = 0;

    err_string = null_err_string;

    while ((conn_idx <= MAX_CONFIGURABLE_CONN) && (!found_conn))
    {
        if (tnicon_p->connection[conn_idx].conn_state == CONN_UNDEFINED)
        {
             conn = conn_idx;
             found_conn = 1;
        }

        conn_idx++;
    }

    return(conn);
}

/* [Conn_Name_Valid]
 *
 * Summary:
 *
 * Conn_Name_Valid (char *domain_name)
 *
 * Input Arguments:
 *
 * domainName      - Domain Name  
 *
 * Description:
 *
 * This function verifies the MX Server can find an IP address associated
 * with a connection's domain name.
 *
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Conn_Name_Valid (char *domain_name)
{
    int                 rc = P_SUCCESS;

    domain_name = strupper(domain_name);

    if ((gethostbyname (domain_name)) == NULL)
    {
        rc = P_FAILURE;
    }

    return(rc);
}

/* [Add_Local_Interface]
 *
 * Summary:
 *
 * Add_Local_Interface (char *domain_name)
 *
 * Input Arguments:
 *
 * domainName      - Domain name of local interface
 *
 * Description:
 *
 * This function searches the Local Interface Table for the interface to be
 * added.  If the interface is already present in the Local Interface Table,
 * then funtion return SUCCESS.  If the interface is not present in the table,
 * and there is room, the interface is added to the table.  If the Local
 * Interface Table is full an error is returned.
 * 
 * Returns Values:
 *
 * P_SUCCESS, P_FAILURE
 *
 */

int
Add_Local_Interface (char *domain_name)
{
    int                 rc = P_SUCCESS;
    int                 interface_added = 0;
    int                 interface_idx = 0;

    err_string = null_err_string;

    if ((domain_name == NULL) ||
        (Conn_Name_Valid (domain_name) == P_FAILURE))
    {
        sprintf (err_string.par1, "invalid domain name %s", domain_name);

        rc = P_FAILURE;
    }

    if (rc == P_SUCCESS)
    {
        while ((interface_idx < MAX_INTERFACES_PER_TNI) &&
               (!interface_added))
        {
            if (tnicon_p->local_domain_name[interface_idx][0] == 0x00)
            {
                memcpy ((void *)tnicon_p->local_domain_name[interface_idx],
                        (void *) domain_name,
                        (size_t) _MIN(sizeof(tnicon_p->local_domain_name)-1,
                                             strlen (domain_name)));

                interface_added = 1;
            }
            else if (strcmp (tnicon_p->local_domain_name[interface_idx],
                             domain_name) == 0)
            {
                interface_added = 1;
            }
            interface_idx++;
        }

        if (interface_added == 0)
        {
            sprintf (err_string.par1, "interface table full");

            rc = P_FAILURE;
        }
    }

    if (rc == P_FAILURE)
    {
        output_err ("Add_Local_Interface",
                    MI_TNI_ADD_INTF_FAIL,
                    MX_ERR_LVL_WARNING,
                    err_string);
    }

    return(rc);
}

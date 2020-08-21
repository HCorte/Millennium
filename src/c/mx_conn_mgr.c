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
/*====[MX_CONN_MGR.C]=========================================================*/
/*                                                                            */
/* Purpose: These functions are used to manage the connections.               */
/*                                                                            */
/* Functions:                                                                 */
/*          Close_All_Connections ()                                          */
/*          Close_Connection (unsigned short conn)                            */
/*          Find_Alternate_Connection (int conn)                              */
/*          Find_Conn_For_Unso (int messaging_type)                           */
/*          Find_Conn_For_App (int messaging_type, char *app_name)            */
/*          Get_Next_State (unsigned short conn,                              */
/*                          int tni_server_state,                             */
/*                          unsigned char last_host_state)                    */
/*                                                                            */
/*====[MX_CONN_MGR.C]=========================================================*/
/*                                                                            */

#include <stdio.h>

#include "includes_mbuf.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void Close_All_Connections ()                                              */
/*                                                                            */
/* Purpose: This function closes all client connections                       */
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

void Close_All_Connections ()
{
    int conn;

    for (conn = 0; conn < MAX_CONN; conn++ ) {

        Close_Connection (conn);
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void Close_Connection (unsigned short conn)                                */
/*                                                                            */
/* Purpose: This function cleans up all the connection and socket related     */
/*          information when a connection and its associated socket are       */
/*          closed.                                                           */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection number                                */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Close_Connection (unsigned short conn)
{
int app_num = 0;
int sock;                                /* socket number                     */

   err_string = null_err_string;

   if (tnicon_p->connection[conn].conn_state > CONN_DEFINED) {

       sock = tnicon_p->connection[conn].sock;
       shutdown(sock,2);
       close (sock);

       if (conn > MAX_CONFIGURABLE_CONN) {

           tnicon_p->connection[conn].conn_state = CONN_UNDEFINED;
       }
       else {

           tnicon_p->connection[conn].conn_state = CONN_DEFINED;
       }

       if ((tnicon_p->connection[conn].tni_proto_ver == TNI_VERSION_02) &&
           (tnicon_p->connection[conn].messaging_type == MSG_TYPE_ES_RPC))
       {
          Deassigned_Conn_From_App (conn,
                                    tnicon_p->connection[conn].app_idx);
       }

       tnicon_p->connection[conn].last_client_state = NOT_PRIMARY;
       tnicon_p->connection[conn].sock = -1;
       tnicon_p->connection[conn].tni_proto_ver = TNI_VERSION_UNKNOWN;
       tnicon_p->connection[conn].max_messages_per_pdu = DEF_MAX_MGS_PER_PDU;
       tnicon_p->connection[conn].max_pdu_size = DEF_PDU_SIZE;
       tnicon_p->connection[conn].messaging_type = DEF_MESSAGING_TYPE;
       tnicon_p->connection[conn].auth_state = AUTH_NOTATTEMPTED;
       tnicon_p->connection[conn].auth_hash_len = -1;
       tnicon_p->connection[conn].consec_auth_failure_cnt = 0;

       if (tnicon_p->connection[conn].enc_key != NULL)
       {

#if defined(PROSYS_ENV_V8) || defined(PROSYS_ENV_PLATFORM)

           switch (tnicon_p->connection[conn].enc_key_type)
           {
           case ENC_KEY_AES_128_CBC:
           case ENC_KEY_AES_256_CBC:
               mb_free_p((struct mbuf *) tnicon_p->connection[conn].enc_key);
               break;
           }
           tnicon_p->connection[conn].enc_key = NULL;
#endif
 
       }

       tnicon_p->connection[conn].enc_key_x_method = METH_ENC_KEY_X_NONE;
       tnicon_p->connection[conn].enc_key_type = ENC_KEY_NONE;
       tnicon_p->connection[conn].enc_state = ENC_NOT_CONFIGURED;

       memset(tnicon_p->connection[conn].auth_hash_value, 0x00,
            sizeof(tnicon_p->connection[conn].auth_hash_value));

       memset(&tnicon_p->connection[conn].last_auth_atmpt_time, 0x00,
            sizeof(tnicon_p->connection[conn].last_auth_atmpt_time));

       memset (tnicon_p->connection[conn].session_tag,
               '\0',
               sizeof(tnicon_p->connection[conn].session_tag));

#      if defined(GOLS_ENV_ALL)

           sprintf(err_string.par1,"\a\a%d",conn);
  
#      endif

#      if defined(PROSYS_ENV_ALL)

           sprintf(err_string.par1,"%d",conn);

#      endif

       sprintf(err_string.par2,"%s",
               tnicon_p->connection[conn].remote_domain_name);

       if (tnicon_p->connection[conn].managed_by == CONNS_CLIENT_ID)
       {
           if (tnicon_p->connection[conn].client_id_val == -1)
           {
               sprintf(err_string.par3,"Unknown client id");
           }
           else
           {
               sprintf(err_string.par3,"client id %d",
                       tnicon_p->connection[conn].client_id_val);
           }
       }
       else
       { 
           app_num = tnicon_p->connection[conn].app_idx;

           if (app_num == -1)
           {
               sprintf(err_string.par3,"%s",
                       "Undefined App");
           }
           else
           {
               sprintf(err_string.par3,"%s",
                       tnicon_p->app[app_num].name);
           }
        }

       output_err("Close_Connection",
                  MI_TNI_CONN_CLOSE,
                  MX_ERR_LVL_WARNING,
                  err_string);
   }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Find_Conn_For_App (int messaging_type, int enc_mode, int app_num,          */
/*                    char *app_name)                                         */
/*                                                                            */
/* Purpose: This function finds the next available connection for a specific  */
/* application.  The connection must be of the proper messaging type and be   */
/* in the state of PRIMARY.  Connection is select in a round-robin fashion.   */
/* Application can be specified by number or name.                            */
/*                                                                            */
/* input Arguments:                                                           */
/*          messaging_type   Type of messaging the connection must be         */
/*                           configured for (OLTP or RPC).                    */
/*                                                                            */
/*          app_num          Application number                               */
/*                                                                            */
/*          app_name         Name of the application                          */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              # Connection number                              */
/*                           0 No connection found                            */
/*                                                                            */
/* Assumptions: None.                                                         */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Find_Conn_For_App (int messaging_type, int enc_mode, int app_num,
                       char *app_name)
{
    int                 client_id_idx = 0;
    int                 client_id = 0;
    int                 conn_cnt = 0;
    int                 conn_idx = 0;
    int                 conn_num = 0;
    int                 send_on_conn = 0;
    int                 found_conn = 0;

    char               *conns_app_name;

    if (app_name != NULL)
    {
        app_num = Find_Application_Number(app_name);
    }

    /*
    ** Connection(s) associated with application name are managed by
    ** application.
    */
 
    if ((app_num >= 0) && (app_num < MAX_APPS))
    {
        conn_cnt = 0;
        conn_idx = tnicon_p->app[app_num].next_unso_client_idx;

        while ((conn_cnt < tnicon_p->app[app_num].client_conn_cnt) &&
               (!found_conn))
        {
            conn_num = tnicon_p->app[app_num].client_conn[conn_idx];

            if ((tnicon_p->connection[conn_num].conn_state == CONN_PRIMARY) &&
                (tnicon_p->connection[conn_num].messaging_type ==
                messaging_type))
            {
                /*
                ** Select connection if encryption is not required.  If
                ** encyrption is required then the following conditions
                ** must be true:
                **     The message type is RPC
                **     The application must be configured for encytpion
                **     The connection must have an encryption key
                */ 

                if ((enc_mode == ENC_NONE) ||
                    ((enc_mode != ENC_NONE) &&
                     (tnicon_p->connection[conn_num].messaging_type == MSG_TYPE_ES_RPC) &&
                     (tnicon_p->app[app_num].es_rpc_enc_mode != ENC_DISABLED) &&
                     (tnicon_p->connection[conn_num].enc_state == ENC_AVAILABLE)))
                {
                    send_on_conn = conn_num;
                    found_conn = 1;

                    if ((conn_idx + 1) >= tnicon_p->app[app_num].client_conn_cnt)
                    {
                        tnicon_p->app[app_num].next_unso_client_idx = 0;
                    }
                    else
                    {
                        tnicon_p->app[app_num].next_unso_client_idx = conn_idx + 1;
                    }
                }
            }

            conn_idx++;

            if (conn_idx >= tnicon_p->app[app_num].client_conn_cnt)
            {
                conn_idx = 0;
            }

            conn_cnt++;
        }
    }

    return(send_on_conn);
}

/* [Find_Conn_For_Oltp_Unso]
 *
 * Summary:
 *
 * Find_Conn_For_Oltp_Unso (int *conn_num)
 *
 * Description:
 *
 * This function finds a primary OLTP connection assoicated with the returned
 * client id.  If a primary connection does not exist, then a connection
 * number of zero is returned.  The client id returned is chosen in a 
 * round robin fasion.
 * 
 * Input Arguments: None
 * 
 * Output Arguments:
 *          conn_num         Primary connection associated with Client Id
 *
 * Return Value:
 *          int              # Client Id
 *                           '0' No Client Ids have been found (configured)
 *
 */

int
Find_Conn_For_Oltp_Unso (int *conn_num)
{
    static int          next_client_id_idx = 0;

    int                 client_id_cnt = 0;
    int                 client_id_idx = 0;
    int                 client_id = 0;
    int                 conn_cnt = 0;
    int                 conn_idx = 0;
    int                 conn = 0;
    int                 found_conn = 0;

    client_id_idx = next_client_id_idx;
    *conn_num = 0;

    if (client_id_idx > MAX_NUM_CLIENT_IDS) 
    {
        client_id_idx = 0;
    }

    while ((client_id_cnt < MAX_NUM_CLIENT_IDS) &&
           (client_id == 0)) 
    {

        client_id = tnicon_p->client_idx_tbl[client_id_idx];

        if (client_id != 0)
        {
            /* Check if any connections have been defined for this client id */

            conn_idx = tnicon_p->last_oltp_conn_sent[client_id_idx];

            if (conn_idx > MAX_CONNS_PER_CLIENT)
            {
                conn_idx = 0;
            }

            while ((conn_cnt < MAX_CONNS_PER_CLIENT) &&
                  (!found_conn))
            {
                conn = tnicon_p->client_conns[client_id_idx][conn_idx];

                if (conn != 0)
                {
                    if (tnicon_p->connection[conn].messaging_type ==
                        MSG_TYPE_OLTP)
                    {
                        if (tnicon_p->connection[conn].conn_state ==
                            CONN_PRIMARY)
                        {
                            *conn_num = conn;
                            found_conn = 1;
                            tnicon_p->last_oltp_conn_sent[client_id_idx] = 
                                conn_idx + 1;
                        }
                    }
                    conn_cnt++;
                    conn_idx++;

                    if (conn_idx > MAX_CONNS_PER_CLIENT)
                    {
                        conn_idx = 0;
                    }
                }
                else
                {
                   conn_cnt = (MAX_CONNS_PER_CLIENT - conn_idx) + conn_cnt;
                   conn_idx = 0;
                }
            }
            next_client_id_idx = client_id_idx + 1;
        }
        else
        {
            if (client_id_idx == 0)
            {
                client_id_cnt = MAX_NUM_CLIENT_IDS;
            }
            else
            { 
                client_id_idx = 0;
            }
        }
    }

    return(client_id);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Find_Alternate_Connection (int conn)                                       */
/*                                                                            */
/* Purpose: This function finds an alternate (redundant) connection to the    */
/* one specific in the connection argument.  For connection managed by        */
/* application name, an alternate connection is a connections that shares the */
/* same session tag.  For connections managed by client id, an alternate      */
/* connection is one that shares the same client id and remote node name.     */
/* The alternate connection must be of the proper messaging type and be in    */
/* the state of PRIMARY.  When no alternate connection is available the       */
/* function returns the connection specified in the argument.                 */
/*                                                                            */
/* input Arguments:                                                           */
/*          conn             Connection to find an alternate for.             */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              Alternate connection number                      */
/*                                                                            */
/* Assumptions: The conn argument contains a valid connection number.         */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Find_Alternate_Connection (int conn)
{
    int                 alternate_conn = 0;
    int                 conn_idx = 0;
    int                 alt_conn_num = 0;
    int                 found_alt_conn = 0;

    alt_conn_num = conn;

    if (tnicon_p->connection[conn].managed_by == CONNS_APP_NAME)
    {
        if (tnicon_p->connection[conn].session_tag[0] != '\0')
        {
            if (conn == MAX_CONFIGURABLE_CONN)
            {
                conn_idx = 1;
            }
            else
            {
                conn_idx = conn + 1;
            }

            while ((conn_idx != conn) && (!found_alt_conn))
            {
                if ((tnicon_p->connection[conn_idx].conn_state == CONN_PRIMARY)
                    && (tnicon_p->connection[conn_idx].session_tag[0] != '\0'))
                {
                    if ((tnicon_p->connection[conn_idx].messaging_type ==
                         tnicon_p->connection[conn].messaging_type) &&
                        (!strcmp (tnicon_p->connection[conn_idx].session_tag,
                                  tnicon_p->connection[conn].session_tag)))
                    {
                        alt_conn_num = conn_idx;
                        found_alt_conn = 1;
                    }
                }

                conn_idx++;

                if (conn_idx > MAX_CONFIGURABLE_CONN)
                {
                    conn_idx = 1;
                }
            }
        }
    }
    else if (tnicon_p->connection[conn].managed_by == CONNS_CLIENT_ID)
    {
        if (tnicon_p->connection[conn].alt_conn != 0)
        {
            alternate_conn = tnicon_p->connection[conn].alt_conn;

            if (tnicon_p->connection[alternate_conn].conn_state ==
                CONN_PRIMARY)
            {
                alt_conn_num = alternate_conn;
            }
        }
    }

    return (alt_conn_num);
}

/*============================================================================*/
/*                                                                            */
/* Get_Next_State (unsigned short conn,                                       */
/*                 int tni_server_state,                                      */
/*                 unsigned char last_host_state)                             */
/*                                                                            */
/* Purpose: This function examines a connection's current state, the current  */
/*          TNI server state, and the last known state of the host            */
/*          communicating on a connection to determine what connection state  */
/*          change, if any, will be required.                                 */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             connection number                                */
/*          tni_server_state state of the system on which the Gswitch is      */
/*                           executing (Primary or Backup)                    */
/*          last_host_state  last known state of the host communicating on    */
/*                           this connection (Primary or Not Primary)         */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*           0               no state change required                         */
/*          -1               the combination of conn_state, tni_server_state, */
/*                           and last_host_state would result in an invalid   */
/*                           (undefined) state based upon the state table     */
/*                           definition below                                 */
/*          >0               connection's new state                           */
/*                                                                            */
/* Assumptions: If a host has never sent a Primary status parameter, then the */
/*              last known host state will be set to the default (Not         */
/*              Primary).  For each successive call to this function, only    */
/*              the TNI server state or the last known host state will        */
/*              change, i.e., both cannot change simultaneously.  In addition,*/
/*              This function ONLY handles transitions for the "connected"    */
/*              states.  Valid connected states are defined as follows        */
/*                                                                            */
/* TNI server state   last host state  state                                  */
/*                                                                            */
/* Primary            Primary          Primary (subject to dual primary check)*/
/* Primary            Not Primary      Switch Primary                         */
/* Not Primary        Primary          Host Primary                           */
/* Not Primary        Not Primary      Not Primary                            */
/*                                                                            */
/* The Primary Pending state is used to determine if a transition to the      */
/* Primary state is allowed.  If allowing a connection state to change from   */
/* Primary Pending to Primary would result in a dual primary situation, then  */
/* the new state is set to Primary Pending and the dual primary situation is  */
/* flagged.  If allowing a connection state to change from Primary Pending to */
/* Primary would not result in a dual primary situation, then the new state   */
/* is set to Primary.                                                         */
/*                                                                            */
/* The following state table is used to determine valid transitions           */
/*                                                                            */
/* Current conn    TNI server      last host       new state                  */
/* state           state           state                                      */
/*                                                                            */
/* Connected       Primary         Primary         Invalid                    */
/*                 Primary         Not Primary     Switch Primary             */
/*                 Not Primary     Primary         Host Primary               */
/*                 Not Primary     Not Primary     Not Primary                */
/*                                                                            */
/* Not Primary     Primary         Primary         Invalid                    */
/*                 Primary         Not Primary     Switch Primary             */
/*                 Not Primary     Primary         Host Primary               */
/*                 Not Primary     Not Primary     No Change                  */
/*                                                                            */
/* Switch Primary  Primary         Primary         Primary Pending or Primary */
/*                 Primary         Not Primary     No Change                  */
/*                 Not Primary     Primary         Invalid                    */
/*                 Not Primary     Not Primary     Not Primary                */
/*                                                                            */
/* Host Primary    Primary         Primary         Primary Pending or Primary */
/*                 Primary         Not Primary     Invalid                    */
/*                 Not Primary     Primary         No Change                  */
/*                 Not Primary     Not Primary     Not Primary                */
/*                                                                            */
/* Primary         Primary         Primary         No Change                  */
/*                 Primary         Not Primary     Switch Primary             */
/*                 Not Primary     Primary         Host Primary               */
/*                 Not Primary     Not Primary     Invalid                    */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

char Get_Next_State (unsigned short conn, int tni_server_state,
                     unsigned char last_host_state)
{
char next_state;                         /* returned state                    */
char combined_state;                     /* combined TNI and host state       */
unsigned char conn_state;                /* current connection state          */


   conn_state = tnicon_p->connection[conn].conn_state;
   next_state = 0;                      /* initialize to no change            */

   if (tni_server_state == PRIMARY && last_host_state == PRIMARY) {
       combined_state = BOTH_PRIMARY;
   }
   else if (tni_server_state == PRIMARY && last_host_state == NOT_PRIMARY) {
       combined_state = TNI_PRIMARY;
   }
   else if (tni_server_state == NOT_PRIMARY && last_host_state == PRIMARY) {
       combined_state = HOST_PRIMARY;
   }
   else {
       combined_state = NEITHER_PRIMARY;
   }

   switch (conn_state)
   {

       case CONN_CONNECTED:
            switch (combined_state)
            {
                case BOTH_PRIMARY:
                     next_state = -1;            /* invalid                   */
                     break;

                case TNI_PRIMARY:
                     next_state = CONN_SWITCH_PRIMARY;
                     break;

                case HOST_PRIMARY:
                     next_state = CONN_HOST_PRIMARY;
                     break;

                case NEITHER_PRIMARY:
                     next_state = CONN_NOT_PRIMARY;
                     break;

            }                                    /* end combined_state switch */

            break;

       case CONN_NOT_PRIMARY:
            switch (combined_state)
            {
                case BOTH_PRIMARY: 
                     next_state = -1;            /* invalid                   */
                     break;

                case TNI_PRIMARY:
                     next_state = CONN_SWITCH_PRIMARY;
                     break;

                case HOST_PRIMARY:
                     next_state = CONN_HOST_PRIMARY;
                     break;

                case NEITHER_PRIMARY:            /* no change                 */
                     break;

            }                                    /* end combined_state switch */
            break;

       case CONN_SWITCH_PRIMARY:
            switch (combined_state)
            {
                case BOTH_PRIMARY:
                     next_state = CONN_PRIMARY_PENDING;
                     break;

                case TNI_PRIMARY:                /* no change                 */
                     break;

                case HOST_PRIMARY:
                     next_state = -1;            /* invalid                   */
                     break;

                case NEITHER_PRIMARY:
                     next_state = CONN_NOT_PRIMARY;
                     break;

            }                                    /* end combined_state switch */
            break;

       case CONN_HOST_PRIMARY:
            switch (combined_state)
            {
                case BOTH_PRIMARY:
                     next_state = CONN_PRIMARY_PENDING;
                     break;

                case TNI_PRIMARY:
                     next_state = -1;            /* invalid                   */
                     break;

                case HOST_PRIMARY:               /* no change                 */
                     break;

                case NEITHER_PRIMARY:
                     next_state = CONN_NOT_PRIMARY;
                     break;

            }                                    /* end combined_state switch */
            break;

/*
       case CONN_PRIMARY_PENDING:
            switch (combined_state)
            {
                case BOTH_PRIMARY:
                     break;

                case TNI_PRIMARY:
                     break;

                case HOST_PRIMARY:
                     break;

                case NEITHER_PRIMARY:
                     break;

            }
            break;
*/

       case CONN_PRIMARY:
            switch (combined_state)
            {
                case BOTH_PRIMARY:               /* no change                 */
                     break;

                case TNI_PRIMARY:
                     next_state = CONN_SWITCH_PRIMARY;
                     break;

                case HOST_PRIMARY:
                     next_state = CONN_HOST_PRIMARY;
                     break;

                case NEITHER_PRIMARY:
                     next_state = -1;            /* invalid                   */
                     break;

            }                                    /* end combined_state switch */
            break;

   }                                             /* end conn_state switch     */


   if ((next_state == CONN_PRIMARY_PENDING)||
       (conn_state == CONN_PRIMARY_PENDING)) {   /* check for dual primary    */

       next_state = CONN_PRIMARY;
   }

   return (next_state);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Validate_Tni_Config_Conns (int first_tni_config_conn)                      */
/*                                                                            */
/* Purpose: This function assigns a client id index to all connections        */
/*          defined in the tni_server.file.  Verify:                          */
/*           - Connections with the same remote node name must have different */
/*             node name must have different local domain names or client ids.*/
/*           - All remote domain names must be unique.                        */
/*           - A remote node name may only be shared by                       */
/*             MAX_INTERFACES_PER_CLIENT connections.                         */
/*           - Maximum number of connection for a given client id is not      */
/*             exceeded.                                                      */
/*                                                                            */
/* input Arguments:                                                           */
/*          first_tni_config_conn    First connection used by the connections */
/*                                   defined in the tni_server file.          */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              0 One of the connection is not valid             */
/*                           1 All connections are valid                      */
/*                                                                            */
/* Assumptions: Domain names have already been validated.                     */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int
Validate_Tni_Config_Conns (int first_tni_config_conn)
{
    int                 client_conn_cnt[MAX_NUM_CLIENT_IDS];
    int                 client_id = 0;
    int                 client_id_idx = -1;
    int                 conn = 0;
    int                 conn_idx = 0;
    int                 conns_per_client_idx = 0;
    int                 conn_num = 0;
    int                 conns_client_id = 0;
    int                 nxt_conn = 0;
    int                 nxt_conn_idx = 0;
    int                 nxt_conns_client_id = 0;
    int                 rc = P_SUCCESS;
    int                 remote_node_cnt = 0;

    char               *conns_local_domain_name;
    char               *conns_remote_domain_name;
    char               *conns_remote_node_name;
    char               *nxt_conns_local_domain_name;
    char               *nxt_conns_remote_domain_name;
    char               *nxt_conns_remote_node_name;

    err_string = null_err_string;

    memset(client_conn_cnt,0,sizeof(client_conn_cnt));

    conn_num = first_tni_config_conn;

    /*
    ** Assigned client id indexes to all connections.
    */

    while ((conn_num <= MAX_CONFIGURABLE_CONN) &&
           (tnicon_p->connection[conn_num].client_id_val != -1) &&
           (rc == P_SUCCESS))
    {
        client_id = tnicon_p->connection[conn_num].client_id_val;

        client_id_idx = Find_Client_Idx(client_id);
 
        if (client_id_idx == -1)
        {
            sprintf(err_string.par1,"%d",MAX_NUM_CLIENT_IDS);

            output_err("Validate_Tni_Config_Conns",
                       MI_TNI_MAX_CLIENTIDS,
                       MX_ERR_LVL_ERROR,
                       err_string);

            rc = P_FAILURE;
        }

        /*
        ** Verify the maximum number of connection for a given client id
        ** is not exceeded.
        */

        if (rc == P_SUCCESS)
        {
            client_conn_cnt[client_id_idx]++;

            if (client_conn_cnt[client_id_idx] > MAX_CONNS_PER_CLIENT)
            {
                sprintf(err_string.par1,"%d",MAX_CONNS_PER_CLIENT);

                output_err("Validate_Tni_Config_Conns",
                           MI_TNI_MAX_CONNS_PER,
                           MX_ERR_LVL_ERROR,
                           err_string);

                rc = P_FAILURE;
            }
        }

        /*
        ** Assign client id index to the concection.  Create client id index
        ** to connection mapping.  Assign connection management type.
        */
 
        if (rc == P_SUCCESS)
        {
            tnicon_p->connection[conn_num].client_id_idx = client_id_idx;

            conns_per_client_idx = client_conn_cnt[client_id_idx] - 1;
            tnicon_p->client_conns[client_id_idx][conns_per_client_idx]
                = conn_num;

            tnicon_p->connection[conn_num].conn_state = CONN_DEFINED;
            tnicon_p->connection[conn_num].managed_by = CONNS_CLIENT_ID;

            conn_num++;
        }
    }

    /*
    ** Examine all the assigned connections for a defined host id.  Verify
    ** the following conditions are all true:
    ** (1)  Connections with the same remote node name must have different
    **      local domain names or client ids.
    ** (2)  All remote domain names must be unique.
    ** (3)  A remote node name may only be shared by MAX_INTERFACES_PER_CLIENT
    **      connections.
    */

    if (rc == P_SUCCESS)
    {
        client_id_idx = 0;

        while ((client_id_idx < MAX_NUM_CLIENT_IDS) &&
               (tnicon_p->client_idx_tbl[client_id_idx] != -1) &&
               (rc == P_SUCCESS))
        {
            conn_idx = 0;

            while (conn_idx < client_conn_cnt[client_id_idx])
            {
                remote_node_cnt = 0;
                conn = tnicon_p->client_conns[client_id_idx][conn_idx];
                conns_client_id = tnicon_p->connection[conn].client_id_val;

                nxt_conn_idx = conn_idx + 1;
 
                while ((nxt_conn_idx < client_conn_cnt[client_id_idx]) &&
                       (rc == P_SUCCESS))
                {
                    nxt_conn =
                        tnicon_p->client_conns[client_id_idx][nxt_conn_idx];
                    nxt_conns_client_id = 
                        tnicon_p->connection[nxt_conn].client_id_val;

                    conns_local_domain_name = 
                        tnicon_p->connection[conn].local_domain_name;
                    conns_remote_domain_name = 
                        tnicon_p->connection[conn].remote_domain_name;
                    conns_remote_node_name =
                        tnicon_p->connection[conn].remote_node_name;

                    nxt_conns_local_domain_name = 
                        tnicon_p->connection[nxt_conn].local_domain_name;
                    nxt_conns_remote_domain_name = 
                        tnicon_p->connection[nxt_conn].remote_domain_name;
                    nxt_conns_remote_node_name =
                        tnicon_p->connection[nxt_conn].remote_node_name;

                    if ((strcmp (conns_local_domain_name,
                                 nxt_conns_local_domain_name) == 0) &&
                        (strcmp (conns_remote_node_name,
                                 nxt_conns_remote_node_name) == 0))
                    {
                        if (conns_client_id == nxt_conns_client_id)
                        {
                            sprintf (err_string.par1, "%d", conn);
                            sprintf (err_string.par2, "%d", nxt_conn);

                            output_err ("Validate_Tni_Config_Conns",
                                        MI_TNI_DUP_CONNS,
                                        MX_ERR_LVL_ERROR,
                                        err_string);

                            rc = P_FAILURE;
                        }
                    }
                    else if (strcmp (conns_remote_domain_name,
                                     nxt_conns_remote_domain_name) == 0)
                    {
                        if (conns_client_id == nxt_conns_client_id)
                        {
                            sprintf (err_string.par1, "%d", conn);
                            sprintf (err_string.par2, "%d", nxt_conn);
                            sprintf (err_string.par3,
                                     "%s",
                                     conns_remote_domain_name);

                            output_err ("Validate_Tni_Config_Conns",
                                        MI_TNI_SAME_REMOTE,
                                        MX_ERR_LVL_ERROR,
                                        err_string);

                            rc = P_FAILURE;
                        }
                    }
                    else if (strcmp (conns_remote_node_name,
                                     nxt_conns_remote_node_name) == 0)
                    {
                        remote_node_cnt++;

                        if (remote_node_cnt > MAX_INTERFACES_PER_CLIENT)
                        {
                            sprintf (err_string.par1,
                                     "%s",
                                     conns_remote_node_name);
                            sprintf (err_string.par2,
                                     "%d",
                                     MAX_INTERFACES_PER_CLIENT);

                            output_err ("Validate_Tni_Config_Conns",
                                        MI_TNI_MAX_SYS,
                                        MX_ERR_LVL_ERROR,
                                        err_string);

                            rc = P_FAILURE;

                        }
                        else
                        {
                            tnicon_p->connection[conn].alt_conn = nxt_conn;
                            tnicon_p->connection[nxt_conn].alt_conn = conn;
                        } 
                    }
                    nxt_conn_idx++;
                }
                conn_idx++;
            }
            client_id_idx++;
        }
    }

    return (rc);
}

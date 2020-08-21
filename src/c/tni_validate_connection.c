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
/*====[TNI_VALIDATE_CONNECTION.C]=============================================*/
/*                                                                            */
/* Validate_Connection (int new_sock,                                         */
/*                      struct sockaddr_in new_sock_addr_in,                  */
/*                      char *local_device_name)                              */
/*                                                                            */
/* Purpose: This function validates an attempted connection.  A connection is */
/*          determined to be valid if the UCX host name of the attempted      */
/*          connection is configured in the TNI configuration file.  If a     */
/*          connection is valid, then the connection information is added to  */
/*          the appropriate connection's information and TNI immediately      */
/*          sends the TNI REQUEST PDU to notify the connecting host which     */
/*          TNI it is connecting to (primary or non-primary).  If the         */
/*          connection is invalid, then the new socket is immediately closed. */
/*                                                                            */
/* Input Arguments:                                                           */
/*          new_sock         Socket number                                    */
/*          new_sock_addr_in socket address structure filled in by TCP accept */
/*                           function                                         */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_VALIDATE_CONNECTION.C]=============================================*/
/*                                                                            */

#include <errno.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>

#include "includes.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include <sys/socket.h>
#   include <netinet/in.h>
#   include <arpa/inet.h>

#elif defined(XOS_VMS)

#   include <inet.h>

#else

#   error - OS-specific logic not handled.

#endif

void Validate_Connection (int new_sock,
                          struct sockaddr_in new_sock_addr_ini,
                          char *local_device_name)
{
int i;                                   /* local indices                     */
int tmp_conn;                            /* temporary connection number       */
int client_namelen;                      /* number of characters in client    */
                                         /* (remote) device name              */
int local_namelen;                       /* number of characters in server    */
                                         /* (local) device name               */
int int_addr;                            /* socket address structure, cast as */
                                         /* int                               */
int found_tmp_conn;                      /* flag indicating available temp    */
                                         /* connection number                 */
                                         /* name has been found in config file*/
int host_id_idx;                         /* host id index                     */
int mx_err_lvl = MX_ERR_LVL_INFO;
unsigned short new_port;                 /* port number on which connection   */
                                         /* has been accepted                 */

char new_client_host[MAX_CLIENT_NAME_LEN]; /* name of remote device           */
char *new_ip_addr;                       /* IP address of remote device       */

struct hostent *new_host;                /* structure containing host name of */
                                         /* machine from which we have just   */
                                         /* accepted the connection           */
char next_state;                         /* next connection state             */

   err_string = null_err_string;

   found_tmp_conn = 0;

   local_namelen = strlen(local_device_name);

   int_addr = (int)new_sock_addr_in.sin_addr.s_addr;

   if ((new_host = gethostbyaddr((char*)(&int_addr),sizeof(int_addr),
                                 AF_INET)) == NULL) {

      sprintf(err_string.par1,
              "%s", 
              inet_ntoa(new_sock_addr_in.sin_addr));

      output_err("Validate_Connection",
                 MI_TNI_NO_TRANSLATE,
                 MX_ERR_LVL_ERROR,
                 err_string);

      shutdown(new_sock,2);
      close (new_sock);
      return;
   }

   else {

      new_ip_addr = inet_ntoa(new_sock_addr_in.sin_addr);
      new_port = ntohs(new_sock_addr_in.sin_port);
 
/*    convert new client name to upper case for comparison later on with      */
/*    allowable (configured) client names                                     */

      client_namelen = strlen(new_host->h_name);

      for (i = 0; i < sizeof(new_client_host);i++) {

         if (i < client_namelen) {
            new_client_host[i] = toupper (new_host->h_name[i]);
         }
         else {
            new_client_host[i] = '\0';
         }
      }                                                            /* end for */

/*    Search all defined connections for the name of the client attempting    */
/*    a connection.  If a connection has been defined in the TNI config file  */
/*    for the local host and client name, the add the client's information to */
/*    the connection table.                                                   */


      for (i = MAX_CONFIGURABLE_CONN + 1; 
           i < MAX_CONN; i++)
      {
         if ((tnicon_p->connection[i].conn_state ==
             CONN_UNDEFINED) && (!found_tmp_conn)) {

            found_tmp_conn = 1;
            tmp_conn = i;
         }
      }

      if (found_tmp_conn) {

          tnicon_p->connection[tmp_conn].app_idx = -1;

/*       Place local and remote names in connecion information table          */

          memset(tnicon_p->connection[tmp_conn].local_domain_name,
                 0x00,
                 sizeof(tnicon_p->connection[tmp_conn].local_domain_name));
          memset(tnicon_p->connection[tmp_conn].remote_domain_name,
                 0x00,
                 sizeof(tnicon_p->connection[tmp_conn].remote_domain_name));

          strncpy(tnicon_p->connection[tmp_conn].remote_domain_name,
                  new_client_host,client_namelen);
          strncpy(tnicon_p->connection[tmp_conn].local_domain_name,
                  local_device_name,local_namelen);

/*       Add the bit for the new socket to the read and write masks           */

         if (0 <= new_sock && new_sock < FD_SETSIZE) {

            tnicon_p->connection[tmp_conn].time_last_sent = current_time;
            tnicon_p->connection[tmp_conn].time_last_rcvd = current_time;

/*          Load socket information into connection structures                */

            tnicon_p->connection[tmp_conn].sock = new_sock;
            tnicon_p->connection[tmp_conn].conn_state = CONN_CONNECTED;

            next_state = Get_Next_State (tmp_conn,
                         tnicon_p->tni_server_state,
                         tnicon_p->connection[tmp_conn].last_client_state);

            switch (next_state)
            {
               case -1:
                  printf ("Invalid state change attempt from "
                         "Validate_Connection \n");
                  break;

               case 0:
                  break;

               default:
                  tnicon_p->connection[tmp_conn].conn_state = next_state;
                  break;
            }

/*          use default keepalive timer value until client tells us to use    */
/*          a different timer value                                           */

            tnicon_p->connection[tmp_conn].keepalive_time =
               Set_Timervalue (DEF_KEEPALIVE_TIME *
                                 TICS_PER_SEC);

            tnicon_p->connection[tmp_conn].conn_alive_timeout =
               Set_Timervalue (DEF_KEEPALIVE_TIME * 3 *
                                 TICS_PER_SEC);
         }
         else {
            sprintf(err_string.par1, "%d", new_sock);
            sprintf(err_string.par2, "%d", 0);
            sprintf(err_string.par3, "%d", FD_SETSIZE);

            output_err("Validate_Connection",
                       MI_TNI_BAD_SOCK_RNG,
                       MX_ERR_LVL_ERROR,
                       err_string);

            found_tmp_conn = 0;
         }
      }
   }                                  /* end else gethostbyaddr OK            */


/*    Notify error logger of any successful or failed connection attempts     */

   if (found_tmp_conn)
   {
       sprintf(err_string.par1,"accepting");
   }
   else
   {
      sprintf(err_string.par1,"rejecting");

      shutdown(new_sock,2);
      close (new_sock);

      mx_err_lvl = MX_ERR_LVL_WARNING;
   }                                  /* end else name not found              */

   sprintf(err_string.par2,"%s",new_host->h_name);
   sprintf(err_string.par3,"%s",new_ip_addr);
   sprintf(err_string.par4,"%d",new_port);
   sprintf(err_string.par5,"%d",new_sock);

   output_err("Validate_Connection",
              MI_TNI_CONN_ATTEMPT,
              mx_err_lvl,
              err_string);
   return;
}

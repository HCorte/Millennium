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
/*====[MX_SOCKET.C]===========================================================*/
/*                                                                            */
/* Purpose: These functions preform the socket operations.                    */
/*                                                                            */
/* Functions:                                                                 */
/*          Get_New_Connections ()                                            */
/*          Listen_Socket_Init ()                                             */
/*          Close_Listen_Sockets ()                                           */
/*          Read_From_Socket (unsigned short connection,                      */
/*                            struct mbuf **sockbuf,                          */
/*                            int length)                                     */
/*          Send_To_Conn (struct mbuf *writechain,                            */
/*                        unsigned short conn,                                */
/*                        int send_mode)                                      */
/*          Sock_Rd_Timeout (int conn)                                        */
/*          Sock_Wrt_Timeout (int conn)                                       */
/*                                                                            */
/*====[MX_SOCKET.C]===========================================================*/
/*                                                                            */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>
#include <netdb.h>

#include "includes_mbuf.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include <netinet/in.h>

#elif defined(XOS_VMS)

#   include <ssdef.h>

#   ifdef TCPIP_SERVICES

#       include "SYS$LIBRARY:TCPIP$INETDEF.h"

#   else

#       include "SYS$LIBRARY:UCX$INETDEF.h"
#   endif


#else

#   error - OS-specific logic not handled.

#endif

#define SOCK_RD_EFN       5              /* socket read event flag            */
#define SOCK_WRT_EFN      2              /* socket write event flag           */

extern void Sock_Rd_Timeout(int);
extern void Sock_Wrt_Timeout(int);

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Get_New_Connections ()                                                     */
/*                                                                            */
/* Purpose: This function handles new client connections  - only listens for  */
/*          a single connection, blocks until a new connection is made.       */
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

void Get_New_Connections ()
{
int i;                                   /* local TNI device index            */
int retval;                              /* select socket function return     */

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX)

    socklen_t addr_len;                  /* length of sockaddr_in             */

#elif defined(XOS_UNIX_TRU64)

    int addr_len;                        /* length of sockaddr_in             */

#elif defined(XOS_VMS)

    unsigned int addr_len;               /* length of sockaddr_in             */

#else

#   error - OS-specific logic not handled.

#endif

fd_set rmask;                            /* read mask                         */
int new_sock;                            /* socket number returned by accept  */
int tmp_sock;                            /* Temporary socket file descriptor  */
int sndbuf_quota;                        /* TCP send buffer quota             */
int rcvbuf_quota;                        /* TCP receive buffer quota          */
int sock_opt_rtn;                        /* set socket option return value    */
char *sock_opt_p;                        /* socket option pointer             */

   err_string = null_err_string;

     for (i = 0; i < MAX_INTERFACES_PER_TNI; i++) {

        if (listen_socket[i] != -1) {

        FD_ZERO(&rmask);
        FD_SET(listen_socket[i], &rmask);

        retval = select (listen_socket[i] + 1, 
                         &rmask, 
                         NULL, 
                         NULL, 
                         &listen_timeout[i]);

        switch (retval) {
           case (-1):

              sprintf(err_string.par1,"(select)");
              sprintf(err_string.par2,"%s",getErrorMsg(errno));

              output_err("Get_New_Connections",
                         MI_TNI_SOCK_ERR,
                         MX_ERR_LVL_ERROR,
                         err_string);

              if (errno != EINTR) {
                  shutdown(listen_socket[i],2);
                  close(listen_socket[i]);
              }

           case (0):
              break;

           default:

              addr_len = sizeof(struct sockaddr_in);
              if ((new_sock = accept(listen_socket[i],
                                     (struct sockaddr *)&new_sock_addr_in,
                                     &addr_len)) < 0) {

                 sprintf(err_string.par1,"(accept)");
                 sprintf(err_string.par2,"%s",getErrorMsg(errno));

                 output_err("Get_New_Connections",
                            MI_TNI_SOCK_ERR,
                            MX_ERR_LVL_ERROR,
                            err_string);
              }
              else {

                /* Prevent the running out of file descriptors. 
                 * The file descriptor returned is the lowest one available. 
                 */ 
                 if (new_sock >= FD_SETSIZE) {
                     tmp_sock = new_sock;
                     new_sock = dup(new_sock);
                     close(tmp_sock);
                 }

                 rcvbuf_quota =  tnicon_p->rcv_buf_size;
                 sock_opt_p = (char *)&rcvbuf_quota;

                 if ((sock_opt_rtn = setsockopt(new_sock,
                                                SOL_SOCKET,
                                                SO_RCVBUF,
                                                sock_opt_p,
                                                sizeof(rcvbuf_quota))) < 0) {

                   sprintf(err_string.par1,"(setsock)");
                   sprintf(err_string.par2,"%s",getErrorMsg(errno));

                   output_err("Get_New_Connections",
                              MI_TNI_SOCK_ERR,
                              MX_ERR_LVL_ERROR,
                              err_string);
                 }

                 sndbuf_quota = tnicon_p->send_buf_size;
                 sock_opt_p = (char *)&sndbuf_quota;

#                if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

                     if ((sock_opt_rtn = setsockopt(new_sock,
                                                    SOL_SOCKET,
                                                    SO_SNDBUF,
                                                    sock_opt_p,
                                                    sizeof(sndbuf_quota))) < 0) {

#                elif defined(XOS_VMS)


#                    ifdef TCPIP_SERVICES

                         if ((sock_opt_rtn = setsockopt(new_sock,
                                                        SOL_SOCKET,
                                                        TCPIP$C_RCVBUF,
                                                        sock_opt_p,
                                                        sizeof(sndbuf_quota))) < 0) {

#                    else

                         if ((sock_opt_rtn = setsockopt(new_sock,
                                                        SOL_SOCKET,
                                                        UCX$C_RCVBUF,
                                                        sock_opt_p,
                                                        sizeof(sndbuf_quota))) < 0) {

#                    endif

#                else

#                    error - OS-specific logic not handled.

#                endif

                   sprintf(err_string.par1,"(setsock)");
                   sprintf(err_string.par2,"%s",getErrorMsg(errno));

                   output_err("Get_New_Connections",
                              MI_TNI_SOCK_ERR,
                              MX_ERR_LVL_ERROR,
                              err_string);
                 }

                 Validate_Connection (new_sock,
                                      new_sock_addr_in,
                                      tnicon_p->local_domain_name[i]);

              }                          /* end socket accept                 */
              break;

        }                                /* end switch (retval)               */
     }
     }                                   /* end for                           */
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Listen_Socket_Init ()                                                      */
/*                                                                            */
/* Purpose: Initialize masks and listening socket information                 */
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

void Listen_Socket_Init ()
{
int i;                                   /* local device count index          */
int reuseaddr_enable = 1;
char *sock_opt_p;                        /* socket option pointer             */

struct hostent *hp[MAX_INTERFACES_PER_TNI];

   err_string = null_err_string;

   for (i = 0; i < MAX_INTERFACES_PER_TNI; i++) {
   
       listen_socket[i] = -1;
   }
   i = 0;

   while (tnicon_p->local_domain_name[i][0] != 0x00) {

     hp[i] = gethostbyname(&tnicon_p->local_domain_name[i][0]);

     memset(&listen_sock_addr[i], 0, sizeof(listen_sock_addr[i]));

     listen_sock_addr[i].sin_family = hp[i]->h_addrtype;
     listen_sock_addr[i].sin_port = htons(tnicon_p->listening_port);
     listen_sock_addr[i].sin_addr = *((struct in_addr *)hp[i]->h_addr);

/*   allocate and open a listening socket                                     */

     if ((listen_socket[i] = socket(hp[i]->h_addrtype, SOCK_STREAM, 0)) < 0 ) {

        sprintf(err_string.par1,"(socket)");
        sprintf(err_string.par2,"%s",getErrorMsg(errno));

        output_err("Listen_Socket_Init",
                   MI_TNI_SOCK_ERR,
                   MX_ERR_LVL_FATAL,
                   err_string);

        exit(1);
     }

     sock_opt_p = (char *)&reuseaddr_enable;

#    if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

         if ((setsockopt(listen_socket[i], SOL_SOCKET, SO_REUSEADDR,
              sock_opt_p, sizeof(reuseaddr_enable))) < 0) {

#    elif defined(XOS_VMS)

         if ((setsockopt(listen_socket[i], SOL_SOCKET, SOCKOPT$C_REUSEADDR,
              sock_opt_p, sizeof(reuseaddr_enable))) < 0) {

#    else

#        error - OS-specific logic not handled.

#    endif

       sprintf(err_string.par1,"(setsockopt)");
       sprintf(err_string.par2,"%s",getErrorMsg(errno));

       output_err("Listen_Socket_Init",
                  MI_TNI_SOCK_ERR,
                  MX_ERR_LVL_FATAL,
                  err_string);

       exit(1);
     }

     if(bind(listen_socket[i], (struct sockaddr*) &listen_sock_addr[i],
       sizeof(listen_sock_addr[i])) < 0) {

       sprintf(err_string.par1,"(bind)");
       sprintf(err_string.par2,"%s",getErrorMsg(errno));

       output_err("Listen_Socket_Init",
                  MI_TNI_SOCK_ERR,
                  MX_ERR_LVL_FATAL,
                  err_string);

       exit(1);
     }

     listen(listen_socket[i], 5);

     listen_timeout[i].tv_sec = LISTEN_WAIT_SEC;
     listen_timeout[i].tv_usec = LISTEN_WAIT_USEC;

     select_timeout.tv_sec = LISTEN_WAIT_SEC;
     select_timeout.tv_usec = LISTEN_WAIT_USEC;

     i++;
   }                                     /* end while                         */

}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Close_Listen_Sockets ()                                                    */
/*                                                                            */
/* Purpose: Closes the listening sockets.                                     */
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

void Close_Listen_Sockets ()
{
    int idx;

    for (idx = 0; idx < MAX_INTERFACES_PER_TNI; idx++ ) {

        if (listen_socket[idx] != -1) {

            close (listen_socket[idx]);
            listen_socket[idx] = -1;
        }
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Read_From_Socket (unsigned short connection, struct mbuf *sockbuf,         */
/*                   int length)                                              */
/*                                                                            */
/* Purpose: This function attempts to read length bytes from the input        */
/*          connection and returns a pointer to an mbuf chain containing      */
/*          the data read from the connection's socket.                       */
/*                                                                            */
/* Input Arguments:                                                           */
/*          connection       Conection number                                 */
/*          length           Number of bytes to be read                       */
/*                                                                            */
/* Output Arguments: None                                                     */
/*          sockbuf          Pointer to an mbuf containing the data read      */
/*                                                                            */
/* Return Value:                                                              */
/*          int              = 0, peer closed connection or socket error      */
/*                           > 0, number of bytes actually read               */
/*                                                                            */
/* Assumptions: The calling function only needs to know if there are data to  */
/*              be processed or not.  Therefore, both a socket error and peer */
/*              closing a connection are treated the same (no action          */
/*              required) by the calling function and result in the same      */
/*              value returned by this function.  Also, it is the             */
/*              responsibility of the calling function to check if fewer      */
/*              than length bytes were actually read.                         */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Read_From_Socket (unsigned short connection, struct mbuf **sockbuf, 
                      int length) {
    int read_len;                          /* number of bytes read            */
    int sock;                              /* input connection's associated   */
                                           /* socket number                   */
    int status;                            /* system service return status    */

    struct mbuf *mymbuf;

    err_string = null_err_string;

    if (tnicon_p->connection[connection].conn_state > CONN_DEFINED) {

        sock = tnicon_p->connection[connection].sock;
        *sockbuf = NULL;

        if( (mymbuf = mb_alloc (length)) == NULL ) {

            sprintf(err_string.par1,"data mbuf");
            sprintf(err_string.par2,"%d",length);

            output_err("Read_From_Socket",
                       MI_TNI_MB_ALLOC,
                       MX_ERR_LVL_ERROR,
                       err_string);

            return(0);
        }

/* Read data directly into mbuf data area and manually set the actual message */
/* length                                                                     */

#       if defined(XOS_VMS)

            status = SYS$SETIMR(SOCK_RD_EFN,
                                &tnicon_p->rd_wrt_tout_time,
                                Sock_Rd_Timeout,
                                connection,
                                0);

            if( status != SS$_NORMAL ) {

                sprintf(err_string.par1, "read");

                output_err("Read_From_Socket",
                           MI_TNI_SOCK_TIMER,
                           MX_ERR_LVL_WARNING,
                           err_string);
            }

#       endif

        read_len = read (sock, mymbuf->m_data, length);

#       if defined(XOS_VMS)

            status = sys$cantim(connection,0);

#       endif

/*
** Make sure the connection has not been closed during the read operation.
** On VMS systems the socket read time-out AST can close the connection even
** though the socket read operation has completed successfully .  If the
** connection has been closed, then return bytes read as zero and free the
** allocated mbuf.
*/
        if (tnicon_p->connection[connection].conn_state > CONN_DEFINED)
        {
            switch (read_len)
            {
    
            case -1:

                sprintf(err_string.par1,"(read)");

#               if defined(XOS_VMS)

                    if (errno == EVMSERR)
                    {
                        errno = EIO;
                    }

#              endif

               sprintf(err_string.par2,"%s",getErrorMsg(errno));

               output_err("Read_From_Socket",
                           MI_TNI_SOCK_ERR,
                       MX_ERR_LVL_ERROR,
                       err_string);      

               Close_Connection (connection);

/*
** Update the read error statistics for the current statistics interval
** and day totals.
*/

                tnicon_p->conn_read_err_count[connection]++;
                tnicon_p->conn_read_err_count_tot[connection]++;

                mb_free_p(mymbuf);          /* free previously allocated mbuf */
                read_len = 0;
                break;

            case 0:

                sprintf(err_string.par1,"%d",connection);

                output_err("Read_From_Socket",
                           MI_TNI_PEER_CLOSE,
                           MX_ERR_LVL_WARNING,
                           err_string);

               Close_Connection (connection);

/*
** Update the read error statistics for the current statistics interval
** and day totals.
*/

                tnicon_p->conn_read_err_count[connection]++;
                tnicon_p->conn_read_err_count_tot[connection]++;

                mb_free_p(mymbuf);       /* free previously allocated mbuf    */
                break;

            default:

/*
** Update the bytes read statistics for the current statistics interval
** and day totals.
*/

                tnicon_p->conn_read_byte_count[connection] += 
                read_len;
                tnicon_p->conn_read_byte_count_tot[connection] += 
                read_len;

                tnicon_p->connection[connection].time_last_rcvd = current_time;

                mymbuf->m_len = read_len;
                *sockbuf = mymbuf;
                break;

            }                                        /* end switch (read_len) */

            if (read_len > 0)
            {
                if( (tnicon_p->dbg_state == DBG_ACTIVE) &&
                    (tnicon_p->print_flag & SOCKET_LEVEL_DBG) ) {

                    Show_Current_Time ("TNI_READ_FROM_SOCKET");
                    fprintf(tnicon_p->dbg_p,
                            "Connection[%d] - Socket[%d]: read_len %d\n\n\t",
                            connection, sock, read_len);

                    DumpMbuf(mymbuf, tnicon_p->dbg_p);
                }
            }
        }
        else
        {
            read_len = 0;
            mb_free_p(mymbuf);              /* free previously allocated mbuf */
        }
    }
    else
    {
        read_len = 0;
    }

    return(read_len);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Send_To_Conn (struct mbuf *writechain,                                     */ 
/*               int   conn,                                                  */
/*               int   send_mode )                                            */
/*                                                                            */
/* Purpose: This function sends TNI data to the host. Depending on the value  */
/*          send_mode write is retried on a possible alternate connection     */
/*          or only tried on the input connection.  The data in the input     */
/*          write chain is ALWAYS freed whether or not the write is successful*/
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/*          writechain       An mbuf chain consisting of PDU(s) to be sent    */
/*                           to the host                                      */
/*                                                                            */
/*          conn             Connection number on which to send               */
/*                                                                            */
/*          send_mode        how to send                                      */
/*                                                                            */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              socket data was actually sent on                 */
/*                                                                            */
/*          if not retried on alternate connection, status = 0, if failed on  */
/*                                                         input socket       */
/*                                                    = input sock, if sent   */
/*                                                      successfully on input */
/*                                                      socket                */
/*          if retried on alternate connection, status = 0 if failed on both  */
/*                                                         sockets            */
/*                                                     = socket number on     */
/*                                                       which we successfully*/
/*                                                       sent, may or may not */
/*                                                       be the same as the   */
/*                                                       input socket         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Send_To_Conn (struct mbuf *writechain, int conn, int send_mode)
{
    int act_conn = 0;                      /* returned connection number      */
    int first_conn_attempted;              /* first connection the write was  */
                                           /* attempted on                    */
    int next_conn;                         /* next available connection       */
    int status;                            /* system service return status    */
    int select_status;                     /* select status                   */
    fd_set rmask;                          /* read mask                       */
    fd_set wmask;                          /* write mask                      */

    struct mbuf *outbuf;                   /* mbuf containing all data to be  */
                                           /* transferred to the host         */
    int retry_count;
    int input_count;                       /* num of bytes in the input write */
                                           /* chain, used for error message   */
    int write_len;                         /* number of bytes written         */
    int host_id_idx;                       /* input socket's host id index    */
    int sock;                              /* input connection's socket number*/
    int curr_len = 0;
    int app_idx;                           /* Application index               */
    int chkauth = 0;                       /* check authentication flag       */
    int retry = 0;                         /* Try alternative connections flag*/

    TNICON *ptr;                           /* pointer to global section info  */

    err_string = null_err_string;

    switch(send_mode)
    {
        case CHKAUTH_NO_ALT_CONN:
            chkauth = 1;
            retry = 0;
            break;
        case CHKAUTH_ALT_CONN:
            chkauth = 1;
            retry = 1;
            break;
        case NO_ALT_CONN:
            chkauth = 0;
            retry = 0;
            break;
        case ALT_CONN:
            chkauth = 0;
            retry = 1;
            break;
    }

    app_idx = tnicon_p->connection[conn].app_idx;

    /* 
     * Authentication should not be checked
     * a) When authentication is not enabled for the application
     * b) When protocol version is less than 2.3 (authentication is introduced in 2.3)
     */

    if (tnicon_p->app[app_idx].auth_required != 1 ||
       tnicon_p->connection[conn].tni_proto_ver < TNI_VERSION_23)
    {
        chkauth = 0;
    }

    ptr = tnicon_p;

    input_count = mb_len_p(writechain);    /* save attempted byte count       */

    if( (outbuf = mb_gather (writechain)) != NULLBUF )
    {
        first_conn_attempted = conn;

        do
        {
            write_len = 0;

/*  Get the input connection's socket number */

            sock = tnicon_p->connection[conn].sock;

            if (((chkauth) && (tnicon_p->connection[conn].auth_state == AUTH_SUCCESS)) ||
                (!chkauth))
            {
            
                if ((sock >= 3) && (sock < FD_SETSIZE))
                {
                    /* 
                     * If write select timeout interval is zero, then do not
                     * perform the select call prior to the write.
                     */
                    if (tnicon_p->wrt_select_tout_invl == 0)
                    {
                        select_status = 1;
                    }
                    else
                    {
                        FD_ZERO(&rmask);
                        FD_ZERO(&wmask);

                        FD_SET(sock, &wmask);

                        /* 
                         * If write select timeout interval is 999, then set
                         * select timeout to block indefinitely.  Otherwise
                         * set the select timeout to the value specified in
                         * MX configuration file.
                         */
                        if (tnicon_p->wrt_select_tout_invl == 999)
                        {
                            select_status = select (sock + 1, 
                                                    &rmask, 
                                                    &wmask, 
                                                    NULL, 
                                                    NULL);  /* NULL => block indefinitely */
                        }
                        else
                        {
                            select_status = select (sock + 1,
                                                    &rmask,
                                                    &wmask,
                                                    NULL,
                                                    &tnicon_p->wrt_select_timeout);
                        }
                    }
                    switch (select_status)
                    {
                    case (-1):

                        sprintf(err_string.par1,"(select)");
                        sprintf(err_string.par2,"%s",getErrorMsg(errno));

                        output_err("Send_To_Conn",
                                   MI_TNI_SOCK_ERR,
                                   MX_ERR_LVL_ERROR,
                                   err_string);
                        break;

                    case (0):
                        break;

                    default:

#                       if defined(XOS_VMS)

                            status = SYS$SETIMR(SOCK_WRT_EFN,
                                                &tnicon_p->rd_wrt_tout_time,
                                                Sock_Wrt_Timeout,
                                                conn,
                                                0);

                            if (status != SS$_NORMAL)
                            {
                                sprintf(err_string.par1, "write");

                                output_err("Send_To_Conn",
                                           MI_TNI_SOCK_TIMER,
                                           MX_ERR_LVL_WARNING,
                                           err_string);
                            }

#                       endif

    /* This is to garantee that the whole data is written out. */

                        write_len = 0;
                        while (write_len < outbuf->m_len)
                        {
                            curr_len = write (sock,
                                              (char *) outbuf->m_data + write_len,
                                              outbuf->m_len - write_len);

    /* Since we are doing all of the error checking below, we will */
    /* just break and set the write_len variable properly */

                            if (curr_len == -1)
                            {
                                write_len = curr_len;
                                break; 
                            }

                            write_len += curr_len;
                        }

#                       if defined(XOS_VMS)

                            status = sys$cantim(conn,0);

#                       endif

                        switch (write_len)
                        {
                        case -1:
                            if (errno != EWOULDBLOCK)
                            {
                                sprintf(err_string.par1,"(write)");
                                sprintf(err_string.par2,"%s",getErrorMsg(errno));

                                output_err("Send_To_Conn",
                                           MI_TNI_SOCK_ERR,
                                           MX_ERR_LVL_ERROR,
                                           err_string);

    /* update the write error statistics */

                                tnicon_p->conn_write_err_count[conn]++;
                                                            /* stats interval*/
                                tnicon_p->conn_write_err_count_tot[conn]++; 
                                                                 /* all day       */
                                Close_Connection (conn);
                            }                            
                            break;

                        case 0:

                            sprintf(err_string.par1,"%d",conn);

                            output_err("Send_To_Conn",
                                       MI_TNI_PEER_CLOSE,
                                       MX_ERR_LVL_WARNING,
                                       err_string);

    /* update the write error statistics */

                            tnicon_p->conn_write_err_count[conn]++;
                                                           /* stats interval*/
                            tnicon_p->conn_write_err_count_tot[conn]++; 
                                                                 /* all day       */
                            Close_Connection (conn);
                            break;

                        default:
                            tnicon_p->connection[conn].time_last_sent
                                = current_time;
                            break;
                        }                          /* end switch (write_len)      */
                        break;
                    }                              /* end switch (select_status)  */
                }                                  /* valid socket number         */
            }

            if( write_len > 0 )                /* successful write, we're done*/
            {
/* update the bytes written statistics */

                tnicon_p->conn_write_byte_count[conn] += write_len;
                tnicon_p->conn_write_byte_count_tot[conn] += write_len;

                act_conn = conn;

                if ((tnicon_p->dbg_state ==DBG_ACTIVE) &&
                    ((ptr->print_flag & SOCKET_LEVEL_DBG) ||
                     (ptr->print_flag & PDU_LEVEL_DBG)))
                {
                    Show_Current_Time ("Send_To_Conn");

                    fprintf(ptr->dbg_p,"Connection[%d]: \n\n\t", conn);
                    DumpMbuf(outbuf, tnicon_p->dbg_p);
                }

                if (first_conn_attempted != conn)
                {
                    sprintf(err_string.par1,"%d",conn);

                    output_err("Send_To_Conn",
                               MI_TNI_ALT_CONN,
                               MX_ERR_LVL_INFO,
                               err_string);
                }
            }
            else
            {
                if (retry == 1)
                {
                    if ((tnicon_p->dbg_state ==DBG_ACTIVE) &&
                        ((ptr->print_flag & SOCKET_LEVEL_DBG) ||
                         (ptr->print_flag & PDU_LEVEL_DBG)))
                    {
                        Show_Current_Time ("Send_To_Conn");

                        fprintf(ptr->dbg_p,"Connection[%d] failure!\n", conn);
                    }

                    conn = Find_Alternate_Connection (conn);

                    if (first_conn_attempted != conn)
                    {
                        if ((tnicon_p->dbg_state ==DBG_ACTIVE) &&
                            ((ptr->print_flag & SOCKET_LEVEL_DBG) ||
                             (ptr->print_flag & PDU_LEVEL_DBG)))
                        {
                            fprintf(ptr->dbg_p,
                                    "Attempting to send PDU over connection[%d]. \n\n",
                                    conn);
                        }
                    }
                }
            }

        } while ((act_conn != conn) && (first_conn_attempted != conn) &&
                 (retry == 1));

        mb_free_p (outbuf);

        if (act_conn == 0)
        {
            sprintf(err_string.par1,
                    "%s",
                    tnicon_p->app[tnicon_p->connection[conn].app_idx].name);

            output_err("Send_To_Conn",
                       MI_TNI_NOSEND_SOCK,
                       MX_ERR_LVL_ERROR,
                       err_string);
        }
    }
    else
    {
        printf ("Buffer of length %d lost while ",input_count);
        printf ("attempting to send to connection %d \n", conn);
    }

    return(act_conn);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void Sock_Rd_Timeout (int conn)                                            */
/*                                                                            */
/* Purpose: This function is called when the socket read timer has expired.   */
/*          The socket associated with the connection is shutdown.  The       */
/*          connection will be closed in the Read_From_Socket function.       */
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

void Sock_Rd_Timeout (int conn)
{
   int sock;                             /* socket number                     */

   err_string = null_err_string;

   sprintf(err_string.par1,"READ");
   sprintf(err_string.par2,"%d",conn);

   output_err("Sock_Rd_Timeout",
              MI_TNI_SOCK_TIMEOUT,
              MX_ERR_LVL_ERROR,
              err_string);

/* update the read error statistics                                           */

   tnicon_p->conn_read_err_count[conn]++;                    /* stats interval*/
   tnicon_p->conn_read_err_count_tot[conn]++;                /* all day       */

   sock = tnicon_p->connection[conn].sock;

   if ((sock >= 0) && (sock < FD_SETSIZE))
   {
      shutdown(sock,2);
   }

   return;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void Sock_Wrt_Timeout (int conn)                                           */
/*                                                                            */
/* Purpose: This function is called when the socket write timer has expired.  */
/*          The socket associated with the connection is shutdown.  The       */
/*          connection will be closed in the Send_To_Conn function.           */
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

void Sock_Wrt_Timeout (int conn)
{
    int sock;                            /* socket number                     */

    err_string = null_err_string;

    sprintf(err_string.par1,"WRITE");
    sprintf(err_string.par2,"%d",conn);

    output_err("Sock_Wrt_Timeout",
               MI_TNI_SOCK_TIMEOUT,
               MX_ERR_LVL_ERROR,
               err_string);

/* update the write error statistics                                          */

    tnicon_p->conn_write_err_count[conn]++;                  /* stats interval*/
    tnicon_p->conn_write_err_count_tot[conn]++;              /* all day       */

    sock = tnicon_p->connection[conn].sock;

    if ((sock >= 0) && (sock < FD_SETSIZE))
    {
       shutdown(sock,2);
    }

    return;
}

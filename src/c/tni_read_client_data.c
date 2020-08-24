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
/*====[TNI_READ_CLIENT_DATA.C] ===============================================*/
/*                                                                            */
/* Read_Client_Data ()                                                        */
/*                                                                            */
/* Purpose: This function checks each socket to determine if the socket has   */
/*          data to be read.  For each socket having data to be read, it      */
/*          calls Read_From_Connection to read an integral number of whole    */
/*          PDUs into an mbuf chain.  Read_Client_Data then calls             */
/*          Duplicate_Chain to duplicate each received PDU into a separate    */
/*          mbuf and Process_Pdu to handle each PDU contained in the          */
/*          duplicate mbuf chain.  Finally the entire mbuf chain is freed.    */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*====[TNI_READ_CLIENT_DATA.C] ===============================================*/
/*                                                                            */

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>

#include "includes_mbuf.h"

void
Read_Client_Data ()
{
int select_status;                       /* select status                     */
fd_set rmask;                            /* read mask                         */

int sock;                                /* socket number                     */

int st;                                  /* Process_Pdu function return status*/

int data_present;                        /* flag to indicate the actual       */
                                         /* presents of data on a socket      */
                                         /* becuase the UNIX select lies      */
int maxfd = -1;                          /* Max file descriptor               */

unsigned short conn;                     /* socket's corresponding connection */
                                         /* number                            */
unsigned short proc_pdu_conn;            /* connection number used for        */
                                         /* process a pdu (value may change)  */

struct mbuf *tcpchain;                   /* chain of mbufs for holding TCP    */
                                         /* transfer data.  The mbufs in      */
                                         /* this chain are duplicated in      */
                                         /* the PDU chain to minimize the     */
                                         /* number of copy operations needed  */
struct mbuf *p;                          /* mbuf loop pointer                 */
struct mbuf *pduchain;                   /* chain of mbufs for holding each   */
                                         /* PDU to be processed.  We assume   */
                                         /* that each mbuf contains 1 entire  */
                                         /* PDU                               */

   err_string = null_err_string;

/* Set up read mask and do select to see if we have anything to read from any */
/* clients                                                                    */

     FD_ZERO(&rmask);

/* Read through all of the sockets that are connected and initialize the read */
/* mask.                                                                      */

     for (conn = 1; conn <= MAX_CONN; conn++ )
     {
	 if (tnicon_p->connection[conn].sock > -1)
         {
	 	FD_SET(tnicon_p->connection[conn].sock, &rmask);

             	if (tnicon_p->connection[conn].sock > maxfd)
                {
			maxfd = tnicon_p->connection[conn].sock;
		}
	 }
     }

     select_status = select (maxfd + 1, 
                             &rmask, 
                             NULL, 
                             NULL,
                             &select_timeout);

/*   update attempt counters - includes ALL sockets                           */

     tnicon_p->read_select_count++;

     switch (select_status)
     {
         case (-1):

              switch (errno)
              {
               /*   case (EINTR):  */
                      /* Do not report an error when the select call is       */
                      /* interrupted by a signal.                             */
               /*       break;  */

                  default:
                      sprintf(err_string.par1,"(select)");
                      sprintf(err_string.par2,"%s",getErrorMsg(errno));

                      output_err("Read_Client_Data",
                                 MI_TNI_SOCK_ERR,
                                 MX_ERR_LVL_ERROR,
                                 err_string);
                      break;
              }
              break;

         case (0):                         /* select timeout                  */
              break; 

         default:

           data_present = 0;

           for (conn = 0; conn < MAX_CONN; conn++ ) {

             if (tnicon_p->connection[conn].conn_state > CONN_DEFINED) {

                 sock = tnicon_p->connection[conn].sock;

                 if (FD_ISSET(sock,&rmask) != 0) {

                    if (data_present == 0) {

                        tnicon_p->read_select_success_count++; 
                        data_present = 1;
                    }

/*                  update count for number of successful socket selects for  */
/*                  this connection/socket                                    */
                    tnicon_p->conn_read_select_success_count[conn]++;

                    if ((tcpchain = Read_From_Connection(conn)) != NULL) {

/*                   The TCP chain is not empty, i.e. we have read something  */
/*                   from the socket.   Duplicate each PDU in the TCP chain   */
/*                   into it's own mbuf                                       */

                       pduchain = Duplicate_Chain (tcpchain, conn);

/*                     Now process the PDU in each mbuf in the PDU chain.     */
/*                     Assume only 1 PDU will be contained in each mbuf.      */

                       if (pduchain != NULLBUF) {
                          proc_pdu_conn = conn;

                          for (p = pduchain; p != NULL; p = p->m_next) {
                              st = Process_Pdu (p->m_len,p->m_data,
                                                &proc_pdu_conn);
                          }
                          p = mb_free_p (pduchain);/* free the PDU mbuf chain */
                       }                           /* end if pduchain not NULL*/

                       else {

                          sprintf(err_string.par1,"%d",sock);
                          sprintf(err_string.par2,"%d",conn);

                          output_err("Read_Client_Data",
                                     MI_TNI_DROP_DATA,
                                     MX_ERR_LVL_ERROR,
                                     err_string);
                       }

                       p = mb_free_p (tcpchain);   /* free the tcp mbuf chain */
                    }                              /* end if chain not NULL   */
                 }                                 /* end if rmask...         */
             }
           }                                       /* end for                 */
           break;
     }                                             /* end switch              */
}

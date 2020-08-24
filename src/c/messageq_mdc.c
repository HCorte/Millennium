#include <stdio.h>
#include <string.h>
#include "p_entry.h"
#include "p_return.h"
#include "p_symbol.h"

/* *****************************************************************
 * Command to compile MESSAGEQ_MDC.C:
 *
 * $CC /stand=vaxc/debug/noopt/lis/include=dmq$user: MESSAGEQ_MDC.C
 *
 *******************************************************************/

/*void Mesq_attach(u_char i[10],u_char *x[10])*/

q_address *q_addr;

struct MQ_Struct_Put
{
   u_char Mess_To_DataMining[1024];
   int Mess_To_Len;
}To_DataMining;
struct MQ_Struct_Get
{
   u_char Mess_From_DataMining[1024];
   int Mess_From_len;
}From_DataMining;

#define MAX_MESSAGE_SIZE 1024

void MessQ_attach(int *status)
{
   int32       attach_mode;
   int32       dmq_status;
   int32       q_num_len;
   int32       q_type;
   int         q_number;
   char        q_num_str[2];
/*   q_address   q_addr;*/

   attach_mode = PSYM_ATTACH_BY_NUMBER;
   q_type      = PSYM_ATTACH_PQ;
   q_number    = 5;
   (void)sprintf( q_num_str, "%d", q_number ); 
   q_num_len   = (int32) strlen( q_num_str );

   dmq_status = pams_attach_q( &attach_mode, &q_addr, &q_type,
                     q_num_str, &q_num_len,
                     (int32 *) 0,   /*  Use default name space */
                     (int32 *) 0,   /*  No name space list len */
                     (int32 *) 0,   /*  No timeout value       */
                     (char *) 0,    /*  Reserved               */
                     (char *) 0);   /*  Reserved               */

/*   printf("\n Status: %d",dmq_status);*/
   if ( dmq_status == PAMS__SUCCESS )
         *status = PAMS__SUCCESS;
   else
         *status = dmq_status;
}

/*void MessQ_put(int *status,char *messagSent)*/
void MessQ_put(int *status)
{
   int         i;
   char        priority;
   char        delivery;
   char        uma;
   int32       large_msg_size = MAX_MESSAGE_SIZE;
   short       msg_class;
   short       msg_type;
   short       msg_size = PSYM_MSG_LARGE;
   int32       dmq_status;
   int32       timeout;
   struct PSB  put_psb;
   q_address q_addrPut;
   char        msg_area[MAX_MESSAGE_SIZE];
   int j;
   
/*   memset(msg_area,0,MAX_MESSAGE_SIZE);
   
   for (j=0;j<To_DataMining.Mess_To_Len;j++) 
      sprintf(msg_area,"%s%d",msg_area,To_DataMining.Mess_To_DataMining[j]);

   for (j=0;j<To_DataMining.Mess_To_Len;j++) 
       msg_area = (char*) To_DataMining.Mess_To_DataMining[j];

   msg_area = (char*) To_DataMining.Mess_To_DataMining;  
   /*msg_area = (u_char*) To_DataMining.Mess_To_DataMining;
   
   messagSent = (char *) malloc(sizeof(To_DataMining.Mess_To_Len));
   
   strncpy(msg_area,messagSent,To_DataMining.Mess_To_Len);
   
   messagSent[To_DataMining.Mess_To_Len] = '\0';*/
   
   q_addrPut.au.group= 0;
   q_addrPut.au.queue= 4;
   
   priority    = '\0';              /* Regular priority; use 0, NOT '0'     */
   msg_class   = msg_type = 0;      /* No class or type at the moment       */
   /*delivery    = PDEL_MODE_NN_MEM;*/  /* No Notification and nonrecoverable   */
   delivery    = PDEL_MODE_WF_DEQ;
   timeout     = 600;               /* Wait 60 seconds before giving up     */
   uma         = PDEL_UMA_DISCL;    /* If can't deliver it, DISCard and Log */
   
   dmq_status = pams_put_msg(
                     To_DataMining.Mess_To_DataMining,
                     &priority,
                     &q_addrPut,        /* passed in */
                     &msg_class,
                     &msg_type,
                     &delivery,
                     &To_DataMining.Mess_To_Len,
                     &timeout,
/*                     (int32 *) 0, */  /*  No timeout value       */
                     &put_psb,
                     &uma,
                     (q_address *) 0, 
                     &large_msg_size,
                     (char *) 0,
                     (char *) 0 );
                     
   if ( dmq_status == PAMS__SUCCESS )
      *status = PAMS__SUCCESS;
   else
      *status = dmq_status;
}

void MessQ_get(int *status)
{
   char*      msg_area;
   char       priority;
   short      g_class;
   short      g_type;
   short      msg_area_len;
   short      len_data;
   int32      sel_filter;
   int32       dmq_status;

                             
   /* Get function parameters */
   priority     = 0;
   sel_filter   = 0;
   /* Call MessageQ function */
   dmq_status = pams_get_msg(From_DataMining.Mess_From_DataMining,
             &priority,
             &q_addr,
             &g_class,
             &g_type,
             &MAX_MESSAGE_SIZE,
             &From_DataMining.Mess_From_len,
             &sel_filter,
             (struct PSB *) 0,
             (struct show_buffer *) 0,
             (int32 *) 0,
             (int32 *) 0, (int32 *) 0, (char *) 0);
   
   if ( dmq_status == PAMS__SUCCESS )
      *status = PAMS__SUCCESS;
   else
      *status = dmq_status;
}

void MessQ_exit(int *status)
{
   int32    dmq_status;
   
   dmq_status    = pams_exit();
   
   if ( dmq_status == PAMS__SUCCESS )
     *status = PAMS__SUCCESS;
   else
     *status = dmq_status;
}

/*void Mesq_attach()
{
   int j;
   u_char msg_area[MAX_MESSAGE_SIZE];
   memset(msg_area,0,MAX_MESSAGE_SIZE);
   
   for (j=0;j<10;j++) 
      sprintf(msg_area,"%s%02X",msg_area,r.a[j]);
/*      strcpy(msg_area,r.a[j]);
      
   printf("\n Valor recebido: [%s]\n",msg_area);
*/   

/*
   printf("\n Valor Recebido VAL: [%s]",msg_area);
   
   int j;
   for (j=0;j<10;j++)
   {
      printf("\n Valor Recebido VAL: [%s]",*i);
   }
}*/

#include <stdio.h>
#include <string.h>
#include "p_entry.h"
#include "p_return.h"
#include "p_symbol.h"
/* cc /stand=vaxc/debug/noopt/lis/include=dmq$user: MESSAGEQ_OLM.C
void Mesq_attach(u_char i[10],u_char *x[10])*/

q_address *q_addr;

struct MQ_Struct_Put
{
   u_char Mess_To_OLM[1024];
   int Mess_To_Len;
}To_OLM;
struct MQ_Struct_Get
{
   u_char Mess_From_OLM[1024];
   int Mess_From_len;
}From_OLM;

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
   q_number    = 2; /*its associated with the bus 13 and queue 2*/
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
   
   for (j=0;j<To_IGS.Mess_To_Len;j++) 
      sprintf(msg_area,"%s%d",msg_area,To_IGS.Mess_To_IGS[j]);

   for (j=0;j<To_IGS.Mess_To_Len;j++) 
       msg_area = (char*) To_IGS.Mess_To_IGS[j];

   msg_area = (char*) To_IGS.Mess_To_IGS;  
   /*msg_area = (u_char*) To_IGS.Mess_To_IGS;
   
   messagSent = (char *) malloc(sizeof(To_IGS.Mess_To_Len));
   
   strncpy(msg_area,messagSent,To_IGS.Mess_To_Len);
   
   messagSent[To_IGS.Mess_To_Len] = '\0';*/
   
   q_addrPut.au.group= 13;
   q_addrPut.au.queue= 1; 
   
   /*from Hades Wildfly09 
   
   %QCT
   !Queue Queue    Byte  Msg   Quota       UCB  Queue      Owner   Conf    Perm   Name     Security
   !Name  Number   Quota Quota Enbl        Send  Type   Queue      Style   Active  Scope
   QUEUE1 1 4080000 75000 ALL . M 0 II N G N
   QUEUE1 2 4080000 75000 ALL . M 0 II N G N
   QUEUE1 3 4080000 75000 ALL . M 0 II N G N
   QUEUE1 4 4080000 75000 ALL . M 0 II N G N
   %EOS

   %GNT
   !
   !Name   Group.Queue     Scope
   JavaQueueIN 13.1 G
   JavaQueueOUT 13.2 G
   JavaQueue2IN 13.3 G
   JavaQueue2OUT 13.4 G
   %EOS

   */
   
   priority    = '\0';              /* Regular priority; use 0, NOT '0'     */
   msg_class   = msg_type = 0;      /* No class or type at the moment       */
   /*delivery    = PDEL_MODE_NN_MEM;*/  /* No Notification and nonrecoverable   */
   delivery    = PDEL_MODE_WF_DEQ;
   timeout     = 100;               /* Wait 10 seconds before giving up     */
   uma         = PDEL_UMA_DISCL;    /* If can't deliver it, DISCard and Log */
   
   dmq_status = pams_put_msg(
                     To_OLM.Mess_To_OLM,
                     &priority,
                     &q_addrPut,        /* passed in */
                     &msg_class,
                     &msg_type,
                     &delivery,
                     &To_OLM.Mess_To_Len,
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
   dmq_status = pams_get_msg(From_OLM.Mess_From_OLM,
             &priority,
             &q_addr,
             &g_class,
             &g_type,
             &MAX_MESSAGE_SIZE,
             &From_OLM.Mess_From_len,
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

/*
   pams_get_msg:

   Priority ranges from 0
   (lowest priority) to 99 (highest priority). For example, priority 1 messages are always
   placed before priority 0 messages

   If no messages are available or meet the selection criteria, then the return status is PAMS__NOMOREMSG.

   (this case is ignore only used 32kb messages thats is static messages) - The receiver program determines whether each message is a FML32 buffer or large message by reading the msg_area_len argument

   Argument Definitions:
      msg_area:
         For static buffer-style messaging, receives the address of a memory region where BEA
         MessageQ writes the contents of the retrieved message.
      priority:
         Supplies the priority level for selective message reception. Priority ranges from 0
         (lowest priority) to 99 (highest priority). If the priority is set to 0, the pams_get_msqw
         function gets messages of any priority. If the priority is set to any value from 1 to 99,
         the pams_get_msqw function gets only messages of that priority.
      source:
         Receives a data structure containing the group ID and queue number of the sender
         program’s primary queue in the following format:
         longword (32 bits) -> | Group ID | Queue Number |
      class:
         Receives the class code of the retrieved message. The class is specified in the
         pams_put_msg function.BEA MessageQ supports the use of symbolic names for class
         argument values. Symbolic class names should begin with MSG_CLAS_. For
         information on defining class symbols, see the p_typecl.h include file.
      type:
         Receives the type code of the retrieved message. The type is specified in the
         pams_put_msg function. BEA MessageQ supports the use of symbolic names for
         type argument values. Symbolic type names begin with MSG_TYPE_. For specific
         information on defining type symbols, see the p_typecl.h include file.
      msg_area_len:
         Supplies the size of the buffer (in bytes) for static message buffers of up to
         32767 bytes. The msg_area buffer is used to store the retrieved message.
      len_data:
         For static buffer-style messaging with messages of up to 32767 bytes, this argument
         receives the number of bytes retrieved from the message queue and stored in the area
         specified by the msg_area argument
      sel_filter:
         Default selection
         Selection by message queue
         Message attributes
         Message source
         Compound selection using the pams_set_select function
         The sel_filter argument is composed of two words as follows:
            longword (32 bits) -> | Select Mode | Select Variable |
         Default Selection (Used one):
            Enables applications to read messages from the queue based on the order in which they
            arrived. The default selection, PSEL_DEFAULT, reads the next pending message from
            the message queue. Messages are stored by priority and then in FIFO order. To specify
            this explicitly, both words in the sel_filter argument should be set to 0.            
         .....
      psb (not used):
         Receives a PAMS Status Block containing the final completion status. The psb
         argument is used when sending or receiving recoverable messages.
      show_buffer (not used):
         Receives additional information which BEA MessageQ extracts from the message
         header.
         The structure of the show_buffer argument is as follows:
            0 Version The version of the show_buffer structure. Valid values
                     are as follows:
                     10 = Version 1.0
                     20 = Version 2.0
                     50 = Version 5.0
      show_buffer_len (not used):
         Supplies the length in bytes of the buffer defined in the show_buffer argument. The
         minimum length is 40 bytes. If the buffer is too small to contain all of the information,
         then the return code PAMS_BUFFEROVF will be in the show_buffer transfer status
      large_area_len (not used):
         Specifies the size of the message area to receive messages larger than 32K
      large_size (not used):
         Returns the actual size of the large message.
      nullarg_3:
         Reserved for BEA MessageQ internal use as a placeholder argument. This argument
         must be supplied as a null pointer.

*/

/*
   pams_get_msg:

   For buffer-style messaging using message buffers up to 32K, this argument
   supplies the length of the message in bytes in the user’s msg_area buffer

   The delivery argument of the pams_put_msg function can be used to guarantee
   message delivery if a system, process, or network fails

   The optional timeout argument lets you set a maximum amount of time for the send
   operation to complete before the function times out. The optional resp_q argument
   allows you to specify an alternate queue for receiving the response messages rather
   than directing responses to the primary queue of the sender program

   Syntax int32 pams_put_msg ( msg_area, priority, target, class, type,
   delivery, msg_size, [timeout], [psb], [uma],
   [resp_q], [large_size], [correlation_id],
   [nullarg_3] )

   msg_area:
      For buffer-style messaging, supplies the address of a memory region or a message
      pointer containing the message to be delivered to the target queue of the receiver
      program.
   
   priority:
      Supplies the priority level for selective message reception. Priority ranges from 0
      (lowest priority) to 99 (highest priority).

   target:
      Supplies the queue number and group ID of the receiver program’s queue address in
      the following format:
         longword (32 bits) -> |Group ID|Queue Number|
   
   class:
      Supplies the class code of message being sent. BEA MessageQ supports the use of
      symbolic names for class argument values. Symbolic class names should begin with
      MSG_CLAS_. For information on defining class symbols, see the p_typecl.h include
      file
   
   type:
      Supplies the type code for the message being sent. BEA MessageQ supports the use of
      symbolic names for type argument values. Symbolic type names begin with
      MSG_TYPE_. For information on defining type symbols, see the p_typecl.h include
      file.

   delivery (in use-> PDEL_MODE_WF_DEQ):
      Supplies the delivery mode for the message using the following format:
         PDEL_MODE_sn_dip—where sn is one of the following sender 
         notification constants:
            WF—Wait for completion
            n AK—Asynchronous acknowledgment
            n NN—No notification 
         And dip is one of the following delivery interest point constants:
            ACK—Read from target queue and explicitly acknowledged using the
            pams_confirm_msg function. ACK can also be an implicit acknowledgement
            sent after the second pams_get_msg call by the receiving application.
            n CONF—Delivered from the DQF and explicitly confirmed using the
            pams_confirm_msg function (recoverable)
            n DEQ—Read from the target queue
            n DQF—Stored in the destination queue file (recoverable)
            n MEM—Stored in the target queue
            n SAF—Stored in the store and forward file (recoverable)

   msg_size:
      For buffer-style messaging using message buffers up to 32K, supplies the length of the
      message in bytes in the user’s msg_area buffer 

   timeout:
      Supplies the maximum amount of time the pams_put_msg function waits for a
      message to arrive before returning control to the application. The timeout value is
      entered in tenths (0.1) of a second. A value of 100 indicates a timeout of 10 seconds.
      If the timeout occurs before a message arrives, the status code PAMS__TIMEOUT is
      returned. Specifying 0 as the timeout value sets the timeout to the default value of 30
      seconds.  

   psb (not used):
      Receives a value in the PAMS Status Block specifying the final completion status. The
      psb argument is used when sending or receiving recoverable messages. The PSB
      structure stores the status information from the message recovery system and may be
      checked after sending or receiving a message.  

   uma (used -> PDEL_UMA_DISCL):
      Supplies the action to be performed if the message cannot be stored at the specified
      delivery interest point. The format of this argument is PDEL_UMA_XXX where XXX is
      one of the following symbols:   
         DISC Discard message
         DISCL Discard after logging message
         DLJ Dead letter journal
         DLQ Dead letter queue
         RTS Return to sender
         SAF Store and Forward 

   resp_q (not used): 
      Supplies a q_address to use as the alternate queue for receiving response messages
      from the receiver program. The sender program must be attached to the queue
      specified in the resp_q argument to receive the response messages. The resp_q
      argument has the following format:
         longword (32 bits) -> |Group ID | Queue Number |
      The group ID is always specified as zero because the sender program cannot assign a
      response queue outside its group.

   large_size (used -> #define MAX_MESSAGE_SIZE 1024):
      Supplies the actual size of the large message written to the message buffer.

   correlation_id:
      Supplies the correlation id, a user-defined identifier stored as a 32-byte value

   nullarg_3:
      Reserved for BEA MessageQ internal use as a placeholder argument. This argument
      must be supplied as a null pointer.           
*/
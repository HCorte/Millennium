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
/*====[MX_PROTO_CALCS.C]======================================================*/
/*                                                                            */
/* Purpose: These functions calculate lengths and sizes of various protocol   */
/*          components.  The component are used to build PDUs.                */
/*                                                                            */
/* Functions:                                                                 */
/*          Calc_Max_Param_Len (unsigned char param_code)                     */
/*          Calc_Offsets ()                                                   */
/*          Calc_Pdu_Hdr_Size (unsigned char pdu_type)                        */
/*          Calc_Term_Msg_Hdr_Size (unsigned char term_id_method)             */
/*          Calc_Rpc_Msg_Hdr_Size()                                           */
/*          Determine_Len (unsigned long int value)                           */
/*                                                                            */
/*====[MX_PROTO_CALCS.C]======================================================*/
/*                                                                            */

#include <limits.h>
#include <stdlib.h>
#include <stdio.h>

#include "includes.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Calc_Max_Param_Len (unsigned char param_code)                              */
/*                                                                            */
/* Purpose: This function calculates the maximum size the input parameter's   */
/*          code/length/value block can assume.  It is used to allocate       */
/*          mbufs of a correct (worse case) size.                             */
/*                                                                            */
/* Input Arguments:                                                           */
/*          param_code       Parameter code                                   */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              # Maximum number of bytes in input parameter     */
/*                             code/length/value block                        */
/*                           0 Error                                          */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Calc_Max_Param_Len (unsigned char param_code)
{
struct TNI_DATA buf;

   err_string = null_err_string;

   switch (param_code)
   {
/*    These parameters ALWAYS take on a parameter value length of 4 bytes.    */
      case TERM_SERVER_ID:
      case TERM_CLIENT_ID:

/*    These parameters can take on a parameter value length of 1 to 4 bytes.  */
/*    Assume worst case of 4 bytes                                            */
      case MAX_MESSAGE_SIZE:
      case MAX_PDU_SIZE:
      case MSG_BLOCK_TIME:
      case KEEP_ALIVE_TIME:
      case NUMBER_MESSAGES:
      case MSG_LENGTH:
      case COMMAND_CODE:
      case SALT_LENGTH:
      case HASH_LENGTH:
      case ENC_KEY_LENGTH:
           return (sizeof(buf.param_data.i_param_value) +
                   sizeof(buf.param_length) +
                   sizeof(buf.param_code));
           break;

      case CLIENT_ID:                   /* ALWAYS 1 byte                      */
      case DELIVERY_MODE:
      case PRIMARY_STATUS:
      case RESULT_CODE:
      case TERM_METHOD:
      case MESSAGING_TYPE:
      case HASH_ALGORITHM:
      case ENC_KEY_X_METHOD:
      case ENC_KEY_TYPE:
      case ENC_MODE:
           return (sizeof(buf.param_data.param_value[0]) +
                   sizeof(buf.param_length) +
                   sizeof(buf.param_code));
           break;

      case TERM_CLIENT_TAG:
           return (MAX_TERM_CLIENT_TAG_LEN);
           break;

      case APPLICATION_NAME:
           return(MXSRV_MAX_APP_NAME_LEN);
           break;

      case RPC_REQUEST_TAG:
           return(MAX_RPC_RQST_TAG_LEN);
           break;

      case SESSION_TAG:
           return(MAX_SESSION_TAG_LEN);
           break;

      case CORRELATION_TAG:
           return(MAX_CORRELATION_TAG_LEN);
           break;

      default:

          sprintf(err_string.par1, "parameter code");
          sprintf(err_string.par2, "%d", param_code);

          output_err("Calc_Max_Param_Len",
                     MI_TNI_UNKNOWN,
                     MX_ERR_LVL_ERROR,
                     err_string);

          return (0);
          break;
   }
   return(0);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Calc_Offsets ()                                                            */
/*                                                                            */
/* Purpose: This function computes byte offsets into the TNI fixed header     */
/*          fields                                                            */
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

void Calc_Offsets () {

   struct TNI_FIXED_HDR header;

   proto_off = 0;
   version_off = proto_off + (unsigned short )sizeof (header.proto_id);
   pdu_len_off = version_off + (unsigned short )sizeof (header.version_id);
   pdu_type_off = pdu_len_off + (unsigned short )sizeof (header.pdu_length);

}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Calc_Pdu_Hdr_Size (unsigned char pdu_type)                                 */
/*                                                                            */
/* Purpose: This function calculates the maximum PDU header size for the      */
/*          input PDU type.  It assumes that all optional parameters,         */
/*          excluding the terminal tag, are included and that each variable   */
/*          length integer parameter takes on the maximum length of 4 bytes.  */
/*          It is used to allocate mbufs of a correct (worse case) size.      */
/*                                                                            */
/* Input Arguments:                                                           */
/*          pdu_type         TNI PDU type                                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              Maximum number of bytes in PDU header            */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Calc_Pdu_Hdr_Size (unsigned char pdu_type)
{
int len;                                 /* returned length                   */

   len = sizeof(struct TNI_FIXED_HDR);   /* ALL PDUs have a fixed header      */

/* Now add variable header parameters if the input PDU allows any             */
   switch ( pdu_type)
   {

       case CLIENT_RESPONSE_PDU:

            len += Calc_Max_Param_Len (CLIENT_ID);
            len += Calc_Max_Param_Len (MAX_MESSAGE_SIZE);
            len += Calc_Max_Param_Len (MAX_PDU_SIZE);
            len += Calc_Max_Param_Len (MSG_BLOCK_TIME);
            len += Calc_Max_Param_Len (KEEP_ALIVE_TIME);
            len += Calc_Max_Param_Len (PRIMARY_STATUS);
            len += Calc_Max_Param_Len (TERM_METHOD);
            len += Calc_Max_Param_Len (APPLICATION_NAME);
            len += Calc_Max_Param_Len (MESSAGING_TYPE);
            len += Calc_Max_Param_Len (ENC_KEY_X_METHOD);
            len += Calc_Max_Param_Len (ENC_KEY_TYPE);
            len += Calc_Max_Param_Len (RESULT_CODE);
            break;

       case SERVER_REQUEST_PDU:
            len += Calc_Max_Param_Len (CLIENT_ID);
            len += Calc_Max_Param_Len (MAX_MESSAGE_SIZE);
            len += Calc_Max_Param_Len (MAX_PDU_SIZE);
            len += Calc_Max_Param_Len (MSG_BLOCK_TIME);
            len += Calc_Max_Param_Len (KEEP_ALIVE_TIME);
            len += Calc_Max_Param_Len (PRIMARY_STATUS);
            break;

       case TERMINAL_SERVER_DATA_PDU:
            len += Calc_Max_Param_Len (NUMBER_MESSAGES);
            break;

       case SERVER_ALIVE_PDU:
            break;

       case SERVER_CMD_REQUEST_PDU:
            len += Calc_Max_Param_Len (COMMAND_CODE);
            break;

       case ERROR_PDU:
            len += Calc_Max_Param_Len (RESULT_CODE);
            len += Calc_Max_Param_Len (TERM_SERVER_ID);
            len += Calc_Max_Param_Len (TERM_CLIENT_ID);
            len += Calc_Max_Param_Len (CLIENT_ID);
            len += Calc_Max_Param_Len (RPC_REQUEST_TAG);
            len += Calc_Max_Param_Len (CORRELATION_TAG);
            break;

       case RPC_SERVER_DATA_PDU:
            len += Calc_Max_Param_Len (NUMBER_MESSAGES);
            break;

       case CLT_SESSION_RESP_PDU:
            len += Calc_Max_Param_Len (SESSION_TAG);
            len += Calc_Max_Param_Len (RESULT_CODE);
            break;

       case SRV_CHALLENGE_RQST_PDU:
            len += Calc_Max_Param_Len (HASH_ALGORITHM);
            len += Calc_Max_Param_Len (SALT_LENGTH);
            break;

       case SRV_CHALLENGE_NOTIFY_PDU:
            len += Calc_Max_Param_Len (RESULT_CODE);
            break;

       case SRV_ENC_KEY_RQST_PDU:
            len += Calc_Max_Param_Len (ENC_KEY_X_METHOD);
            len += Calc_Max_Param_Len (ENC_KEY_TYPE);
            len += Calc_Max_Param_Len (ENC_KEY_LENGTH);
            break;

   }                                     /* end of PDU type switch            */

   return (len);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Calc_Term_Msg_Hdr_Size (unsigned char term_id_method)                      */
/*                                                                            */
/* Purpose: This function calculates the maximum Termnal msg header size for  */
/*          a given message.  It assumes that that each variable length       */
/*          integer parameter takes on the maximum length of 4 bytes.  It is  */
/*          used to allocate mbufs of a correct (worse case) size.            */
/*                                                                            */
/* Input Arguments:                                                           */
/*          term_id_method   Terminal identification method                   */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              Maximum number of bytes in Terminal msg header   */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Calc_Term_Msg_Hdr_Size (unsigned char term_id_method)
{
   int len = 0;                          /* returned length                   */

   len += Calc_Max_Param_Len (DELIVERY_MODE);
   len += Calc_Max_Param_Len (MSG_LENGTH);
   len += Calc_Max_Param_Len (CORRELATION_TAG);
   len += Calc_Max_Param_Len (ENC_MODE);

   switch (term_id_method)
   {

       case METH_TERM_CLIENT_ID:

            len += Calc_Max_Param_Len (TERM_CLIENT_ID);
            break;

       case METH_TERM_CLIENT_TAG:

            len += Calc_Max_Param_Len (TERM_CLIENT_TAG);
            break;

       case METH_TERM_SERVER_ID:

            len += Calc_Max_Param_Len (TERM_SERVER_ID);
            break;

   }                                     /* end of terminal id method switch  */

   return (len);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Calc_Rpc_Msg_Hdr_Size ()                                                   */
/*                                                                            */
/* Purpose: This function calculates the maximum RPC message header size for  */
/*          a given message.  It assumes that that each variable length       */
/*          integer parameter takes on the maximum length of 4 bytes.  It is  */
/*          used to allocate mbufs of a correct (worse case) size.            */
/*                                                                            */
/* Input Arguments: None                                                      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              Maximum number of bytes in RPC message header    */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Calc_Rpc_Msg_Hdr_Size (void)
{
   int len = 0;                          /* returned length                   */

   len += Calc_Max_Param_Len (RPC_REQUEST_TAG);
   len += Calc_Max_Param_Len (DELIVERY_MODE);
   len += Calc_Max_Param_Len (MSG_LENGTH);
   len += Calc_Max_Param_Len (ENC_MODE);

   return (len);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Determine_Len (unsigned long int value)                                    */
/*                                                                            */
/* Purpose: This function returns the minimum number of bytes required to     */
/*          represent the input value.                                        */
/*                                                                            */
/* Input Arguments:                                                           */
/*          value            Value                                            */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              0 error                                          */
/*                           1 byte                                           */
/*                           2 bytes                                          */
/*                           4 bytes                                          */
/*                                                                            */
/* Assumptions: There is no such thing as a 3 byte integer                    */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

unsigned char Determine_Len (unsigned long int value)
{

   if ((value == 0) || ((value > 0) && (value <= UCHAR_MAX)))
       return (sizeof(unsigned char));
   else if ((UCHAR_MAX + 1 <= value) && (value <= USHRT_MAX))
       return (sizeof(unsigned short));
   else if ((USHRT_MAX + 1 <= value) && (value <= ULONG_MAX))
       return (sizeof(unsigned long));
   else
       return (0);
}

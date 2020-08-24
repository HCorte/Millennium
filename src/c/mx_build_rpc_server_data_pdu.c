static const char *fileid = "";

/*
 * ===[mx_build_rpc_server_data_pdu.c]==================================== 
 *
 * Description:
 *
 * Functions to encode the RPC Server Data PDU.
 *
 * Functions:
 *
 * Build_Rpc_Msg_Header      - build RPC message header
 * Build_Rpc_Server_Data_Pdu - build RPC Server Data PDU
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
 * =======================================================================
 */

#include "includes_mbuf.h"

/* [Build_Rpc_Msg_Header]
 *
 * Summary:
 *
 * Build_Rpc_Msg_Header (unsigned short conn,
 *                       char *rpc_rqst_tag, 
 *                       long int msg_len,
 *                       long int del_mode,
 *                       int enc_mode)
 *
 * conn             - Connection the PDU is to be sent on
 * rpc_rqst_tag    - pointer to the RPC request tag
 * msg_len         - length in bytes of RPC request message
 * del_mode        - delivery mode
 * enc_mode        - encryption mode
 *
 * Description:
 *
 * This function builds the terminal message header for each         
 * RPC message to be included in a RPC Server Data PDU. 
 *
 * Returns Values:
 *
 * (struct mbuf *)  Pointer to mbuf containing terminal message
 *                  header
 *
 */

struct mbuf *Build_Rpc_Msg_Header (unsigned short conn,
                                   char *rpc_rqst_tag,
                                   long int msg_len,
                                   long int del_mode,
                                   int enc_mode)
{

int off_set;                             /* running sum of number of bytes in */
                                         /* terminal message header.  Same as */
                                         /* offset into mbuf data area        */
int len;                                 /* length of a single parameter      */
                                         /* code/value pair                   */
int hdr_len = 0;                         /* length off RPC message header     */
                                         /* parameters                        */
struct mbuf *my_mbuf;                    /* returned mbuf pointer             */
unsigned char *mbuf_data_ptr;            /* pointer to mbuf data area         */

   err_string = null_err_string;

   hdr_len = Calc_Rpc_Msg_Hdr_Size();

   if ((my_mbuf = mb_alloc (hdr_len)) == NULL)
   {
      sprintf(err_string.par1,"rpc message header");
      sprintf(err_string.par2,"%d",hdr_len);
      sprintf (err_string.par3, "Build_Rpc_Msg_Header");

      output_err ("Build_Rpc_Msg_Header",
                  MI_TNI_MB_ALLOC,
                  MX_ERR_LVL_ERROR,
                  err_string);

      return (NULL);
   }
   off_set = 0;

/* Get pointer to the data area of the mbuf we just allocated                 */

   mbuf_data_ptr = my_mbuf->m_data;

/* RPC request tag                                                            */

   len = Build_Parameter (RPC_REQUEST_TAG,
                          0,
                          rpc_rqst_tag,
                         (struct TNI_DATA *)(mbuf_data_ptr+off_set));
   off_set += len;

/* delivery mode                                                              */

   len = Build_Parameter (DELIVERY_MODE,
                          del_mode,
                          NULL,
                         (struct TNI_DATA *)(mbuf_data_ptr+off_set));
   off_set += len;

/* encryption mode                                                            */

   if (tnicon_p->connection[conn].tni_proto_ver >= TNI_VERSION_30)
   { 
       len = Build_Parameter (ENC_MODE,
                              enc_mode,
                              NULL,
                             (struct TNI_DATA *)(mbuf_data_ptr+off_set));
       off_set += len;
   }

/* message length is ALWAYS the last parameter                                */

   len = Build_Parameter (MSG_LENGTH,
                          msg_len,
                          NULL,
                         (struct TNI_DATA *)(mbuf_data_ptr+off_set));

   my_mbuf->m_len = off_set + len;

   return (my_mbuf);
}

/* [Build_Rpc_Server_Data_Pdu]
 *
 * Summary:
 * 
 * Build_Rpc_Server_Data_Pdu(unsigned short conn,
 *                           struct mbuf *input_chain,
 *                           struct TNI_PARAM_PAIR *params)
 *
 * conn             - Connection the PDU is to be sent on
 *
 * input_chain      - Pointer to an mbuf chain consisting of RPC request 
 *                   message header(s) and their corresponding RPC
 *                   request message(s).
 *
 *                           ---------------------------------                
 *          inputchain -->   | RPC message 1 header          |               
 *                           | inmbuf->m_next                | --|          
 *                           ---------------------------------   |         
 *                      ------------------------------------------        
 *                      |    ---------------------------------           
 *                      |--> | RPC message 1                 |          
 *                           | inmbuf->m_next->m_next        | --|     
 *                           ---------------------------------   |    
 *                      ------------------------------------------   
 *                      |    ---------------------------------      
 *                      |--> | RPC message 2 header          |     
 *                           | inmbuf->m_next->m_next->m_next| --|
 *                           ---------------------------------   |            
 *                      ------------------------------------------           
 *                      |    ---------------------------------              
 *                      |--> | RPC message 2                 |             
 *                           | inmbuf->m_next->...->m_next   | --|        
 *                           ---------------------------------   |       
 *                      ------------------------------------------      
 *                      |                  .                           
 *                      |                  .                                 
 *                      |-->               .                                  
 *                                                             --|            
 *                                                               |            
 *                      ------------------------------------------           
 *                      |    ---------------------------------                
 *                      |    | last RPC message              |                
 *                      -->  | ... ->m_next = NULL           |                
 *                           ---------------------------------                
 *
 * params          - Pointer to a structure containing parameter
 *                   code/value pairs.  The list is terminated with a
 *                   parameter code of zero.
 *
 * Description:
 *
 * This function constructs an mbuf chain containing a single        
 * RPC Server Data PDU.  Two separate mbufs are constructed and    
 * chained together to form a single chain.  The first mbuf created
 * contains the variable part of the PDU based upon the input     
 * parameters.  The second mbuf created contains the fixed header
 * of the RPC Server Data PDU.  The mbuf containing the variable part 
 * of the PDU is then appended to the mbuf containing the fixed part 
 * of the PDU to form an mbuf chain.  Finally the input chain is    
 * appended to the mbuf chain to form the returned mbuf chain.     
 *
 * Returns Values: 
 *
 * (struct mbuf *)           Pointer to the mbuf chain containing the single  
 *                           Terminal Data PDU                                
 *                                                                            
 *                           ---------------------------------                
 *       (struct mbuf *) --> | Fixed part of PDU             |                
 *                           | fixed_hdr_mbuf->m_next        | --|            
 *                           ---------------------------------   |            
 *                      ------------------------------------------            
 *                      |    ---------------------------------                
 *                      |--> | Variable part of PDU          |                
 *                           | var_hdr_mbuf->m_next          | --|            
 *                           ---------------------------------   |            
 *                      ------------------------------------------            
 *                      |    ---------------------------------                
 *     (inputchain) --> |--> | RPC message 1 header          |                
 *                           | var_hdr_mbuf->m_next->m_next  | --|            
 *                           ---------------------------------   |            
 *                      ------------------------------------------            
 *                      |    ---------------------------------                
 *                      |--> | RPC message 1                 |                
 *                           | inmbuf->m_next->m_next        | --|            
 *                           ---------------------------------   |            
 *                      ------------------------------------------            
 *                      |    ---------------------------------                
 *                      |--> | RPC message 2 header          |                
 *                           | inmbuf->m_next->m_next->m_next| --|            
 *                           ---------------------------------   |            
 *                      ------------------------------------------           
 *                      |    ---------------------------------                
 *                      |--> | RPC message 2                 |                
 *                           | inmbuf->m_next->...->m_next   | --|            
 *                           ---------------------------------   |            
 *                      ------------------------------------------            
 *                      |                  .                                  
 *                      |                  .                                  
 *                      |-->               .                                  
 *                                                             --|            
 *                                                               |            
 *                      ------------------------------------------            
 *                      |    ---------------------------------                
 *                      |    | last RPC message              |                
 *                      -->  | ... ->m_next = NULL           |                
 *                           ---------------------------------                 
 *
 */

struct mbuf *Build_Rpc_Server_Data_Pdu(unsigned short conn,
                                       struct mbuf *input_chain,
                                       struct TNI_PARAM_PAIR *params)
{
    int m_len;                             /* length variable                 */
    int data_len;                          /* number of bytes in inputchain   */
    struct mbuf *fixed_hdr_mbuf;           /* pointer to mbuf containing fixed*/
                                           /* header part of Terminal Data PDU*/
    struct mbuf *var_hdr_mbuf;             /* pointer to mbuf containing      */
                                           /* variable part of Term Data PDU  */
    struct TNI_FIXED_HDR *fixed_buf;       /* pointer to data area of         */
                                           /* fixed_hdr_mbuf                  */
    char *var_buf;                         /* pointer to data area of         */
                                           /* var_hdr_mbuf                    */

/* Get total number of data bytes in the input mbuf chain                     */

    err_string = null_err_string;

    data_len = mb_len_p (input_chain);

    if (data_len == 0)
    {                                      /* just report the error           */
        sprintf (err_string.par1,
                 "Build_Rpc_Server_Data_Pdu");

        output_err ("Build_Rpc_Server_Data_Pdu",
                    MI_TNI_ZERO_CHAIN,
                    MX_ERR_LVL_ERROR,
                    err_string);
    }

/* Determine the maximum size of the fixed and variable RPC SERVER DATA PDU   */
/* header so we can allocate an mbuf of the correct size.  The returned value */
/* is larger than we need for the variable part only, but it's close enough   */

    m_len = Calc_Pdu_Hdr_Size (RPC_SERVER_DATA_PDU);

    if( (var_hdr_mbuf = mb_alloc_pri (m_len)) == NULL )
    {
        sprintf (err_string.par1, "variable data");
        sprintf (err_string.par2, "%d",m_len);
        sprintf (err_string.par3, "Build_Rpc_Server_Data_Pdu");

        output_err ("Build_Rpc_Server_Data_Pdu",
                    MI_TNI_MB_ALLOC,
                    MX_ERR_LVL_ERROR,
                    err_string);

        mb_free_p(input_chain);            /* free mbuf chain, can not send   */

        return(NULL);
    }

/* Build RPC SERVER DATA PDU variable header first to make the total PDU      */
/* length easier to calcuate for the RPC SERVER DATA PDU fixed header         */

    var_buf = (char *)var_hdr_mbuf->m_data;

    var_hdr_mbuf->m_len = 0;

    while( params->param_code )            /* parameter list is 0 terminated  */
    {
        m_len = Build_Parameter (params->param_code,
                                 params->param_value,
                                 params->param_value_char,
                                 (struct TNI_DATA *)var_buf);
        var_hdr_mbuf->m_len += m_len;
        var_buf += m_len;
        params++;
    }

/* Determine the maximum size of the fixed and variable RPC SERVER DATA PDU   */
/* header so we can allocate an mbuf of the correct size.  The returned value */
/* is larger than we need for the fixed part only, but it's close enough      */

    m_len = sizeof(struct TNI_FIXED_HDR);

    if( (fixed_hdr_mbuf = mb_alloc_pri (m_len)) == NULL )
    {
        sprintf (err_string.par1, "fixed header");
        sprintf (err_string.par2, "%d", m_len);
        sprintf (err_string.par3, "Build_Rpc_Server_Data_Pdu");

        output_err ("Build_Rpc_Server_Data_Pdu",
                    MI_TNI_MB_ALLOC,
                    MX_ERR_LVL_ERROR,
                    err_string);

        mb_free_p (var_hdr_mbuf);          /* free previously allocated mbuf  */
        mb_free_p(input_chain);            /* free mbuf chain, can not send   */

        return(NULL);
    }

/* Build RPC SERVER DATA PDU fixed header                                     */

    fixed_buf = (struct TNI_FIXED_HDR *)fixed_hdr_mbuf->m_data;
    fixed_buf->proto_id = TNI_PROTOCOL;          

/* If the TNI protocol version is unknown then set the protocol version to    */
/* the default verson                                                         */

    if (tnicon_p->connection[conn].tni_proto_ver == TNI_VERSION_UNKNOWN)
    {
        fixed_buf->version_id  = DEFAULT_TNI_VERSION;
    }
    else
    {
        fixed_buf->version_id  = tnicon_p->connection[conn].tni_proto_ver;
    }

    fixed_buf->pdu_type = RPC_SERVER_DATA_PDU; 
    fixed_hdr_mbuf->m_len = sizeof (struct TNI_FIXED_HDR);

    fixed_buf->pdu_length = data_len + fixed_hdr_mbuf->m_len +
                            var_hdr_mbuf->m_len;

    fixed_buf->pdu_length = htons (fixed_buf->pdu_length);

/* Now append the 3 mbuf chains - fixed header, variable header, input data   */

    mb_append (&fixed_hdr_mbuf, var_hdr_mbuf);
    mb_append (&fixed_hdr_mbuf, input_chain);

   if ((tnicon_p->dbg_state ==DBG_ACTIVE) &&
       (tnicon_p->print_flag & PDU_LEVEL_DBG))
   {
       fprintf(tnicon_p->dbg_p,"\nBuilt rpc server data PDU");
   }

    return(fixed_hdr_mbuf);
}

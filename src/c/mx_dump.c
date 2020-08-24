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
/*====[MX_DUMP.C]=============================================================*/
/*                                                                            */
/* Purpose: These functions provide basic file io for logging and             */
/*          debugging purposes                                                */
/*                                                                            */
/* Functions:                                                                 */
/*                                                                            */
/* LogPdu(struct PDU_STRUCT *pdu_struct,short term_net_id,unsigned short conn)*/
/* int DumpPdu(struct PDU_STRUCT *pdu_struct, long int pdulen, FILE *file)    */
/* void DumpMbufChain(struct mbuf *mbufchain, unsigned short conn)            */
/* int DumpMbuf(struct mbuf *p, FILE *file)                                   */
/* int DumpMbuf(struct mbuf *p, FILE *file)                                   */
/* void LogMsg(char *error_string)                                            */
/* void ChkExcessiveLogging(void)                                             */

/*                                                                            */
/*====[MX_DUMP.C]=========================================================*/
/*                                                                            */

#include "includes_mbuf.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void LogPdu(struct PDU_STRUCT *pdu_struct,                                 */
/*          short term_net_id, unsigned short conn)                           */
/*                                                                            */
/* Purpose: This function logs pdus to the logfile                            */
/*                                                                            */
/* Input Arguments: Pdu, Terminal Id, connection number                       */
/*                                                                            */
/*                                                                            */
/* Output Arguments: LogFile and the amount logged over last interval         */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void LogPdu(struct PDU_STRUCT *pdu_struct,  
            short term_net_id, unsigned short conn) {

    char *current_time;

    CONNECTION_INFO *conn_info;    
    long int dump_length;                           
    int max_pdu_length;    
    long int pdu_length;                 /* PDU length in HOST order          */
    int app_num;

    if( tnicon_p->log_state == LOG_ACTIVE ) {

        pdu_length  = ntohs (pdu_struct->fixed_header.pdu_length);
        conn_info = (CONNECTION_INFO *)&tnicon_p->connection[conn];
        max_pdu_length = conn_info->max_pdu_size; 
        app_num = conn_info->app_idx;

        if( (pdu_length < max_pdu_length) || (max_pdu_length == 0) ) {

            dump_length = pdu_length; 

        } else {

            dump_length = max_pdu_length;

        } 

        current_time = Get_Time();

        tnicon_p->amount_logged += 
        fprintf(tnicon_p->log_p,
                "%s App %s, Connection[%d] - remote_domain_name: %s\n\n",
                current_time, 
                tnicon_p->app[app_num].name, 
                conn,
                conn_info->remote_domain_name);

        if( term_net_id == -1 ) {

            tnicon_p->amount_logged += 
            fprintf(tnicon_p->log_p, 
                    "%s App %s, Connection[%d] - PDU Data,"
                    " length[%ld] (max pdu length[%d]):\n\n\t",
                    current_time, 
                    tnicon_p->app[app_num].name,
                    conn, 
                    pdu_length, 
                    max_pdu_length);

        } else {

            tnicon_p->amount_logged += 
            fprintf(tnicon_p->log_p,
                    "%s App %s, Connection[%d] - PDU Data,"
                    " length[%ld] (max pdu length[%d]),"
                    " for Terminal[%d]:\n\n\t",
                    current_time,
                    tnicon_p->app[app_num].name, 
                    conn, 
                    pdu_length, 
                    max_pdu_length, 
                    term_net_id);
        }

        tnicon_p->amount_logged += DumpPdu(pdu_struct, 
                                           dump_length, 
                                           tnicon_p->log_p);
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void DumpPdu(struct PDU_STRUCT *pdu_struct, long int pdulen, FILE *file)   */
/*                                                                            */
/* Purpose: This function logs pdus to the logfile                            */
/*                                                                            */
/* Input Arguments: Pdu, Pdu length, file to dump pdu to                      */
/*                                                                            */
/*                                                                            */
/* Output Arguments: File and the amount logged over last interval            */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int DumpPdu(struct PDU_STRUCT *pdu_struct, long int pdulen, FILE *file) {

    unsigned short length;
    unsigned char *app_msg_ptr;
    int i;

    int char_cnt = 0;
    int amt_logged = 0;

    app_msg_ptr = pdu_struct->msg_data;

    length = htons(pdu_struct->fixed_header.pdu_length);

    amt_logged += fprintf(file, "%2x %2x %2x %2x %2x ",
                          pdu_struct->fixed_header.proto_id, 
                          pdu_struct->fixed_header.version_id,
                          (length & 0xFF00) >> 8,
                          length & 0x00FF, 
                          pdu_struct->fixed_header.pdu_type);

    char_cnt = 5;

    for( i = 0; i < (pdulen - sizeof(struct TNI_FIXED_HDR)) ; 
       i++ ) {

        if( char_cnt < 22 ) {

            amt_logged += fprintf(file, "%2x ", *(app_msg_ptr+i));
            char_cnt++;

        } else {

            amt_logged += fprintf(file, "%2x\n\t", *(app_msg_ptr+i));
            char_cnt = 0;

        }        
    }

    if( char_cnt != 0 ) {

        amt_logged += fprintf(file, "\n");

    }

    return amt_logged;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void DumpMbufChain(struct mbuf *mbufchain, unsigned short conn)            */
/*                                                                            */
/* Purpose: This function mbufs to the logfile                                */
/*                                                                            */
/* Input Arguments: Mbuf, connection number                                   */
/*                                                                            */
/*                                                                            */
/* Output Arguments: LogFile and the amount logged over last interval         */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void DumpMbufChain(struct mbuf *mbufchain, unsigned short conn) {

    struct mbuf *p;                          /* mbuf loop pointer             */
    int char_cnt;

    if( (tnicon_p->log_state) == LOG_ACTIVE ) {

        tnicon_p->amount_logged += fprintf(tnicon_p->log_p,
                                           "Connection[%d] \n\t", 
                                           conn);

        for( p = mbufchain; p != NULL; p = p->m_next ) {

            tnicon_p->amount_logged += fprintf(tnicon_p->log_p,
                                               "p->m_len: %d \n\t",
                                               p->m_len);

            tnicon_p->amount_logged += DumpMbuf(p, tnicon_p->log_p);

        }
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* int DumpMbuf(struct mbuf *p, FILE *file)                                   */
/*                                                                            */
/* Purpose: This function mbufs to a file                                     */
/*                                                                            */
/* Input Arguments: Mbuf, file to dump the contents of the the mbuf to        */
/*                                                                            */
/*                                                                            */
/* Output Arguments: File and the amount logged over last interval            */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int DumpMbuf(struct mbuf *p, FILE *file) {

    int char_cnt = 0;
    int amt_logged = 0;

    int byteCount;

    for( byteCount = 0; byteCount < p->m_len; byteCount++ ) {

        if( char_cnt < 22 ) {

            amt_logged += fprintf(file, "%2x ", 
                                  *((unsigned char *) (p->m_data)+byteCount));

            char_cnt++;

        } else {

            amt_logged += fprintf(file, "%2x\n\t", 
                                  *((unsigned char *) (p->m_data)+byteCount));

            char_cnt = 0;

        }
    } 

    if( char_cnt != 0 ) {

        amt_logged += fprintf(file, "\n");

    }

    return amt_logged;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void LogMsg(char *error_string)                                            */
/*                                                                            */
/* Purpose: This function messages to logfile                                 */
/*                                                                            */
/* Input Arguments: Message to be logged                                      */
/*                                                                            */
/*                                                                            */
/* Output Arguments: log file and the amount logged over last interval        */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void LogMsg(char *error_string) {

    char *current_time;

    if( (tnicon_p->log_state) == LOG_ACTIVE ) {

        current_time = Get_Time();

        tnicon_p->amount_logged += fprintf (tnicon_p->log_p, 
                                            "%s %s\n\n", 
                                            current_time, 
                                            error_string);
    }
}


/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void ChkExcessiveLogging(void)                                             */
/*                                                                            */
/* Purpose: This function messages checks if excessive logging is occurring   */
/*          And attempts to notify operator of situation via elog             */
/*                                                                            */
/* Input Arguments: void                                                      */
/*                                                                            */
/*                                                                            */
/* Output Arguments: The amount logged over last interval                     */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void ChkExcessiveLogging(void) {

    if( difftime(time(NULL), tnicon_p->excess_log_check_time) >= 60 ) {

        tnicon_p->excess_log_check_time = time(NULL);

        if( tnicon_p->amount_logged >= tnicon_p->log_threshold ) {

            sprintf(err_string.par1,
                    tnicon_p->log_file_name);

            output_err("Server_Main_Loop",
                       MI_TNI_EXCESS_LOG,
                       MX_ERR_LVL_WARNING,
                       err_string);
        }
        tnicon_p->amount_logged = 0;
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void DumpValue(const char *name, const void *buf, int buflen)              */
/*                                                                            */
/* Purpose: This function dumps the hex value to a file                       */
/*                                                                            */
/* Input Arguments: name, buf, buflen                                         */
/*                                                                            */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          number of bytes logged                                            */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int DumpValue(const char *name, const void *buf, int buflen, FILE *file)
{

    int char_cnt = 0;
    int amt_logged = 0;
    int byteCount;

    amt_logged += fprintf(file, "\t%s \n",
                                    name);
    for( byteCount = 0; byteCount < buflen; byteCount++ ) 
    {
        if( char_cnt < 22 ) 
        {

            amt_logged += fprintf(file, "%2x ", 
                                  *((unsigned char *) (buf)+byteCount));

            char_cnt++;

        } 
        else 
        {

            amt_logged += fprintf(file, "%2x\n\t", 
                                  *((unsigned char *) (buf)+byteCount));

            char_cnt = 0;

        }
    } 

    if( char_cnt != 0 ) 
    {
        amt_logged += fprintf(file, "\n");
    }

    return amt_logged;
}


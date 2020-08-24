/* */

/*
 * ===[mxsrv_common_proto.h]==============================================
 *
 * Description: Function prototypes that are common to all supported
 *              platforms.
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

int Add_Event_To_Free_Queue(RQST_EVENT *newEvent);

int Add_Event_To_Rpc_Rqst_Queue(RQST_EVENT *newEvent);

int Add_Local_Interface (char *domain_name);

void App_Detailed(void);

void App_Input(char *command, int par);

void App_Summary(void);

void Application(void);

int All_Clients_Connected(int *connection);

int Allocate_Application (void);

int Allocate_Rpc_Rqst_Event(int app_number,
                            int transaction_id,
                            int cdc,
                            int hours,
                            int minutes,
                            int seconds,
                            int product_number,
                            int *event_number);

int Application_Defined (char *appName);

void AppMsgDbg(int terminal_num,
               char *msg_desc,
               long int app_msg_len,
               unsigned char *app_msg_ptr);

#if defined(XOS_VMS)

    struct TIMER_VALUES Set_Timervalue_Vms (int tics);

#endif

int Assign_Connection (int client_id,
                       char *app_name,
                       unsigned short *conn);

int Assign_Conn_To_App (int connNum, int appNum);

int attach_core_sections(char tnicon_type, char tstats_type, char events_type);

void Attempt_Connection(int conn);

void Awaiting_Notification(int connection);

struct mbuf * __buf_alloc (int len, int priority, char *file, int line);

struct mbuf * __buf_allocmax (char *file, int line);

void __buf_free (struct mbuf *bp, char *file, int line);

void bufp_init (void);

void bufp_mem (void);

struct mbuf *Build_Client_Session_Resp_Pdu (unsigned short conn,
                                            struct TNI_PARAM_PAIR *params);

struct mbuf *Build_Error_Pdu (unsigned short conn,
                              struct TNI_PARAM_PAIR *params);

struct mbuf *Build_Client_Param_Response_Pdu (unsigned short conn,
                                              struct TNI_PARAM_PAIR *params);

void Build_Oltp_Bro_Msg (int conn_num,
                         long int term_server_id,
                         long int msg_len,
                         unsigned char *msg);

struct mbuf *Build_Oltp_Msg (int conn_num,
                             int delivery_mode,
                             long int term_server_id,
                             long int msg_len,
                             unsigned char *msg,
                             char *correlation_tag,
                             int *msg_mbuf_len);

void Build_Oltp_Resp_Msg (long int term_server_id,
                          long int msg_len,
                          unsigned char *msg);

byte_4 Build_Oltp_Rqst_Msg (byte_4 conn_num,
                            byte_4 terminal_server_id,
                            byte_4 term_stats_idx,
                            byte_4 msg_len,
                            ubyte_1 *msg);

void Build_Oltp_Unso_Msg (int conn_num,
                          long int term_server_id,
                          int term_stats_idx,
                          long int msg_len,
                          unsigned char *msg);

int Build_Parameter (unsigned char code, long int value,
                     char *string_val, struct TNI_DATA *buf);

struct mbuf *Build_Pdu (unsigned short conn,
                        unsigned char pdu_type,
                        struct mbuf *inmbuf,
                        struct TNI_PARAM_PAIR *params);

struct mbuf *Build_Rpc_Msg_Header (unsigned short conn,
                                   char *RpcRqstTag,
                                   long int msgLen,
                                   long int delMode,
                                   int enc_mode);

struct mbuf *Build_Rpc_Server_Data_Pdu(unsigned short conn,
                                       struct mbuf *inputChain,
                                       struct TNI_PARAM_PAIR *params);
 
struct mbuf *Build_Term_Data_Pdu (unsigned short conn,
                                  struct mbuf *inputchain,
                                  struct TNI_PARAM_PAIR *params);

struct mbuf *Build_Term_Pdu_Header (long int term_ndx, long int host_term,
                                    char *host_term_tag, long int msg_len,
                                    long int del_mode, int conn_num,
                                    char *correlation_tag);

struct mbuf *Build_Server_Alive_Pdu (unsigned short conn);

struct mbuf *Build_Server_Cmd_Request_Pdu (unsigned short conn,
                                           struct TNI_PARAM_PAIR *params);

struct mbuf *Build_Serv_Challenge_Rqst_Pdu(unsigned short conn, 
                                           struct mbuf *inmbuf, 
                                           struct TNI_PARAM_PAIR *params);

struct mbuf *Build_Serv_Challenge_Notify_Pdu(unsigned short conn, 
                                             struct TNI_PARAM_PAIR *params);

struct mbuf *Build_Server_Request_Pdu (unsigned short conn,
                                       struct TNI_PARAM_PAIR *params);

struct mbuf *Build_Srv_Enc_Key_Rqst_Pdu (unsigned short conn,
                                         struct mbuf *inputchain,
                                         struct TNI_PARAM_PAIR *params);

int Build_Vision_Cmd(int cmd_type,
                     int arg_int1,
                     int arg_int2,
                     char *arg_str1,
                     char *arg_str2,
                     COMMAND_INFO *command);

void Calc_Game_Delay (unsigned int term_num);

int Calc_Max_Param_Len (unsigned char param_code);

void Calc_Offsets (void);

int Calc_Pdu_Hdr_Size (unsigned char pdu_type);

int Calc_Rpc_Msg_Hdr_Size (void);

int Calc_Term_Msg_Hdr_Size (unsigned char term_id_method);

void Check_Blocking_Timers(void);

void Check_Comm_Mode(void);

void Check_Cycle_Rate(void);

void Check_Rpc_Request_Timeout(void);

void Check_Statistics_Timer(void);

void Check_Connection_Status(void);

unsigned char Check_Dual_Primary (void);

int Check_Dual_Primary_Fixed (int host_id_idx,
                              unsigned int *host_id_val);

int Check_Parameter (int conn,
                     struct TNI_DATA *param_struct,
                     unsigned char *code,
                     long int *length,
                     long int *value,
                     char *string_val);

void Check_Server_Takeover(void);

void Check_Keep_Alive(void);

void ChngLogFile(void);

void ChngLogThreshold(void);

void ChkExcessiveLogging(void);

void Clear_Authentication_Lockouts(void);

void client(void);

void Client_Alive (struct PDU_STRUCT *pdu_struct, long int pdulen,
                   unsigned short conn);

void Client_Cmd_Request (struct PDU_STRUCT *pdu_struct, long int pdulen,
                         unsigned short conn);

void Client_Id (void);

void Client_Id_Detailed (void);

void Client_Id_Input (char *command, int par);

void Client_Id_Summary (void);

void Client_Param_Request (int conn);

void Client_Request (struct PDU_STRUCT *pdu_struct, long int pdulen,
                   unsigned short *conn);

void Client_Session_Request (struct PDU_STRUCT *pdu_struct,
                             long int pdulen,
                             unsigned short conn);
 
void Close_All_Connections (void);

void Close_Connection (unsigned short conn);    

void Close_Dbg_file(void);

void Close_Listen_Sockets (void);

void Close_Log_file(void);

void Clt_Encryption_Key_Notify (struct PDU_STRUCT *pdu_struct,
                                long int pdulen,
                                unsigned short conn);

void Conn(void);

void Conn_Detailed(void);

int Conn_Name_Valid (char *domain_name);

void Conn_Summary(void);

void Conn_Input(char *command, int par);

void Deallocate_Application (int appIdx);

int Deallocate_Rpc_Rqst_Event(RQST_EVENT *event);

int Deassigned_Conn_From_App (int conn_num,
                              int app_num);
void Debug(void);

void Debug_Input(char *command, int par);

void Debug_Terminal(void);

void Debug_Terminal_Input(char *command, int par);

int Decrypt_Client_Key (ubyte_2 conn, byte_4 enc_key_length, ubyte_1 *enc_key_ptr);

struct mbuf *Decrypt_Data(ubyte_2 conn, byte_4 del_mode, byte_4 enc_mode,
                          byte_4 data_len, ubyte_1 *data, byte_4 *rstat);

unsigned char Determine_Len (unsigned long int value);

int disable_int(void);

void Display_Cmd_Result(int code);

int Dual_Primary_Exists (unsigned short conn);

struct mbuf *Duplicate_Chain (struct mbuf *inputchain, unsigned short conn);

int DumpMbuf(struct mbuf *p, FILE *file);

void DumpMbufChain(struct mbuf *mbufchain, unsigned short conn);

int DumpPdu(struct PDU_STRUCT *pdu_struct, long int pdulen, FILE *file);

int DumpValue(const char *name, const void *buf, int buflen, FILE *file);

struct mbuf *Encrypt_Data(ubyte_2 conn, byte_4 del_mode, byte_4 enc_mode,
                          byte_4 *data_len, ubyte_1 *data, byte_4 *rstat);

void Error_Pdu (struct PDU_STRUCT *pdu_struct, long int pdulen,
                unsigned short conn);

void Event_Queues(void);

void Event_Queues_Input(char *command, int par);

int Load_Exchange_Enc_Key(void);

void FATAL( int value);

int Find_Alternate_Connection (int conn);

int Find_Application_Number (char *app_name);

unsigned char Find_Client_Id(int connType, char *appName);

int Find_Client_Idx (int client_id_val);

int Find_Conn_For_App (int messaging_type, int enc_mode, int app_num, char *app_name);

int Find_Conn_For_Oltp_Unso (int *conn_num);

int Find_Exclusive_Idx(int conn_num);

int Find_Rpc_Rqst_Event(int event_number,
                        int app_number,
                        int transaction_id,
                        int cdc,
                        int hours,
                        int minutes,
                        int seconds,
                        RQST_EVENT **event);

int Find_Unused_Connection (void);

int Generate_Vision_Cmd(int cmd_type,
                        int arg_int1,
                        int arg_int2,
                        char *arg_str1,
                        char *arg_str2);

void Generate_Salt_Data(unsigned char *buf, 
                        unsigned int len);

int Generate_Msg_Digest(unsigned int hash_alg_code, 
                        unsigned char *msg, 
                        unsigned int len, 
                        unsigned char *md_value, 
                        unsigned int *md_len);

unsigned short Get_Connection (int host_id_idx);

char* getErrorMsg(int errnum);

int Get_Free_Event(RQST_EVENT **newEvent);

void Get_New_Connections (void);

char Get_Next_State (unsigned short conn, int tni_server_state,
                     unsigned char last_host_state);

int Get_Rqst_Event(int eventNumber, RQST_EVENT **requestedEvent);

char *Get_Time(void);

void Initialize_Connection(int conn);

void Initialize_Event_Queue(int queueNumber, EVENT_QUE_HDR *queueHdr);

int Initialize_Request_Event_List(RQST_EVENT *event);

void Initialize_Statistics(void);

void init_event_log (void);

struct mbuf *Init_Write_Chain (void);

int Insert_Msg_In_Server_Data_Pdu (int conn_num,
                                   int msgs_len,
                                   int delivery_mode,
                                   int enc_msg_flag,
                                   struct mbuf *msg_mbuf);

void Inv_Pdu_Hdr_Err (struct mbuf *mbuf_chain, unsigned short conn);

void Inv_Pdu_Len_Err (struct mbuf *mbuf_chain, unsigned short conn);

void Listen_Socket_Init(void);

void Load_Param (long int value, unsigned char len, union PARAM_DATA *buf);

void log_authentication_attempts(int conn, int result_code);

void log_rpc_transaction (int log_code,
                          char *rpc_tag,
                          int transaction_length,
                          char *transaction);

void Logging(void);

void Logging_Input(char *command, int par);

void LogMsg(char *error_string);

void LogPdu(struct PDU_STRUCT *pdu_struct,  
            short term_net_id, unsigned short conn);

void Menu(void);

int Mxsrv_Global(void);

void Open_Dbg_file(void);

void Open_Log_file(void);

int Open_Mx_Config_File(void);

int Open_Tni_Config_File (void);

void output_err(char *func_name,
                int error_code,
                int error_level,
                struct ERR_STRING err_string);

int Parse_Close_Conn_Cmd(COMMAND_INFO *request);

int Parse_Comm_Mode_Cmd(COMMAND_INFO *request);

int Parse_Debug_Level_Cmd(COMMAND_INFO *request);

int Parse_Log_Mode_Cmd(COMMAND_INFO *request);

int Parse_Log_Path_Cmd(COMMAND_INFO *request);

int Parse_Log_Threshold_Cmd(COMMAND_INFO *request);

int Parse_Overide_Prim_Cmd(COMMAND_INFO *request);

int Parse_Rpc_Request_Tag(char *rpcRequestTag,
                          int *rqstEventNumber,
                          int *productNumber,
                          int *transactionId,
                          int *cdc,
                          int *hours,
                          int *minutes,
                          int *seconds);

int Parse_Vision_Cmd(COMMAND_INFO *request);

void Part_Client_Ids (unsigned long int head,
                 unsigned long int tail,
                 unsigned long int *lowhead,
                 unsigned long int *lowtail,
                 unsigned long int *highhead,
                 unsigned long int *hightail);

int Primary_Conn_Exists (int host_id_idx);

void Prime_Conn(void);

void Prime_Conn_Input(char *command, int par);

void Print_Pdu (char * pdu_string, struct PDU_STRUCT *pdu_struct,
                long int pdulen);

int Process_App_Section(int *last_conn_assigned);

int Process_Buffer_Section(void);

int Process_Conn_Section_Tni (int num_conns,
                              int first_available_conn_num);

int Process_Command(COMMAND_INFO *request);

int Process_Control_Section(void);

int Process_Control_Section_Tni (int mx_config_valid,
                                 int *num_conns);

int Process_Pdu( unsigned short length, struct PDU_STRUCT *pdu_struct,
                 unsigned short *conn);


void Process_Term_Msg_Block (struct PDU_STRUCT *pdu_struct, int msg_offset, 
                             long int block_len, long int num_msgs, 
                             unsigned short conn);

int Process_Tni_Section(void);

void Process_Rpc_Msg_Block(struct PDU_STRUCT *pduStruct,
                           int msgOffset, long int blockLen,
                           long int numMsgs,
                           unsigned short conn);

int Process_Tni_Section_Tni (int mx_config_valid);

int Protocol_Valid (unsigned char *pdu_header,
                    unsigned short conn);

void Q_Sort_Client_Ids (unsigned long int head,
                   unsigned long int tail);

int Read_From_Socket (unsigned short connection, struct mbuf **sockbuf,
                      int length);

void Read_Client_Data (void);

struct mbuf *Read_From_Connection (unsigned short conn);

int Remove_Event_From_Rpc_Rqst_Q(RQST_EVENT *removeEvent);

int restore_int(void);

void Rpc_Client_Data(struct PDU_STRUCT *pduStruct,
                     long int pduLen,
                     unsigned short conn);

void Rpc_Messaging(void);

void Rpc_Messaging_Input(char *command, int par);

static void Rpc_Msgs_Snapshot(void);

static void Rpc_Stats_Snapshot(void);

void Send_App_Bro (long int term_msg_len, char *app_msg_ptr);

void Send_App_Rqst_Resp (int conn_num,
                         long int term_net_id,
                         long int term_host_id,
                         char *host_term_tag,
                         char *correlation_tag,
                         long int term_msg_len,
                         struct PDU_STRUCT *pdu_struct,
                         long int length,
                         long int offset,
                         int msg_offset,
                         int msg_type);

void Send_App_Unso (unsigned long int term_net_id,
                    long int term_host_id, long int term_msg_len,
                    char *app_msg_ptr);

void Send_Authentication_Requests(void);

int send_invalid_pdu_error (int conn_num);

int send_msg_undelivered_error (int conn_num,
                                long int terminal_server_id,
                                long int terminal_client_id,
                                char *terminal_client_tag,
                                char *correlation_tag);

int send_not_authenticated_error (int conn_num);

int send_rpc_rqst_error_pdu (int  conn_num,
                         char *rpc_rqst_tag,
                         int  result_code);

void Send_Oltp_Bro_Msg (long int term_server_id,
                        long int msg_len,
                        unsigned char *msg);

void Send_Oltp_Resp_Msg (long int term_server_id,
                         long int msg_len,
                         unsigned char *msg);

byte_4 Send_Oltp_Rqst_Msg (byte_4 terminal_server_id,
                           byte_1 *app_name,
                           byte_4 msg_len,
                           ubyte_1 *msg,
                           byte_4 *result_code,
                           byte_1 *result_text);

void Send_Oltp_Unso_Msg (long int term_server_id,
                         long int msg_len,
                         unsigned char *msg);

void Send_Px2x_Enable_Req(void);

void Send_Reply(void *mesp, short meslen);

void Send_Rpc_Response_To_Game(char *rpc_request_tag,
                               int reply_code,
                               char *reply_text,
                               long int mes_len,
                               char *msg_data,
                               int calc_resp_time);

int Send_Rpc_To_Client (int addr_num,
                        char *rpc_rqst_tag,
                        long int del_mode,
                        int enc_mode,
                        long int msg_len,
                        unsigned char *msg,
                        int *result_code,
                        char *result_text);

void Send_Srv_Enc_Key_Rqst (unsigned short conn);

int send_term_unknown_error (int conn_num,
                             long int terminal_server_id,
                             long int terminal_client_id,
                             char *terminal_client_tag,
                             char *correlation_tag);

void Send_To_Client (long int term_ndx, long int msg_len, long int del_mode,
                     unsigned char *msg);

int Send_To_Conn (struct mbuf *writechain, int conn, int retry);

int Send_Vision_Cmd(COMMAND_INFO *request);

void Server(int sig);

void Server_Cmd_Response (struct PDU_STRUCT *pdu_struct, long int pdulen,
                          unsigned short conn);

void Server_Challenge_Request(int conn);

void Server_Challenge_Response(struct PDU_STRUCT *pdu_struct, long int pdulen, 
                                unsigned short conn);

void Server_Main_Loop(void);

void Server_Response (struct PDU_STRUCT *pdu_struct, long int pdulen,
                      unsigned short conn);

void Serv_Summary(void);

void Serv_Summary_Input(char *command, int par);

void Set_Next_Screen_Update(void);

struct timeb Set_Timervalue (int tics);

void Show_Current_Time (char *caller);

void Start_Game_Delay_Timer (unsigned int term_num);

void Srv_Encryption_Key_Resp (struct PDU_STRUCT *pdu_struct,
                              long int pdulen,
                              unsigned short conn);

char *strupper (char *s);

void subtimes(struct timeb *intime1,struct timeb *intime2,struct timeb *outtime);

void switch_event_log (void);

void Task_Cmd_Reply(int result_code);

void Term_Client_Data (struct PDU_STRUCT *pdu_struct, long int pdulen,
                       unsigned short conn);

void Term_Serv(void);

void Term_Serv_Input(char *command, int par);

void tm_wkafter( int value); 

int tni_cfgint(char *, char*, int, char);

int tni_cfgread(FILE *,char *,int);

char *tni_cfgsect(char *,char *);

char *tni_cfgstr (char *,char *);

int Tni_Cfg_Conn(CONNECTION_INFO *conndata);

int Tni_Cfg_Cont(void);

int Tni_Cfg_Tni(void);

int Tni_Conns_Valid (int tot_conns);

void tni_erase_con_rec(int conn_to_erase,int num_conns);

int Tni_Init(void);

int Tni_Setup(void);

void UFATAL( int value);

void Update_Min_Max_Resp_Times(int responseTime, int *minimumRespTime, int *maximumRespTime);

void UpdateLogFile(char *file);

int Validate_Client_Id (int client_id_val);

void Validate_Connection (int new_sock,
                          struct sockaddr_in new_sock_addr_ini,
                          char *local_device_name);

int Validate_Tni_Config_Conns (int first_tni_config_conn);

int Valid_Term_Input (long int term_server_id,
                      int del_mode,
                      int conn_num,
                      long int *term_client_id,
                      char *term_client_tag);

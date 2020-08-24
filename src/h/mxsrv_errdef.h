/* */

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
/*====[MXSRV_ERRDEF.H]========================================================*/
/*                                                                            */
/* Purpose: This header file contains MX error - Definitions                  */
/*                                                                            */
/*====[MXSRV_ERRDEF.H]========================================================*/
/*                                                                            */

#ifndef ERRDEF

#   define ERRDEF

#   if defined(PROSYS_ENV_ALL)

#       define MX_ERR_LVL_INFO    GEL_INFO
#       define MX_ERR_LVL_WARNING GEL_WARNING
#       define MX_ERR_LVL_ERROR   GEL_ERROR
#       define MX_ERR_LVL_FATAL   GEL_FATAL

#   else

#       define MX_ERR_LVL_INFO    1
#       define MX_ERR_LVL_WARNING 2
#       define MX_ERR_LVL_ERROR   3
#       define MX_ERR_LVL_FATAL   4

#   endif

#   define ME_BAD_TER_RNG         0    /* "Terminal number is out of range"      */
#   define MI_UNEXPGTMSREQ        1    /* "Unrecognized request from GTMS"       */
#   define ME_FILE_ERROR          2    /* "Error %s file %s"                     */
#   define MI_TNI_NO_DELIVER      3    /* "Unable to deliver %s message to conn. */
#   define MI_TNI_INV_CONN        4    /* "Connection %s is out of range [%s,%s] */
#   define MI_TNI_INVREQUEST      5    /* "Invalid request %s from prod %s"      */
#   define ME_TNI_ERR_CFG_FIL     6    /* "Unable to start TNI server, error ... */
#   define MI_TNI_INV_FORMAT      7    /* "Invalid configuration format of fi... */
#   define MI_TNI_NO_SECTION      8    /* "Section %s not found"                 */
#   define MI_TNI_UNKNOWN         9    /* "Unknown %s %s"                        */
#   define MI_TNI_INV_MSG_SIZE   10    /* "Terminal %s has invalid message le... */
#   define MI_TNI_MB_ALLOC       11    /* "Error allocating %s mbuf of length... */
#   define MI_TNI_MB_GATHER      12    /* "Error gathering %s in %s"             */
#   define MI_TNI_CONN_ATTEMPT   13    /* "TNI %s connection from host %s wit... */
#   define MI_TNI_SOCK_ERR       14    /* "%s  - Socket error: %s  (Generic s... */
#   define MI_TNI_CONN_CLOSE     15    /* "Closing connection %d to %s on hos... */
#   define MI_TNI_INV_EPDU       16    /* "Invalid PDU error received on conn... */
#   define MI_TNI_UNKN_EPDU      17    /* "Terminal number %s is unknown to H... */
#   define MI_TNI_UND_EPDU       18    /* "Unable to deliver message to termi... */
#   define MI_TNI_DUAL_EPDU      19    /* "Host Id %s detects dual primaries ... */
#   define MI_TNI_TIMEOUT        20    /* "Connection time out"                  */
#   define MI_TNI_ERASE_CONN     21    /* "Removing %s %s from %s                */
#   define MI_TNI_DEF_VALUE      22    /* "Assigning default value of %s to k... */
#   define MI_TNI_SOCK_TIMEOUT   23    /* "Socket %s timeout on connection %s"   */
#   define MI_TNI_PEER_CLOSE     24    /* "Connection %s closed by peer"         */
#   define MI_TNI_INV_PDU_TYPE   25    /* "Invalid PDU type %s received on co... */
#   define MI_TNI_STRING         26    /* your own string                        */
#   define MI_TNI_INV_STATE      27    /* "Invalid state change attempt"         */
#   define MI_TNI_ERR_SEND       28    /* "Error sending %s pdu on connection %s"*/
#   define MI_TNI_DROP_DATA      29    /* "Data dropped while reading socket %s..*/
#   define MI_TNI_IN_GAME        30    /* "Transaction is game for temrinal %s   */
#   define MI_TNI_NO_PROCOM      31    /* "Unable to allocate PROCOM buffer for. */
#   define MI_TNI_ERR_UNSO       32    /* "Unable to send unsolicited msg for... */
#   define MI_TNI_ERR_BRO        33    /* "Unable to send broadcast msg to %s... */
#   define MI_TNI_NOT_CONFIGED   34    /* "Unable to reassign %s to a configured */
#   define MI_TNI_NOKEYWORD      35    /* "Keyword %s is missing"                */
#   define MI_TNI_CMD_CHNG       36    /* "%s changed from %s to %s"             */
#   define MI_TNI_CMD_CONN       37    /* "%s command sent in connection %s"     */
#   define MI_TNI_CMD_NACK       38    /* "connection %s unable to preform %s c..*/
#   define MI_TNI_AWAITING_CONN  39    /* "Unable to notify Clients of PRIMARY ..*/
#   define MI_TNI_CMD_INV_GRP    40    /* "Command received with invalid %s"     */
#   define MI_TNI_CMD_INV_TYP    41    /* "Command received from group %s with ..*/
#   define MI_TNI_CMD_INV_ARG    42    /* "Command received from group %s, type .*/
#   define MI_TNI_ZERO_CHAIN     43    /* "Zero length data chain in %s"         */
#   define MI_TNI_COMM_ENABLE    44    /* "PX2X/X2X communications enabled"      */
#   define MI_TNI_COMM_WAIT      45    /* "Waiting for %s to enable comm..."     */
#   define MI_TNI_EXCESS_LOG     46    /* "WARNING: Excessive logging to %s"     */
#   define MI_TNI_NOCONFIG_CONN  47    /* "No configurable connection available. */
#   define MI_TNI_NODEF_CONNS    48    /* "No defined connections found for ...  */
#   define MI_TNI_REMOVE_APP     49    /* "CLUSTERED_APP section %s as been ...  */
#   define MI_TNI_NOALLO_APP     50    /* "Unable to allocate application stru.  */
#   define MI_TNI_BAD_CONN_RNG   51    /* "Unable to assign Conn %s to App %s..  */
#   define MI_TNI_BAD_APP_RNG    52    /* "Unable to assign Conn %s to App %s..  */
#   define MI_TNI_APP_NOTDEF     53    /* "Unable to assign Conn %s to App %s..  */ 
#   define MI_TNI_CONN_ASSGND    54    /* "Unable to assign Conn %s to App %s..  */
#   define MI_TNI_MAX_ASSGND     55    /* "Unable to assign Conn %s to App %s..  */ 
#   define MI_TNI_APP_DEFINED    56    /* "Application already defined with ...  */
#   define MI_TNI_MAX_RPC_RQSTS  57    /* "Maximum outstanding RPC request  ...  */
#   define MI_TNI_NOSEND_APP     58    /* "Unable to send RPC request, app  ...  */
#   define MI_TNI_NOSAVE_TRANS   59    /* "Unable to save %s transaction"        */
#   define MI_TNI_NORESTR_TRANS  60    /* "Unable to restore %s transaction ...  */
#   define MI_TNI_NOSEND_SUPP    61    /* "Delivery of %s message not supported  */
#   define MI_TNI_NORECEIPT_SUP  62    /* "Receipt of %s messages not supported" */
#   define MI_TNI_NOCONN_AVL     63    /* "No connection available to send RPC   */
#   define MI_TNI_MAX_PDU_EXCD   64    /* "%s of %s bytes exceeds max PDU size   */
#   define MI_TNI_LATE_RESP      65    /* "Received late %s with Id %s, message  */
#   define MI_TNI_NOOUT_RQST     66    /* "No outstanding request for %s with .. */
#   define MI_TNI_MSG_DISCARD    67    /* "%s message discarded"                 */
#   define MI_TNI_NORPC_RQSTAG   68    /* "No RPC request tag found in %s ...... */
#   define MI_TNI_BAD_RQSTAG     69    /* "Unable to parse %s in RPC request ... */ 
#   define MI_TNI_MAX_QLEN       70    /* "%s queue exceeded maximum length .... */
#   define MI_TNI_NULL_ELEMENT   71    /* "Attempting to put %s back on %s ..... */
#   define MI_TNI_Q_CORRUPT      72    /* "%s queue is corrupted %s"             */
#   define MI_TNI_Q_EMPTY        73    /* "%s queue is empty"                    */
#   define MI_TNI_NOT_IN_Q       74    /* "Event %s not located in %s queue"     */
#   define MI_TNI_BAD_EVNT_NUM   75    /* "Event index %s is out of the request. */
#   define MI_TNI_OUTSTAND_TRANS 76    /* "Additional transaction for product .. */
#   define MI_TNI_SOCK_TIMER     77    /* "Unable to initiate sock %s timer"     */
#   define MI_TNI_NOSEND_SOCK    78    /* "Unable to send, message(s) dropped"   */
#   define MI_TNI_NOPROC_MSG     79    /* "Unable to process %s message from ... */
#   define ME_TNI_NO_START       80    /* "Unable to start MX server"            */
#   define MI_TNI_COM_SYNC_OVR   81    /* "Synchronization to communications ... */
#   define MI_TNI_NO_OVERRIDE    82    /* "Server in PRIMARY state, unable to .. */
#   define ME_TNI_NO_WINDOW      83    /* "Unable to create window! Check ...... */
#   define MI_TNI_NO_TIME        84    /* "Unable to get time from system"       */
#   define MI_TNI_COMM_SHUTDWN   85    /* "Forced communications shutdown ...... */
#   define MI_TNI_INIT_SIG       86    /* "Sigaction call failed on signal %s"   */
#   define MI_TNI_TERM_SIGNAL    87    /* "MX Server received terminate signal"  */
#   define MI_TNI_CONN_ESTABLD   88    /* "Client connection already established"*/
#   define MI_TNI_INV_PROTO_ID   89    /* "Invalid protocol id on conn %s, ..... */
#   define MI_TNI_INV_PROTO_VER  90    /* "Invalid protocol version on conn %s . */
#   define MI_TNI_INV_PDU_LEN    91    /* "Invalid PDU len received on ......... */
#   define MI_TNI_NO_TRANSLATE   92    /* "Unable to translate %s into a domain  */
#   define MI_TNI_BAD_SOCK_RNG   93    /* "Socket value %s out of range [%s - %s]*/
#   define MI_TNI_SCREEN_TIMER   94    /* "Unable to initiate screen update timer*/
#   define MI_TNI_SIG_ALRM       95    /* "Alarm signal fuction failed"          */
#   define MI_TNI_ADD_INTF_FAIL  96    /* "Unable to add local interface: %s     */
#   define MI_TNI_NOREAD_TNICFG  97    /* "Unable to read %s file, no connections*/
#   define MI_TNI_NO_CONNS       98    /* "No valid connections found in the ... */
#   define MI_TNI_MAX_CLIENTIDS  99    /* "Maximum configurable Client Ids (%s). */
#   define MI_TNI_MAX_CONNS_PER  100   /* "Maximum connections per Host Id (%... */
#   define MI_TNI_DUP_CONNS      101   /* "Connection %s and connection %s ar... */
#   define MI_TNI_SAME_REMOTE    102   /* "Connection %s and connection %s, a... */
#   define MI_TNI_MAX_SYS        103   /* "Maximum connections to remote syst... */
#   define MI_TNI_ALT_CONN       104   /* "PDU sent over alternate connection %s */
#   define MI_TNI_TERM_HASH_FAIL 105   /* "Terminal hash failure (Id %s): %s"    */
#   define MI_TNI_NO_TERM_ID     106   /* "Unable to send %s msg to conn %s, ... */
#   define MI_TNI_BADPARAMETER   107   /* "arameter %s (%s) is out of range (%s..*/
#   define MI_TNI_BADKEYWORD     108   /* "Keyword %s has bad value %s"          */
#   define MI_TNI_MSGTOOBIG      109   /* "Rpc Message Too Big                   */
#   define MI_TNI_CLIENT_REJECT  110   /* "%s rejected by client on conn %s      */ 
#   define MI_TNI_NOT_AUTH_EPDU  111   /* "Not Authenticated PDU erroir recei..  */
#   define MI_TNI_ERR_PSWD_RTRV  112   /* "Password Retrieve failed for app .... */
#   define MI_TNI_ERR_HASH_GEN   113   /* "Hash generation failed for app %s ... */
#   define MI_TNI_INV_HASH_LEN   114   /* "Invalid hash length %s received on... */
#   define MI_TNI_MAX_AUTH_ATMPTS 115  /* "Maximum number of authentication ...  */
#   define MI_TNI_AUTH_DISABLED  116   /* "Authentication for app %s is being... */
#   define MI_TNI_WARN_AUTH_HACK 117   /* "An attempt to systematically determ...*/
#   define MI_TNI_ENC_KEY_ERR    118   /* "Unable to process encryption key .....*/
#   define MI_TNI_KEY_X_DISABLE  119   /* "Key exchange disabled: %s %s          */

#   if defined(GOLS_ENV_ALL)

#       define MI_INVALID            120   /* "Invalid value of %s = %s ",name,value */
#       define MI_DELETE             121   /* "Error deleting %s %s", object, name */
#       define MI_CREATE             122   /* "Error creating %s %s", object, name */
#       define MI_ATTACH             123   /* "Error attaching to %s %s", object, .. */
#       define MI_ACCESS             124   /* "Error accessing %s %s", object, name  */
#       define MI_PAGESIZE           125   /* "Error obtaining page size" */

#   endif

    struct TNI_ERROR {
        char err_fmt[128];
        int  err_par;
    };

#   if defined(PROSYS_ENV_ALL)

        static struct TNI_ERROR TNI_ERROR_CODES[MI_TNI_KEY_X_DISABLE +1]={

#   endif

#   if defined(GOLS_ENV_ALL)

        static struct TNI_ERROR TNI_ERROR_CODES[MI_PAGESIZE +1]={

#   endif

          {"Terminal Server Id %s is out of range [%s-%s]",3},
          {"Unrecognized request (%s) from GTMS",1},
          {"Error %s file %s",2},
          {"Unable to send %s msg to Term Srv Id %s, Conn %s, %s",4},
          {"Connection %s is out of range [%s - %s] for Terminal Server Id %s",4}, 
          {"Invalid request %s from prod %s",2}, 
          {"Unable to start MX server, error in config file",0}, 
          {"Invalid configuration file format, file %s",1},
          {"Section %s not found",1}, 
          {"Unknown %s %s",2},
          {"%s message for Terminal %s has invalid length %s",3},
          {"Error allocating %s mbuf of length %s from %s",3},
          {"Error gathering %s in %s",2}, 
          {"MX %s conn from %s with IP addr %s, port %s, socket %s",5},
          {"%s  - Socket error: %s",2}, 
          {"Closing connection %s '%s' from %s",3},
          {"Invalid PDU error received on connection %s from %s",2}, 
          {"Terminal Server Id %s is unknown to %s",2},
          {"%s is unable to deliver message to Terminal Server Id %s",2}, 
          {"%s, connection %s detects dual primary servers",2},
          {"Connection timed out",0},
          {"Removing %s %s from %s",3},
          {"Assigning default value of %s to keyword %s",2},
          {"Socket %s time-out on connection %s",2},
          {"Connection %s closed by peer",1}, 
          {"Invalid PDU type %s received on connection %s",2},
          {"%s",1},
          {"Invalid state change attempt",0},
          {"Error sending %s PDU on connection %s",2},
          {"Data dropped while reading socket %s, connection %s",2},
          {"Transaction in game (%s sec) for Terminal Server Id %s [%s]",3},
          {"Unable to allocate PROCOM buffer for Terminal Server Id %s",1},
          {"Unable to send unsolicited msg for Term Srv Id %s to %s, no connections available",2},
          {"Unable to send broadcast msg to %s, no connections available",1},
          {"Unable to reassign %s to a configured client connection for %s",2},
          {"Keyword %s is missing",1},
          {"%s changed from %s to %s",3},
          {"%s command sent to connection %s",2},
          {"Connection %s unable to perform %s command",2},
          {"Unable to notify Clients of PRIMARY status until %s connects",1},
          {"Command received with invalid group %s",1},
          {"Command received from group %s with invalid type %s",2},
          {"Command received from group %s, type %s with invalid argument %s [%s]",4},
          {"Zero length data chain in %s",1},
          {"PX2X/X2X communications enabled",0},
          {"Waiting for %s to enable communications",1},
          {"WARNING: Excessive logging to %s",1},
          {"No configurable connection available for %s, connection %s",2},
          {"No defined connections found for application %s",1},
          {"CLUSTERED_APP section %s has been removed from the configuration",1},
          {"Unable to allocate application, data structure full",0},
          {"Unable to assign Conn %s to App %s. Conn out of range [%s-%s]",4},
          {"Unable to assign Conn %s to App %s. App out of range [%s-%s]",4},
          {"Unable to assign Conn %s to App %s. App not in DEFINED state",2},
          {"Unable to assign Conn %s to App %s. Conn already assign to App %s",3},
          {"Unable to assign Conn %s to App %s. Max connections assigned",2},
          {"Application already defined with name %s",1},
          {"Maximum outstanding RPC requests exceeded",0},
          {"Unable to send RPC request, application %s is undefined",1},
          {"Unable to save %s transaction",1},
          {"Unable to restore %s transaction with Id %s",2},
          {"Delivery of %s message not supported",1},
          {"Receipt of %s messages not supported",1},
          {"No connection available to send RPC for application %s",1},
          {"%s of %s bytes exceeds max PDU size of %s bytes",3},
          {"Received late %s with Id %s, message discarded",2},
          {"No outstanding request for %s with Id %s, message discarded",2},
          {"%s message discarded",1},
          {"No RPC request tag found in %s message",1},
          {"Unable to parse %s in RPC request tag %s",2},
          {"%s queue exceeded maximum length [%s]",2},
          {"Attempting to put %s back on %s queue",2},
          {"%s queue is corrupted %s",2},
          {"%s queue is empty",1},
          {"%s %s not located in %s queue",3},
          {"Event index %s is out of the request event range [%s-%s]",3},
          {"Additional trans from Term %s for Prod %s dropped due to outstanding request (%s sec)",3},
          {"Unable to initiate sock %s timer",1},
          {"Unable to send to %s, message(s) dropped",1},
          {"Unable to process %s message from game",1},
          {"Unable to start MX server",0},
          {"Synchronization with communications subsystems overridden",0},
          {"Server in PRIMARY state, unable to Override Primary Notify",0},
          {"Unable to create window! Check terminal type",0},
          {"Unable to get time from system",0},
          {"Forced communications shutdown (STOPSYS or DAYEND)",0},
          {"Sigaction call failed on signal %s",1},
          {"MX Server received terminate signal",0},
          {"Client connection already established",0},
          {"Invalid protocol id on conn %s, expected %s, received %s",3},
          {"Invalid protocol version on conn %s, expected %s, received %s",3},
          {"Invalid PDU len received on connection %s",1},
          {"Unable to translate %s into a domain name",1},
          {"Socket value %s out of range [%s-%s]",3},
          {"Unable to initiate screen update timer",0},
          {"Alarm signal function failed",0},
          {"Unable to add local interface: %s",1},
          {"Unable to read %s , no connections available",1},
          {"No valid connections found in the %s",1},
          {"Maximum configurable Client Ids (%s) exceeded",1},
          {"Maximum connections per Client Id (%s) exceeded",1}, 
          {"Connection %s and connection %s are duplicates",2},
          {"Connection %s and connection %s, assigned the same remote domain name %s",3}, 
          {"Maximum connections to remote system %s (%s) exceeded",2},
          {"PDU sent over alternate connection %s",1},
          {"Terminal hash failure (Id %s): %s",2},
          {"Unable to send %s msg to Conn %s, %s not provided",3},
          {"Parameter %s (%s) is out of range (%s to %s)",4},
          {"Keyword %s has bad value %s",2},
          {"Rpc message length %s is too big ",1},
          {"%s rejected by client on conn %s",2},
          {"Not authenticated PDU error received on connection %s from %s",2} ,
          {"Password retrieve failed for app %s for connection %s",2},
          {"Hash generation failed for app %s for connection %s",2},
          {"Invalid hash length %s received on connection %s",2},
          {"Maximum number of authentication attempts exceeded for app %s from %s",2},
          {"Authentication for app %s is being disabled to the next %s minutes",2},
          {"An attempt to systematically determine the application password may be in progress!!!",0},
          {"Unable to process encryption key for conn %s: %s",2},

#       if defined(PROSYS_ENV_ALL)
     
          {"Key exchange disabled: %s %s",2}

#       endif

#       if defined(GOLS_ENV_ALL)

          {"Key exchange disabled: %s %s",2},
          {"Invalid value of %s = %s ",2},
          {"Error deleting %s %s",2},
          {"Error creating %s %s",2},
          {"Error accessing %s %s",2},
          {"Error attaching to %s %s",2},
          {"Error obtaining page size",0}

#       endif

        };

    struct ERR_STRING{
        char par1[64];
        char par2[64];
        char par3[64];
        char par4[64];
        char par5[64];
    };
  
    extern struct ERR_STRING err_string;
    extern  struct ERR_STRING null_err_string;
 
#endif 

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
/*====[TNI_BUILD_PARAMETER.C]=================================================*/
/*                                                                            */
/* Purpose: These function build and verify a single TNI parameter block.     */
/*                                                                            */
/* Functions:                                                                 */
/*          Build_Parameter (unsigned char code,                              */
/*                           long int value,                                  */
/*                           char *string_val                                 */
/*                           struct TNI_DATA *buf)                            */
/*                                                                            */
/*          Check_Parameter (struct TNI_DATA *param_struct,                   */
/*                           unsigned char *code,                             */
/*                           long int *length,                                */
/*                           long int *value,                                 */
/*                           char *string_val)                                */
/*                                                                            */
/*          Load_Param (long int value,                                       */
/*                      unsigned char len,                                    */
/*                      union PARAM_DATA *buf)                                */
/*                                                                            */
/*====[TNI_BUILD_PARAMETER.C]=================================================*/
/*                                                                            */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "includes.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include <netinet/in.h>

#elif defined(XOS_VMS)

#   include <in.h>

#else

#   error - OS-specific logic not handled.

#endif

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Build_Parameter (unsigned char code,                                       */
/*                  long int value,                                           */
/*                  char *string_val                                          */
/*                  struct TNI_DATA *buf)                                     */
/*                                                                            */
/* Purpose: This function builds a single TNI parameter block and returns     */
/*          the ENTIRE length of the block.  A returned value of 0 indicates  */
/*          an error.  A single TNI parameter block contains the parameter    */
/*          code/length/value for a single TNI parameter.                     */
/*                                                                            */
/* Input Arguments:                                                           */
/*          code             TNI parameter code                               */
/*                                                                            */
/*          value            TNI parameter integer value                      */
/*                                                                            */
/*          string_val       TNI parameter string value                       */
/*                                                                            */
/* Output Arguments:                                                          */
/*          buf              Pointer to parameter block                       */
/*                                                                            */
/* Return Value:                                                              */
/*          int              # Length of parameter block in bytes             */
/*                           0 Error                                          */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Build_Parameter (unsigned char code, long int value,
                     char *string_val, struct TNI_DATA *buf) {
    int loc_idx;

    err_string = null_err_string;

    switch( code ) {
    case TERM_SERVER_ID:                                   /* ALWAYS 4 bytes  */
    case TERM_CLIENT_ID:

        buf->param_code = code;
        buf->param_length = sizeof(int);

        Load_Param (value, buf->param_length, &buf->param_data);

        return(buf->param_length +
               sizeof(buf->param_length) +
               sizeof(buf->param_code));
        break;

    case CLIENT_ID:                                        /* ALWAYS 1 byte   */
    case DELIVERY_MODE:
    case PRIMARY_STATUS:
    case RESULT_CODE:
    case TERM_METHOD:
    case MESSAGING_TYPE:
    case HASH_ALGORITHM:
    case ENC_KEY_X_METHOD:
    case ENC_KEY_TYPE:
    case ENC_MODE:

        buf->param_code = code;
        buf->param_length = sizeof(unsigned char);

        Load_Param (value, buf->param_length, &buf->param_data);

        return(buf->param_length +
               sizeof(buf->param_length) +
               sizeof(buf->param_code));
        break;

    case MAX_MESSAGE_SIZE:                               /* variable length */
    case MAX_PDU_SIZE:
    case MSG_BLOCK_TIME:
    case KEEP_ALIVE_TIME:
    case NUMBER_MESSAGES:
    case COMMAND_CODE:
    case MSG_LENGTH:
    case SALT_LENGTH:
    case HASH_LENGTH:
    case ENC_KEY_LENGTH:

        buf->param_code = code;

        if( (buf->param_length = Determine_Len (value)) != 0 ) {

            Load_Param (value, buf->param_length, &buf->param_data);

            return(buf->param_length +
                   sizeof(buf->param_length) +
                   sizeof(buf->param_code));
        } else {
            return(0);
        }
        break;

    case TERM_CLIENT_TAG:
    case APPLICATION_NAME:
    case RPC_REQUEST_TAG:
    case SESSION_TAG:
    case CORRELATION_TAG:

        buf->param_code = code;

        if( (buf->param_length = strlen(string_val)) != 0 ) {

            for( loc_idx = 0; 
               loc_idx < buf->param_length; 
               loc_idx++ ) {

                buf->param_data.param_value[loc_idx] = 
                string_val[loc_idx];
            }
            return(buf->param_length +
                   sizeof(buf->param_length) +
                   sizeof(buf->param_code));
        } else {
            return(0);
        }
        break;

    default:

        sprintf(err_string.par1, "parameter code");
        sprintf(err_string.par2, "%d", code);

        output_err("Build_Parameter",
                   MI_TNI_UNKNOWN,
                   MX_ERR_LVL_ERROR,
                   err_string);

        return(0);
        break;
    }
    return(0);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Check_Parameter (int conn,                                                 */
/*                  struct TNI_DATA *param_struct,                            */
/*                  unsigned char *code,                                      */
/*                  long int *length,                                         */
/*                  long int *value,                                          */
/*                  char *string_val)                                         */
/*                                                                            */
/* Purpose: This function breaks an input parameter code/length/value block   */
/*          into its components.                                              */
/*                                                                            */
/* Input Arguments:                                                           */
/*          conn             Connection the TNI data was received on          */
/*                                                                            */
/*          param_struct     Pointer to a parameter code/length/value block   */
/*                                                                            */
/* Output Arguments:                                                          */
/*          code             Parameter code                                   */
/*                                                                            */
/*          length           Parameter length                                 */
/*                                                                            */
/*          value            Parameter integer value                          */
/*                                                                            */
/*          string_val       Parameter string value                           */
/*                                                                            */
/* Return Value:                                                              */
/*          int              1 Parsing successful                             */
/*                           0 Error                                          */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int Check_Parameter (int conn,
                     struct TNI_DATA *param_struct, 
                     unsigned char *code,
                     long int *length, 
                     long int *value,
                     char *string_val) {

    int loc_idx;

    unsigned char bvalue;                              /* local byte variable */

/* Assume that first byte following fixed header is a parameter block         */

    *code = param_struct->param_code;
    *length = param_struct->param_length;

    switch( *code ) {
    case TERM_CLIENT_TAG:

        if ((*length > 0) && (*length <= MAX_TERM_CLIENT_TAG_LEN))
        {
            for( loc_idx = 0; loc_idx < *length; loc_idx++ ) {

                string_val[loc_idx] = 
                param_struct->param_data.param_value[loc_idx];
            }
            string_val[*length] = '\0';
        } else {
            return(0);
        }
        break;

    case APPLICATION_NAME:

        if (tnicon_p->connection[conn].tni_proto_ver != TNI_VERSION_01)
        {
            if ((*length > 0) && (*length <= MXSRV_MAX_APP_NAME_LEN))
            {
                for( loc_idx = 0; loc_idx < *length; loc_idx++ ) {

                    string_val[loc_idx] =
                    param_struct->param_data.param_value[loc_idx];
                }
                string_val[*length] = '\0';
            } else {
                return(0);
            }
        }
        else
        {
            return(0);
        }
        break;

    case RPC_REQUEST_TAG:

        if (tnicon_p->connection[conn].tni_proto_ver != TNI_VERSION_01)
        {
            if ((*length > 0) && (*length <= MAX_RPC_RQST_TAG_LEN))
            {
                for( loc_idx = 0; loc_idx < *length; loc_idx++ ) {

                    string_val[loc_idx] =
                    param_struct->param_data.param_value[loc_idx];
                }
                string_val[*length] = '\0';
            } else {
                return(0);
            }
        }
        else
        {
            return(0);
        }
        break;

    case SESSION_TAG:

        switch (tnicon_p->connection[conn].tni_proto_ver)
        {
        case TNI_VERSION_30:
        case TNI_VERSION_23:
        case TNI_VERSION_22:
        case TNI_VERSION_21:

            if ((*length > 0) && (*length <= MAX_SESSION_TAG_LEN))
            {
                for( loc_idx = 0; loc_idx < *length; loc_idx++ ) {

                    string_val[loc_idx] =
                    param_struct->param_data.param_value[loc_idx];
                }
                string_val[*length] = '\0';
            } else {
                return(0);
            }
            break;

        default:

            return(0);
            break;
        }
        break;

    case CORRELATION_TAG:

        switch (tnicon_p->connection[conn].tni_proto_ver)
        {
        case TNI_VERSION_30:
        case TNI_VERSION_23:
        case TNI_VERSION_22:

            if ((*length > 0) && (*length <= MAX_CORRELATION_TAG_LEN))
            {
                for( loc_idx = 0; loc_idx < *length; loc_idx++ ) {

                    string_val[loc_idx] =
                    param_struct->param_data.param_value[loc_idx];
                }
                string_val[*length] = '\0';
            } else {
                return(0);
            }
            break;

        default:

            return(0);
            break;
        }
        break;

    case CLIENT_ID:
    case MAX_MESSAGE_SIZE:
    case MAX_PDU_SIZE:
    case MSG_BLOCK_TIME:
    case KEEP_ALIVE_TIME:
    case NUMBER_MESSAGES:
    case TERM_CLIENT_ID:
    case TERM_SERVER_ID:
    case DELIVERY_MODE:
    case MSG_LENGTH:
    case PRIMARY_STATUS:
    case TERM_METHOD:
    case COMMAND_CODE:
    case RESULT_CODE:
    case SALT_LENGTH:
    case HASH_LENGTH:
    case HASH_ALGORITHM:
    case ENC_KEY_X_METHOD:
    case ENC_KEY_TYPE:
    case ENC_KEY_LENGTH:
    case ENC_MODE:

        if(tnicon_p->connection[conn].tni_proto_ver < TNI_VERSION_23)
        {
            switch(*code)
            {
                case SALT_LENGTH:
                case HASH_LENGTH:
                case HASH_ALGORITHM:    
                    return(0);
                    break;
                default:
                    break;
            }
        }

        if(tnicon_p->connection[conn].tni_proto_ver < TNI_VERSION_30)
        {
            switch(*code)
            {
                case ENC_KEY_X_METHOD:
                case ENC_KEY_TYPE:
                case ENC_KEY_LENGTH:
                case ENC_MODE:
                    return(0);
                    break;
                default:
                    break;
            }
        }

        switch( *length ) {

        case 1:

            bvalue = *param_struct->param_data.param_value;
            *value = (long)bvalue;
            break;

        case 2:

            *value = (long)ntohs(param_struct->param_data.s_param_value);
            break;

        case 3:

            return(0);
            break;

        case 4:

            *value = ntohl (param_struct->param_data.i_param_value);
            break;

        default:

            return(0);

        }
        break;

    case MESSAGING_TYPE:

        if (tnicon_p->connection[conn].tni_proto_ver != TNI_VERSION_01)
        {
            switch( *length ) {
        
            case 1:

                bvalue = *param_struct->param_data.param_value;
                *value = (long)bvalue;
                break;

            case 2:

                *value = (long)ntohs(param_struct->param_data.s_param_value);
                break;

            case 3:

                return(0);
                break;

            case 4:

                *value = ntohl (param_struct->param_data.i_param_value);
                break;

            default:

                return(0);

            }
        }
        else
        {
            return(0);
        }

        break;

    default:

        return(0);
    }

    return(1);

}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Load_Param (long int value,                                                */
/*             unsigned char len,                                             */
/*             union PARAM_DATA *buf)                                         */
/*                                                                            */
/* Purpose: This function loads 'len' bytes of 'value' into the variable      */
/*          length TNI variable parameter value field in network order.       */
/*                                                                            */
/* Input Arguments:                                                           */
/*          value            Parameter value                                  */
/*                                                                            */
/*          len              Parameter length in bytes                        */
/*                                                                            */
/* Output Arguments:                                                          */
/*          buf              Pointer to parameter field                       */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Load_Param (long int value, unsigned char len, union PARAM_DATA *buf) {

    switch( len ) {
    case 1:
        buf->param_value[0] = (unsigned char)value;
        break;

    case 2:
        buf->s_param_value = htons((unsigned short)value);
        break;

    case 3:
        break;

    case 4:
        buf->i_param_value = htonl(value);
        break;
    }
}

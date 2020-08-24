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
/*====[TNIVIS_READ.C]=========================================================*/
/*                                                                            */
/* FUNCTION NAME AND CALLING PARAMETERS GO HERE                               */
/*                                                                            */
/* Purpose:                                                                   */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/* Output Arguments:                                                          */
/*                                                                            */
/* Return Value:                                                              */
/*                                                                            */
/* Assumptions:                                                               */
/*                                                                            */
/*====[TNIVIS_READ.C]=========================================================*/
/*                                                                            */

#include <string.h>
#include <errno.h>

#include "includes_vision.h"

void client() {

    int numbufindex;
    int par;
    int localpar;
    int icount;
    int n,index;
    int i;
    int loop_index =1;
    int foundPar = 0;
    int buff_pos = 0;
    int local_input_x;
    int buff_len;

    char numericbuff[MAXCOL];
    char buff[MAXCOL];
    char user_inp;

    noecho();

    while( loop_index == 1 ) {
        memset(buff,'\0',80);

#       if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

            cbreak();
	
#       elif defined(XOS_VMS)

            crmode();
	
#       else

#           error - OS-specific logic not handled.

#       endif

        buff[buff_pos] = getch();

        while ((buff[buff_pos] != '\r') &&
               ((unsigned char)buff[buff_pos] != 10)) {

            if ((buff[buff_pos] != '\b') &&
                ((unsigned char)buff[buff_pos] != 127)) {

                if (buff_pos == 0) {
                    local_input_x = input_x;
                }

                wmove(win,input_y,local_input_x);
                waddch(win,buff[buff_pos]);

                local_input_x++;
                buff_pos++;

                if (local_input_x >= MAXCOL)
                {
                    local_input_x = input_x;
                }

                if (buff_pos >= MAXCOL)
                {
                    buff_pos = 0;
                }
            } else {

                local_input_x--;
                buff_pos--;

                if (local_input_x >= input_x)
                {
                    wmove(win,input_y,local_input_x);
                    waddch(win,' ');
                } else {

                   local_input_x = input_x;
                   buff_pos = 0;
                }
            }

            buff[buff_pos] = getch();
        }

/*      Remove end of line character */

        buff[buff_pos] = '\0';

        buff_pos = 0;
        local_input_x = input_x;

        n=strlen(buff);

        par = -1; 
        numbufindex = 0;

/*      Do not look for digits when entering log file name */

        if (current_menu != CHNG_LOG_FILE)
        {
            for( i = 0; i < n; i++ ) {

                if( isdigit(buff[i]) != 0 ) {

                    numericbuff[numbufindex++] = buff[i];
                    buff[i] = '\0';
                    foundPar = 1;

                } else if (buff[i] == ' ') {
                    buff[i] = '\0';
                }
            }
        }

        buff_len = strlen(buff);

        if( foundPar == 1 ) {

            errno = 0; 

            localpar = strtoul(numericbuff, NULL, 10);

            if( errno == 0 ) {

                par = localpar;                     

            } else {

                /* AN ERROR OCCURRED */
                perror(getErrorMsg(errno));
            }

            memset ( numericbuff , '\0' , MAXCOL); 
            foundPar = 0;
        }

        if (buff_len > 0) {

            if( ( strcasecmp(buff, "CON") ) == 0 ) {
                current_menu = CONNECTION;
            } else if( ( strcasecmp( buff, "APP") ) == 0 ) {
                current_menu = APPLICATION;
            } else if( ( strcasecmp( buff, "CLIENT") ) == 0 ) {
                current_menu = CLIENTID;
            } else if( ( strcasecmp( buff, "RPC") ) == 0 ) {
                current_menu = RPC;
            } else if( ( strcasecmp( buff, "SERVER") ) == 0 ) {
                current_menu = SERVER_SUMMARY;
            } else if( ( strcasecmp( buff, "MENU") ) == 0 ) {
                current_menu = MAIN_MENU;
            } else if( ( strcasecmp( buff, "DEBUG") ) == 0 ) {
                current_menu = DEBUG;
            } else if( ( strcasecmp( buff, "LOG") ) == 0 ) {
                current_menu = LOG;
            } else if( ( strcasecmp( buff, "EVENT") ) == 0 ) {
                current_menu = EVENT_QUEUES; 

#           if defined(PROSYS_ENV_ALL)

                } else if( ( strcasecmp( buff, "TERM") ) == 0 ) {
                    current_menu = TERM;

#           endif

            } else if( ( strcasecmp( buff, "EXIT") ) == 0 ||
                       ( strcasecmp( buff, "STOP") ) == 0 ||
                       ( strcasecmp( buff, "QUIT") ) == 0 ) {
                loop_index = 0;
                current_menu = MXVIS_STOP;            
            }
        }

        wmove(win,input_y,input_x);
        clrtoeol();
        refresh();

        switch (current_menu) {

        case (APPLICATION):
            App_Input(buff, par);
            break;

        case (CLIENTID):
            Client_Id_Input(buff, par);
            break;

        case (CONNECTION):
            Conn_Input(buff, par);
            break;

        case (DEBUG):
            Debug_Input(buff, par);
            break;

        case (DEBUG_TERMINAL):
            Debug_Terminal_Input(buff, par);
            break;

        case (EVENT_QUEUES):
            Event_Queues_Input(buff, par);
            break;

        case (LOG):
        case (CHNG_LOG_FILE):
        case (CHNG_LOG_THRESHOLD):

            Logging_Input(buff, par);
            break;

        case (RPC):
            Rpc_Messaging_Input(buff, par); 
            break;

        case (SERVER_SUMMARY):
            Serv_Summary_Input(buff, par);
            break;

#       if defined(PROSYS_ENV_ALL)

            case (TERM):
                Term_Serv_Input(buff, par);
                break;

#       endif

        case (MAIN_MENU):
        case (MXVIS_STOP):
            break;
        }
    }
}

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
/*====[TNIVIS_LOG.C]==========================================================*/
/*                                                                            */
/*                                                                            */
/* Purpose: These functions provide the ability to enable/disable logging     */
/*          and also edit MX server's logging funcionality through the        */
/*          Log menu in MxVision                                              */
/*                                                                            */
/* Functions:                                                                 */
/*          void Logging()                                                    */
/*          void ChngLogFile()                                                */
/*          void ChngLogThreshold()                                           */
/*          void UpdateLogFile(char *file)                                    */
/*          void Logging_Input(char *command, int par)                        */
/*                                                                            */
/*                                                                            */
/*====[TNIVIS_LOG.C]==========================================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void Logging()                                                             */
/*                                                                            */
/* Purpose: This function is the Log menu in mxvision                         */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/*                                                                            */
/* Output Arguments:                                                          */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Logging() {

    char logging_status[14];

    switch( tnicon_p->log_state ) {
    
    case LOG_INIT_FILE:
    case LOG_ACTIVE:
    case LOG_CLOSE:

        sprintf(logging_status, "on            ");
        break;

    case LOG_OFF:

        sprintf(logging_status, "off           ");
        break;

    case LOG_FILE_ERR:

        sprintf(logging_status, "log file error");
        break;

    default:
        sprintf(logging_status, "Error. Unknown logging status");
    }

    wmove(win,0,0);
    wprintw(win,"LOGGING SNAPSHOT");

    wmove(win,0, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    wmove(win, 4, 0);
    wprintw(win, "Logfile: %s", tnicon_p->log_file_name); 

    wmove(win,5,0);
    wprintw(win,"Logging is %s", logging_status);

    wmove(win,6,0);
    wprintw(win,"Logging Threshold: %u chars/minute", tnicon_p->log_threshold); 
    clrtoeol();

    wmove(win,6,40);
    wprintw(win,"Amount Logged: %u chars/minute", tnicon_p->amount_logged); 
    clrtoeol();

    wmove(win,10,0);
    wprintw(win,"Toggle Logging: Enter 1 - (Enable), 0 - (Disable)");

    wmove(win,21,0);
    wprintw(win,"EDIt log location, EXIT, MENU, THReshold change");

    wmove(win,23,0);
    wprintw(win,"Selection: ");

    wmove(win,23,11);

    wrefresh(win);

    input_y = 23;
    input_x = 11;
}   

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void ChngLogFile()                                                         */
/*                                                                            */
/* Purpose: This function is a submenu of the Log menu in mxvision and which  */
/*          allows for the changing of MX logFile                             */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/*                                                                            */
/* Output Arguments:                                                          */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void ChngLogFile() {

    wmove(win,0,0);
    wprintw(win,"Change Log Location");

    wmove(win,0, (MAXCOL - strlen(Get_Time())));
    wprintw (win, "%s", Get_Time() );
    clrtoeol();

    wmove(win,4,0);
    wprintw(win, "Logfile: %s", tnicon_p->log_file_name); 

    wmove(win,21,0);
    wprintw(win,"EXIT, MENU");

    wmove(win,23,0);
    wprintw(win,"New log location: ");

    wmove(win,23,18);

    wrefresh(win);

    input_y = 23;
    input_x = 18;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void ChngLogThreshold()                                                    */
/*                                                                            */
/* Purpose: This function is a submenu of the Log menu in mxvision and which  */
/*          allows for the changing of MX logging threshold                   */
/*                                                                            */
/* Input Arguments:                                                           */
/*                                                                            */
/*                                                                            */
/* Output Arguments:                                                          */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void ChngLogThreshold() {

    wmove(win,0,0);
    wprintw(win,"Change Logging Threshold");

    wmove(win,0, (MAXCOL - strlen(Get_Time())));
    wprintw (win, "%s", Get_Time() );
    clrtoeol();

    wmove(win,4,0);
    wprintw(win, "Logfile: %s", tnicon_p->log_file_name); 
    clrtoeol();

    wmove(win,6,0);
    wprintw(win,"Logging Threshold: %u chars/minute", tnicon_p->log_threshold); 
    clrtoeol();

    wmove(win,6,40);
    wprintw(win,"Amount Logged: %u chars/minute", tnicon_p->amount_logged); 
    clrtoeol();

    wmove(win,21,0);    
    wprintw(win,"EXIT, MENU");

    wmove(win,23,0);
    wprintw(win,"New logging threshold(chars/minute): ");

    wmove(win,23,37);

    wrefresh(win);

    input_y = 23;
    input_x = 37;
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void UpdateLogFile(char *file)                                             */
/*                                                                            */
/* Purpose: This function updates the MX logging file's location              */
/*                                                                            */
/* Input Arguments: New logging file                                          */
/*                                                                            */
/*                                                                            */
/* Output Arguments: MX logging file                                          */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void UpdateLogFile(char *file) {

    int status = 0;
    int update = 1;

    if( tnicon_p->log_state == LOG_ACTIVE ) {

        update = -1;

        wmove(win,10,0);
        wprintw(win, "%s", 
                "Please disable logging before changing log location.");
        clrtoeol();

    } else if( fopen(file, "r") != NULL ) {

        update = -1;    

        wmove(win,10,0);
        wprintw(win, "%s already exists.  Choose another",
                file);
        clrtoeol();
    }

    if( update == 1 ) {

        status = Generate_Vision_Cmd(CHANGE_LOG_PATH,
                                     0,
                                     0,
                                     file,
                                     NULL);
        Display_Cmd_Result(status);

        if (status == GOOD_CMD)
        {
            current_menu = LOG;
        }
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void Logging_Input(char *command, int par)                                 */
/*                                                                            */
/* Purpose: This function updates the MX logging file's location              */
/*                                                                            */
/* Input Arguments: Menu command, command data                                */
/*                                                                            */
/*                                                                            */
/* Output Arguments: Logging state, current mxvision menu                     */
/*                                                                            */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Logging_Input(char *command, int par) {

    int status = 0;

    if( current_menu == CHNG_LOG_FILE ) {

        if( strlen(command) > 0 ) {

            UpdateLogFile(command);
        }

    } else if( current_menu == CHNG_LOG_THRESHOLD ) {

        status = Generate_Vision_Cmd(CHANGE_LOG_THRESHOLD,
                                     par,
                                     0,
                                     NULL,
                                     NULL);
        Display_Cmd_Result(status);

        current_menu = LOG; 

    } else {

        if( par == 0 || par == 1 ) {

            status = Generate_Vision_Cmd(CHANGE_LOG_MODE,
                                         par,
                                         0,
                                         NULL,
                                         NULL);
            Display_Cmd_Result(status);
        }

        if( strcasecmp(command, "EDI") == 0 ) {

            current_menu = CHNG_LOG_FILE; 

        } else if( strcasecmp(command, "THR") == 0 ) {

            current_menu = CHNG_LOG_THRESHOLD;

        }
    }
}    

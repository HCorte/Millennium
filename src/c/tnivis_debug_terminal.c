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
/*====[TNIVIS_DEBUG_TERMINAL.C]===============================================*/
/*                                                                            */
/* Functions:                                                                 */
/*          Debug_Terminal()                                                  */
/*          Debug_Terminal_Input(char *command, int par)                      */
/*                                                                            */
/*====[TNIVIS_DEBUG_TERMINAL.C]===============================================*/
/*                                                                            */

#include <string.h>

#include "includes_vision.h"

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Debug_Terminal()                                                           */
/*                                                                            */
/* Purpose: This function displays the debug terminal snapshot.               */
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

void Debug_Terminal() {

    wmove(win,0,0);
    wprintw(win,"DEBUG SNAPSHOT");

    wmove(win,0, (MAXCOL - strlen(Get_Time())));
    clrtoeol();
    wprintw (win, "%s", Get_Time() );

    wmove(win,2,5);

    switch (tnicon_p->dbg_term_num) {

    case -2:

        wprintw(win,"Invalid terminal number!            ");
        wmove(win,23,0);
        wprintw(win,"Enter Terminal Number: ");
        wmove(win,23,23);
        break;

    case ALL_TERMINALS:

        wprintw(win,"Debug level: terminal (all)         ");
        current_menu = DEBUG;
        break;

    case 0:

        wprintw(win,"Debug level: terminal (?)           ");
        wmove(win,23,0);
        wprintw(win,"Enter Terminal Number: ");
        wmove(win,23,23);

        input_y = 23;
        input_x = 23;
        break;

    default:

        wprintw(win,"Debug level: terminal (%d)          ",
                tnicon_p->dbg_term_num);
        current_menu = DEBUG;
        break;
    }

    refresh();
    wrefresh(win);
} 

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Debug_Terminal_Input(char *command, int par)                               */
/*                                                                            */
/* Purpose: This function processes all the commands specific to the          */
/*          debug terminal snapshot.                                          */
/*                                                                            */
/* Input Arguments:                                                           */
/*          command          string portion of the command entered            */
/*          par              integer command or integer parameter accompaning */
/*                           command (optional)                               */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value: None                                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Debug_Terminal_Input(char *command, int par) {

    int status = 0;

    if (par != -1 )
    {
        status = Generate_Vision_Cmd(CHANGE_DEBUG_LEVEL,
                                     8,
                                     par,
                                     NULL,
                                     NULL);
        Display_Cmd_Result(status);
    }
}

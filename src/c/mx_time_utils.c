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
/*====[MX_TIME_UTILS.C]=======================================================*/
/*                                                                            */
/* Purpose: These functions provide time utilities.                           */
/*                                                                            */
/* Functions:                                                                 */
/*          Set_Timervalue (int tics)                                         */
/*          Show_Current_Time (char *caller)                                  */
/*          subtimes(struct timeb *intime1,                                   */
/*                   struct timeb *intime2,                                   */
/*                   struct timeb *outtime)                                   */
/*                                                                            */
/*====[MX_TIME_UTILS.C]=======================================================*/
/*                                                                            */

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

#include "includes.h"

#if defined(XOS_LINUX_RH) || defined(XOS_UNIX_AIX) || defined(XOS_UNIX_TRU64)

#   include <sys/timeb.h>

#elif defined(XOS_VMS)

#   include <descrip.h>
#   include <ssdef.h>
#   include <timeb.h>

#else

#   error - OS-specific logic not handled.

#endif

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Set_Timervalue_Vms (int tics)                                              */
/*                                                                            */
/* Purpose: In VMS this function converts an input in 10 millsecond tics to   */
/*          an ASCII string, calls SYS$BINTIM to get a VMS DELTA BINARY TIME  */
/*          structure, and returns the time structure.  A local time structure*/
/*          is used to get around stack corruption problems.                  */
/*                                                                            */
/* Input Arguments:                                                           */
/*          tics             Number of 10 millisecond tics                    */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          TIMER_VALUES     VMS delta binary time                            */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

struct timeb Set_Timervalue (int tics)
{
    int msec_to_wait;
    unsigned int days;
    unsigned int hours;
    unsigned int minutes;
    unsigned int seconds;
    unsigned int milliseconds;

    struct timeb time_rec;            /* returned structure               */

    time_rec.time = 0;
    time_rec.millitm = 0;

    msec_to_wait = tics * 10;

    days = 0;
    hours = 0;
    minutes = 0;
    seconds = 0;

    seconds = msec_to_wait/MS_IN_SEC;
    milliseconds = msec_to_wait - seconds*MS_IN_SEC;

    time_rec.time = seconds;
    time_rec.millitm = milliseconds;

    return (time_rec);
}

#if defined(XOS_VMS)

    struct TIMER_VALUES Set_Timervalue_Vms (int tics)
    {

/* For whatever reason, not doing these two lines causes this function to     */
/* blow away the stack.  These were directly taken from the DECC User's Guide */
/* for Open VMS Systems, section "Using Time Routines".                       */

        typedef int quadword[2];
        quadword delta_time_val;         /* local VMS binary time structure   */

        int st;                          /* VMS return status                 */
        int msec_to_wait;
        unsigned int days;
        unsigned int hours;
        unsigned int minutes;
        unsigned int seconds;
        unsigned int hundredths_of_sec;

#       pragma message disable (ADDRCONSTEXT)
#       pragma member_alignment save
#       pragma nomember_alignment

            char delta_asc_time[17];         /* ASCII time string             */

            $DESCRIPTOR (timedesc,delta_asc_time);

            struct TIMER_VALUES time_rec;    /* returned structure            */

#       pragma member_alignment restore

        time_rec.time1 = 0;
        time_rec.time2 = 0;

        msec_to_wait = tics * 10;

        days = 0;
        hours = 0;
        minutes = 0;
        seconds = 0;
        hundredths_of_sec = 0;

        minutes = msec_to_wait/MS_IN_MIN;
        seconds = (msec_to_wait - (minutes*MS_IN_MIN)) / MS_IN_SEC;
        hundredths_of_sec = ((msec_to_wait) - (minutes*MS_IN_MIN) -
                             (seconds * MS_IN_SEC)) / MS_IN_HUNDREDTHS;

        sprintf(delta_asc_time,"%04d %02d:%02d:%02d.%02d",
                days,hours,minutes,seconds,hundredths_of_sec);

        st = SYS$BINTIM (&timedesc, delta_time_val);
        if (st == SS$_NORMAL) {
            time_rec.time1 = delta_time_val[0];
            time_rec.time2 = delta_time_val[1];
            return (time_rec);
        }
    }

#endif

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* void Show_Current_Time (char *caller)                                      */
/*                                                                            */
/* Purpose: Print current system time to the standard output                  */
/*                                                                            */
/* Input Arguments:                                                           */
/*                   *caller:  pointer to name of calling function            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:     None                                                     */
/*                                                                            */
/* Assumptions:      None                                                     */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void Show_Current_Time (char *caller) {
 
    int st;

    char timeret[9];

    struct tm loc_time;
    struct tm *loc_time_p;

    loc_time_p = &loc_time;

    if( (st = ftime(&current_time)) != -1 )
    {
        loc_time_p = localtime(&current_time.time);
        strftime(timeret, 9, "%H:%M:%S", loc_time_p);

        fprintf (tnicon_p->dbg_p,
                 "\nCalling function %s, time = %s,%3.3d\n",
                 caller,
                 timeret,
                 current_time.millitm); 
    }
    else
    {
        fprintf(tnicon_p->dbg_p,
                "Show_Current_Time(ftime) failed\n");
    }
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Get_Time(void)                                                             */
/*                                                                            */
/* Purpose: Returns current system time.                                      */
/*                                                                            */
/* Input Arguments:  None                                                     */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:     Current systen time as an ASCII string                   */
/*                                                                            */
/* Assumptions:      None                                                     */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

char *Get_Time(void) {

    time_t rawtime;
    struct tm * timeinfo;

    time ( &rawtime );
    timeinfo = localtime ( &rawtime );

    return asctime (timeinfo);

}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* Tni_subtimes()                                                             */
/*                                                                            */
/* Purpose: This function computes the difference between the two input       */ 
/*          timeb sturctures and returns the result in the third timeb        */
/*          struture.                                                         */
/*                                                                            */
/* Input Arguments: Two timeb structures                                      */
/*                                                                            */
/* Output Arguments: timeb structure where the difference in time is stored   */
/*                                                                            */
/* Return Value:                                                              */
/*          void                                                              */         
/*                                                                            */
/* Assumptions:                                                               */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

void subtimes(struct timeb *intime1,struct timeb *intime2,struct timeb *outtime)
{
	time_t	diffsec, diffmsec;

	if (intime1->millitm >= intime2->millitm)
        {
                diffmsec = intime1->millitm - intime2->millitm;
                diffsec = intime1->time - intime2->time;
        }
        else
        {
                diffmsec = 1000 + (intime1->millitm - intime2->millitm);
                diffsec = (intime1->time - 1) - intime2->time;
        }

	outtime->time = abs(diffsec);
	outtime->millitm = abs(diffmsec);
} 

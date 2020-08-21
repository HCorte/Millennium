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
/*====[TNI_CFG.C]=============================================================*/
/*                                                                            */
/* Description:   TNI generic configuration file routines.                    */
/*                                                                            */
/* Functions:     tni_cfgread ()                                              */
/*                tni_cfgsect ()                                              */
/*                tni_cfgstr ()                                               */
/*                tni_cfgint ()                                               */
/*                                                                            */
/*====[TNI_CFG.C]=============================================================*/
/*                                                                            */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

#include "includes.h"

#define COMMENT         '#'
#define MXCFGLINELEN    132
#define PARMSEPS        " =\t\n"
#define QUOSEPS         "\"\n"

#define SECTOPEN        '['
#define SECTSEPS        " []\t\n"

#define NOTFOUND     1  /* Parameter not found    */
#define INVALID      2  /* Parameter value invalid */

static char *currbufp   = NULL;

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* tni_cfgread (FILE *fp, char *bufp, int blen)                               */
/*                                                                            */
/* Purpose: Read configuration file                                           */
/*                                                                            */
/*    The configuration file has the format:                                  */
/*                                                                            */
/*       [section1]                                                           */
/*       parm1 = value1                                                       */
/*       parm2 = value2                                                       */
/*       parm3 = value3                                                       */
/*       ...                                                                  */
/*                                                                            */
/*       [sectionN]                                                           */
/*       parm1 = value1                                                       */
/*       parm2 = value2                                                       */
/*       parm3 = value3                                                       */
/*       ...                                                                  */
/*                                                                            */
/*    The file is read, and fills the given buffer of a given size with       */
/*    all the assignments in the file accordingly.  The format in the         */
/*    buffer is:                                                              */
/*                                                                            */
/*       [<section1>0<name1>=<value1>0...<nameN>=<valueN>00                   */
/*                                                                            */
/*    where assignments are separated by '\0' and end of buffer is            */
/*    indicate by an extra '\0' appended.  Names do not have to be            */
/*    unique.                                                                 */
/*                                                                            */
/* Input Arguments:                                                           */
/*          fp               Configuration file handle                        */
/*                                                                            */
/*          bufp             Configuration buffer to be filled                */
/*                                                                            */
/*          blen             Configuration buffer size                        */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              Returns length of the buffer excluding the       */
/*                           terminating null character.  Returns -1 on       */
/*                           failure.                                         */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int 
tni_cfgread (FILE *fp, char *bufp, int blen)
{
   int i, len;
   char *lp, *namep, *valp, *np;
   char cfgline[MXCFGLINELEN];

   len = 0;

   /* Read each line from file  */
   while (fgets (cfgline, MXCFGLINELEN, fp)) {
      lp = cfgline;

      /* Get parameter/section name  */
      if ((namep = strtok (lp, PARMSEPS)) == NULL)
         continue;

      /* Comment lines  */
      if (*namep == COMMENT)
         continue;

      /* If this is a section head, get section name  */
      if (*namep == SECTOPEN) { 
         if ((namep = strtok (namep, SECTSEPS)) == NULL)
            return (-1);

         /* Insert section name  */
         bufp += sprintf (bufp, "%c%s", SECTOPEN, namep) + 1;
      }

      /* If this is not a section header, get value  */
      else {

         np = namep + strlen (namep) + 1;

         /* Get parameter value  */
         if ((i = strcspn (np, "\"")) < strlen (np)) {
            if ((valp = strtok (np + i, QUOSEPS)) == NULL)
               break;
         }

         else if ((valp = strtok (NULL, PARMSEPS)) == NULL)
            return (-1);

         /* Verify name/value will fit  */
         if ((len += strlen (namep) + strlen (valp) + 2) >= blen)
            break;

         /* Insert null-terminated parameter "name=value"  */
         bufp += sprintf (bufp, "%s=%s", namep, valp) + 1;
      }
   }

   *(bufp++) = 0;

   return (len);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* tni_cfgsect (char *namep, char *bufp)                                      */
/*                                                                            */
/* Purpose: Move to next configuration section                                */
/*                                                                            */
/*    This function searches for the next section with the given name,        */
/*    in a buffer built by calling tni_cfgread ().  Alternatively, if         */
/*    the name is NULL, the next section is found.                            */
/*                                                                            */
/*    If found, following calls to tni_cfgxxx () will start from this point.  */
/*                                                                            */
/* Input Arguments:                                                           */
/*          namep            Section name to be searched.  If NULL, next      */
/*                           section is returned.                             */
/*                                                                            */
/*          bufp             Configuration buffer.  If NULL, use current      */
/*                           position from previous tni_cfgsect () call.      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          char *           Returns pointer to section name, NULL if not     */
/*                           found.                                           */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

char *
tni_cfgsect (char *namep, char *bufp)
{
   char *p;

   p = bufp;

   if ((currbufp = bufp) == NULL)
      return (NULL);

   while (*p) {

      /* Start of new section  */
      if (*p == SECTOPEN) {
         p++;

         /* See if it is the section we want (or we don't care)  */
         if ((namep == NULL) || (!strcasecmp (p, namep))) {
            currbufp = p;

            return (currbufp);
         }
      }
         
      p += strlen (p) + 1;
   }      
   
   return (NULL);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* tni_cfgstr (char *namep, char *bufp)                                       */
/*                                                                            */
/* Purpose: Get string parameter value.                                       */
/*                                                                            */
/*      Search for value of a specified parameter name in a buffer built      */
/*      by calling tni_cfgread ().  Search starts from start of current       */
/*      section (bufp == NULL), or at given point.                            */
/*                                                                            */
/*      Search stops at end of current section, as though it were the end     */
/*      of the file.  Moving beyond this point requires a call to             */
/*      tni_cfgsect () to move to another section.                            */
/*                                                                            */
/* Input Arguments:                                                           */
/*          namep            Parameter name to be searched                    */
/*                                                                            */
/*          bufp             Configuration buffer.  If NULL, use current      */
/*                           position from previous tni_cfgsect () call.      */
/*                                                                            */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          char *           Returns pointer to parameter value, NULL if not  */
/*                           found.                                           */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

char *
tni_cfgstr (char *namep, char *bufp)
{
   size_t len;
   char *parmp;

   if (bufp == NULL)
      parmp = currbufp;

   /* If caller provided section pointer, skip section header  */
   else if ((strcspn (currbufp, "=")) == strlen (currbufp))
      parmp = bufp + strlen (bufp) + 1;

   if (parmp == NULL)
      return (NULL);

   /* Search until end of current section or buffer  */
   while (*parmp && (*parmp != SECTOPEN)) {
      len = strcspn (parmp, "=");

      if (!strncasecmp (parmp, namep, len))
         return (parmp + len + 1);

      parmp += strlen (parmp) + 1;
   }

   return (NULL);
}

/*                                                                            */
/*============================================================================*/
/*                                                                            */
/* tni_cfgint (char *namep, char *bufp, int base)                             */
/*                                                                            */
/* Purpose: Get integer parameter value.                                      */
/*                                                                            */
/*    Search for value of a specified parameter name in a buffer built        */
/*    by calling tni_cfgread ().  Search starts from start of current         */
/*    section (bufp == NULL), or at given point.                              */
/*                                                                            */
/*    Search stops at end of current section, as though it were the end       */
/*    of the file.  Moving beyond this point requires a call to               */
/*    tni_cfgsect () to move to another section.                              */
/*                                                                            */
/* Input Arguments:                                                           */
/*          namep            Parameter name to be searched                    */
/*                                                                            */
/*          bufp             Configuration buffer.  If NULL, use current      */
/*                           position from previous tni_cfgsect () call.      */
/*                                                                            */
/*          base             Parameter value base.  If set to zero, will      */
/*                           determine base from parameter value format       */
/*                           (defaulting to base 10)                          */
/*                                                                            */
/*          mode             'M' keyword must be found or error message is    */
/*                               sent to ELOG                                 */
/*                           'D' if keyword is not found no error message is  */
/*                               sent to ELOG                                 */
/* Output Arguments: None                                                     */
/*                                                                            */
/* Return Value:                                                              */
/*          int              Returns parameter value, -1 if not found.        */
/*                                                                            */
/* Assumptions: None                                                          */
/*                                                                            */
/*============================================================================*/
/*                                                                            */

int
tni_cfgint (char *namep, char *bufp, int base, char mode)
{
   long int val;
   char *valp, *endp;

   err_string = null_err_string;

   /* Find parameter  */
   if (((valp = tni_cfgstr (namep, bufp)) == NULL) || (*valp == '\0')) {

      if ((mode == 'M') || (mode == 'm')) {

          sprintf(err_string.par1,"%s",namep);

          output_err("tni_cfgint",
                     MI_TNI_NOKEYWORD,
                     MX_ERR_LVL_ERROR,
                     err_string);
      }
      return (-1);
   }

   val = strtol (valp, &endp, base);

   if ((endp == valp) || (val == LONG_MAX) || (val == LONG_MIN)) {

      sprintf(err_string.par1, "%s", namep);
      sprintf(err_string.par2, "%ld", (unsigned long) valp);

      output_err("tni_cfgint",
                 MI_TNI_BADKEYWORD,
                 MX_ERR_LVL_ERROR,
                 err_string);

      return (-1);
   }

   return (val);
}

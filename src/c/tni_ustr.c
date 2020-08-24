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
/*====[TNI_USTR.C]============================================================*/
/*                                                                            */
/*   Description:   Communications Management - String Handling               */
/*                                                                            */
/*   Functions:     strcasecmp ()                                             */
/*                  strncasecmp ()                                            */
/*                  strtrim ()                                                */
/*                  strupper ()                                               */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*====[TNI_USTR.C]============================================================*/
/*                                                                            */


#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdarg.h>

#include "includes.h"

#if defined(XCC_VMS_OLD)

/* ---[ strcasecmp ]-----------------------------------------------------

   Summary:

      strcasecmp (const char *s1, const char *s2)

   Description:

      Compares string arguments 
      
   Input:

      s1             String pointer

      s2             String pointer

   Output:

      None

   Return Value:  

      int
         Returns an integer greater than, equal to, or less than 0, 
         according as s1 is lexicographically greater than, equal to, 
         or less than s2, ignoring differences in case.

   Possible Errors:
  
      None

   ----------------------------------------------------------------------- */

/*
 * This array is designed for mapping upper and lower case letter
 * together for a case independent comparison.  The mappings are
 * based upon ASCII character sequences.
 */

/*   Removed single quotes around each element and inserted 0 for each      */
/*   element for Octal representation                                       */

static unsigned char charmap[] = {
	   0,   01,   02,   03,   04,   05,   06,   07,
         010,  011,  012,  013,  014,  015,  016,  017,
         020,  021,  022,  023,  024,  025,  026,  027,
         030,  031,  032,  033,  034,  035,  036,  037,
         040,  041,  042,  043,  044,  045,  046,  047,
         050,  051,  052,  053,  054,  055,  056,  057,
         060,  061,  062,  063,  064,  065,  066,  067,
         070,  071,  072,  073,  074,  075,  076,  077,
        0100, 0141, 0142, 0143, 0144, 0145, 0146, 0147,
        0150, 0151, 0152, 0153, 0154, 0155, 0156, 0157,
        0160, 0161, 0162, 0163, 0164, 0165, 0166, 0167,
        0170, 0171, 0172, 0133, 0134, 0135, 0136, 0137,
        0140, 0141, 0142, 0143, 0144, 0145, 0146, 0147,
        0150, 0151, 0152, 0153, 0154, 0155, 0156, 0157,
        0160, 0161, 0162, 0163, 0164, 0165, 0166, 0167,
        0170, 0171, 0172, 0173, 0174, 0175, 0176, 0177,
        0200, 0201, 0202, 0203, 0204, 0205, 0206, 0207,
        0210, 0211, 0212, 0213, 0214, 0215, 0216, 0217,
        0220, 0221, 0222, 0223, 0224, 0225, 0226, 0227,
        0230, 0231, 0232, 0233, 0234, 0235, 0236, 0237,
        0240, 0241, 0242, 0243, 0244, 0245, 0246, 0247,
        0250, 0251, 0252, 0253, 0254, 0255, 0256, 0257,
        0260, 0261, 0262, 0263, 0264, 0265, 0266, 0267,
        0270, 0271, 0272, 0273, 0274, 0275, 0276, 0277,
        0300, 0341, 0342, 0343, 0344, 0345, 0346, 0347,
        0350, 0351, 0352, 0353, 0354, 0355, 0356, 0357,
        0360, 0361, 0362, 0363, 0364, 0365, 0366, 0367,
        0370, 0371, 0372, 0333, 0334, 0335, 0336, 0337,
        0340, 0341, 0342, 0343, 0344, 0345, 0346, 0347,
        0350, 0351, 0352, 0353, 0354, 0355, 0356, 0357,
        0360, 0361, 0362, 0363, 0364, 0365, 0366, 0367,
        0370, 0371, 0372, 0373, 0374, 0375, 0376, 0377
};

#ifndef _DECC_V4_SOURCE

int strcasecmp (const char *s1, const char *s2)
{
   register unsigned char *cm = charmap,
                          *us1 = (unsigned char *)s1,
                          *us2 = (unsigned char *)s2;

/*   printf("s1 %s s2 %s\n",s1,s2); */
   while (cm[*us1] == cm[*us2++]) {
/*   printf("%c %c\n",cm[*us1],cm[*us2]); */
      if (*us1++ == '\0')
         return (0);
   }

   return (cm[*us1] - cm[*--us2]);
}                                                        

#endif


/* ---[ strncasecmp ]----------------------------------------------------

   Summary:

      strncasecmp (const char *s1, const char *s2, size_t n)

   Description:

      Compares string arguments 
      
      Perform same comparison as strcasecmp (), but compare at most n
      characters.

   Input:

      s1             String pointer

      s2             String pointer

      n              Maximum characters to compare

   Output:

      None

   Return Value:  

      int
         Returns an integer greater than, equal to, or less than 0, 
         according as s1 is lexicographically greater than, equal to, 
         or less than s2, ignoring differences in case.

   Possible Errors:
  
      None

   ----------------------------------------------------------------------- */

#ifndef _DECC_V4_SOURCE

int strncasecmp (const char *s1, const char *s2, size_t n)
{
   register unsigned char *cm = charmap,
                          *us1 = (unsigned char *)s1,
                          *us2 = (unsigned char *)s2;

/*   printf("s1 %s s2 %s len %d\n",s1,s2,n); */
   while ((--n >= 0) && (cm[*us1] == cm[*us2++])) {
/*   printf("%c %c\n",cm[*us1],cm[*us2]); */
      if (*us1++ == '\0'){
         return (0);
      }
   }

/*   printf("%c %c %d\n",cm[*us1],cm[*us2],n); */

   if (((int) n) < 0) {
     if (*--us2 =='\0') {
/*       printf("here\n"); */
       return(0);
     }
     else {
/*       printf("here1\n"); */
       return(0);
     }
   }
   else {
/*   printf("here2\n"); */
     return(1);
   }
}

#endif

#endif

/* ---[ strtrim ]--------------------------------------------------------

   Summary:

      strtrim (char *strp)

   Description:

      Trims leading and trailing white space from string

   Input:

      strp           String pointer

   Output:

      None

   Return Value:  

      void *
         Returns new pointer to head of string

   Possible Errors:
  
      None

   ----------------------------------------------------------------------- */
void *
strtrim (char *strp)
{
   char *fp, *ep;

   /* Trim leading white space  */
   for (fp = strp; *fp && isspace (*fp); fp++)
      ;

   /* Trim trailing white space  */
   for (ep = fp + strlen (fp); *ep && isspace (*ep); *ep-- = '\0')
      ;

   return (fp);
}


/* ---[ strtrim ]--------------------------------------------------------

   Summary:

      strupper (char *s)

   Description:

      Convert character string to upper case

   Input:

      s              String pointer

   Output:

      None

   Return Value:  

      char *
         Returns input parameter s

   Possible Errors:
  
      None

   ----------------------------------------------------------------------- */
char *
strupper (char *s)
{
   register char *sp = (char *)s;

   while (*sp) {
      if (islower (*sp))
         *sp = (*sp - 'a') + 'A';

      sp++;
   }

   return (s);
}


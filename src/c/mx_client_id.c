static const char *fileid = "";

/*
 * ===[mx_client_id.c]====================================================
 *
 * Description:
 *
 * Functions used to find and verify the Client Identifier 
 * and Client Index .
 *
 * Functions:
 *
 * Find_Client_Idx
 * Validate_Client_Id
 * Part_Client_Ids
 * Q_Sort_Client_Ids
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
 * Copyright (c) 2004 GTECH Corporation.  All rights reserved.
 * -----------------------------------------------------------------------
 * ======================================================================= */

#include "includes.h"

/* [Find_Client_Idx]
 *
 * Summary:
 *
 * Find_Client_Idx(int client_id_val)
 *
 * Description:
 *
 * This function searches the Client Index Table for the client id value 
 * If an entry exists for the client id value, then the table index is 
 * returned.  If an entry does not exist and there is room, it is
 * added to the Client Index Table.  If an entry does not exist and
 * there is no room, then -1 is retunred.
 *
 * Input Arguments:
 *
 * client_id_val - Client identifier to be search for
 *
 * Returns Values:
 *
 * Client Index assoicated with Client identifier or -1 if not found
 *
 */

int Find_Client_Idx(int client_id_val)
{
    int                 client_id_idx; 
    int                 index1;

/*  Find which index in the Client Idx Table contains the client id.  If the  */
/*  client id is not found, then add it to the Host Idx Table.  Assuming      */
/*  there is room.                                                            */

    client_id_idx = -1;
    index1 = 0;

    while ((index1 < MAX_NUM_CLIENT_IDS) && (client_id_idx == -1))
    {
        if (tnicon_p->client_idx_tbl[index1] == 0)
        {                    
            tnicon_p->client_idx_tbl[index1] = client_id_val;
            client_id_idx = index1;
        }
        else if (tnicon_p->client_idx_tbl[index1] == client_id_val)
        {
            client_id_idx = index1;
        }
        else
        {
            index1++;
        }
    }

    return(client_id_idx);
}

/* [Validate_Client_Id]
 *
 * Summary:
 *
 * Validate_Client_Id (int client_id_val)
 *
 * Description:
 *
 * This function searches the Client Index Table for the client id value
 * If an entry exists for the client id value, then the table index is
 * returned. If an entry does not exist then -1 is retunred. 
 *
 * Input Arguments:
 *
 * client_id_val - Client identifier to be search for
 *
 * Returns Values:
 *
 * Client Index assoicated with Client identifier or -1 if not found
 *
 */

int
Validate_Client_Id (int client_id_val)
{
    int                 client_id_idx;
    int                 index1;

/*  Find which index in the Client Idx Table contains the client id.          */

    client_id_idx = -1;
    index1 = 0;

    while ((index1 < MAX_NUM_CLIENT_IDS) && (client_id_idx == -1))
    {
        if (tnicon_p->client_idx_tbl[index1] == client_id_val)
        {
            client_id_idx = index1;
        }
        else
        {
            index1++;
        }
    }

    return(client_id_idx);
}

/* [Part_Client_Ids]
 *
 * Summary:
 *
 * Part_Client_Ids(unsigned long int head,
 *                 unsigned long int tail,
 *                 unsigned long int *lowhead,
 *                 unsigned long int *lowtail,
 *                 unsigned long int *highhead,
 *                 unsigned long int *hightail)
 *
 * Description:
 *
 * This function performs the partition portion of the quick
 * sort.  The pivot point is the middle of the input array (Sorted
 * Client Ids).  All elements less than the pivot point are moved
 * into the sub array bounded by the indexes lowhead and lowtail.
 * All elements greater than the pivot point are moved into the
 * sub array bounded by the indexes highhead and hightail.
 *
 * Input Arguments:
 *
 *          head             First index in the input array
 * 
 *          tail             Last index in the input array
 * 
 * Output Arguments:
 *
 *          lowhead          First index in the sub array containing
 *                           the elements less than the pivot point
 *
 *          lowtail          Last index in the sub array containing
 *                           the elements less than the pivot point
 *
 *          highhead         First index in the sub array containing
 *                           the elements greater than the pivot point
 * 
 *          hightail         Last index in the sub array containing
 *                           the elements greater than the pivot point
 *
 * Output Arguments: None
 *
 * Return Value: None
 * 
 * Assumptions: None
 *
 */

void
Part_Client_Ids (unsigned long int head,
                 unsigned long int tail,
                 unsigned long int *lowhead,
                 unsigned long int *lowtail,
                 unsigned long int *highhead,
                 unsigned long int *hightail)
{
unsigned int pivot_key;
unsigned int temp_item;
unsigned long int pivot_idx;

unsigned long int i,j;

    pivot_idx = (head + tail)/2;

    pivot_key = tnicon_p->sorted_client_ids[pivot_idx];

    i = head - 1;
    j = tail + 1;

    do
    {
        do
        {
            i++;
        } while (tnicon_p->sorted_client_ids[i] < pivot_key);

        do
        {
            j--;
        } while (tnicon_p->sorted_client_ids[j] > pivot_key);

        if (i < j)
        {
            temp_item = tnicon_p->sorted_client_ids[i];

            tnicon_p->sorted_client_ids[i] =
                tnicon_p->sorted_client_ids[j];

            tnicon_p->sorted_client_ids[j] = temp_item;
        }

    } while (i < j);

    *lowhead = head;
    *hightail = tail;

    if (i == j)
    {
        *lowtail = j - 1;
        *highhead = i + 1;
    }
    else
    {
        *lowtail = j;
        *highhead = i;
    }
}

/* [Q_Sort_Cleint_Ids]
 *
 * Summary:
 *
 * Q_Sort_Cleint_Ids (unsigned long int head, unsigned long int tail)
 *
 * Description:
 *
 * This function performs a Quick Sort on the Sorted Client Ids
 * array.  The array is partition into two sub array.  Each sub
 * array is then Quick Sorted until there are only two elements
 * in the array. 
 *
 * Input Arguments:
 *
 *          head             First index in the Sorted Client Ids array
 *
 *          tail             Last populated index in the Client Ids array
 *
 * Output Arguments: None
 *
 * Return Value: None
 * 
 * Assumptions: None
 *
 */

void
Q_Sort_Client_Ids (unsigned long int head, 
                   unsigned long int tail)
{
unsigned long int lowhead;
unsigned long int lowtail;
unsigned long int highhead;
unsigned long int hightail;

    if (head < tail)
    {
        Part_Client_Ids (head,
                         tail,
                         &lowhead,
                         &lowtail,
                         &highhead,
                         &hightail);

        Q_Sort_Client_Ids (lowhead, lowtail);
        Q_Sort_Client_Ids (highhead, hightail);
    }
}

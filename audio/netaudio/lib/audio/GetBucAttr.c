/**
 * Copyright 1993 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 *
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $NCDId: @(#)GetBucAttr.c,v 1.6 1994/01/14 01:32:15 greg Exp $
 */

#include "Alibint.h"

typedef struct _CacheEntryRec
{
    AuBucketAttributes *attr;
    AuServer       *aud;
    struct _CacheEntryRec *prevServer,
                   *nextServer,
                   *prev,
                   *next;
}               CacheEntryRec, *CacheEntryPtr;

static CacheEntryPtr cache;

void
_AuAddToBucketCache(aud, attr)
AuServer       *aud;
AuBucketAttributes *attr;
{
    CacheEntryPtr   c,
                    e,
                    p;

    if (!(e = (CacheEntryPtr) Aumalloc(sizeof(CacheEntryRec))))
	return;

    e->attr = attr;
    e->aud = aud;
    attr->cached = AuTrue;

    p = NULL;
    c = cache;

    /* look for our server */
    while (c)
    {
	if (c->aud == aud)
	    break;		/* found it */

	p = c;			/* remember the previous server */
	c = c->nextServer;
    }

    /* point to previous server entry */
    e->prevServer = p;

    /* if there's no previous server entry, we must be the head of the list */
    if (!p)
	cache = e;

    /* if we found our server then add us to the head of the list */
    if (c)
    {
	e->nextServer = c->nextServer;
	c->prev = e;
    }
    else
	/* otherwise, we're the only one in the list */
	e->nextServer = NULL;

    /* we're at the head of the attribute list */
    e->prev = NULL;
    e->next = c;
}

AuBucketAttributes *
_AuLookupBucketInCache(aud, bucket)
AuServer       *aud;
AuBucketID      bucket;
{
    CacheEntryPtr   c = cache;

    while (c)
    {
	if (c->aud == aud)
	{
	    do
	    {
		if (AuBucketIdentifier(c->attr) == bucket)
		    return c->attr;

		c = c->next;
	    } while (c);
	}
	else
	    c = c->nextServer;
    }

    return NULL;
}

static void
removeFromCache(c)
CacheEntryPtr   c;
{
    /* if there's a previous attribute, point it around this one */
    if (c->prev)
	c->prev->next = c->next;
    else
    {				/* otherwise, we're the head attribute */
	/* if there's a previous server, point it around this one */
	if (c->prevServer)
	    c->prevServer->nextServer = c->nextServer;
	else
	    /* otherwise, we're the head of the server list */
	    cache = c->nextServer;

	/* if there's a next server, point it around this one */
	if (c->nextServer)
	    c->nextServer->prevServer = c->prevServer;
    }

    /* if there's a next attribute, point it around this one */
    if (c->next)
    {
	c->next->prev = c->prev;

	/* if there's no previous attribute, propagate the server links */
	if (!c->prev)
	{
	    c->next->nextServer = c->nextServer;
	    c->next->prevServer = c->prevServer;
	}
    }

    /* free up all the data */
    c->attr->cached = AuFalse;
    AuFreeBucketAttributes(c->aud, 1, c->attr);
    Aufree(c);
}

void
_AuRemoveFromBucketCache(aud, bucket)
AuServer       *aud;
AuBucketID      bucket;
{
    CacheEntryPtr   c = cache;

    while (c)
    {
	if (c->aud == aud)
	{
	    do
	    {
		if (AuBucketIdentifier(c->attr) == bucket)
		{
		    removeFromCache(c);
		    return;
		}

		c = c->next;
	    } while (c);
	}
	else
	    c = c->nextServer;
    }
}

/* remove all the entried for aud from the cache */
void
_AuFreeBucketCache(aud)
AuServer       *aud;
{
    CacheEntryPtr   c = cache,
                    n;

    while (c)
    {
	if (c->aud == aud)
	{
	    do
	    {
		n = c->next;
		removeFromCache(c);
		c = n;
	    } while (c);
	}
	else
	    c = c->nextServer;
    }
}

AuBucketAttributes *
AuGetBucketAttributes(aud, bucket, ret_status)
AuServer       *aud;
AuBucketID      bucket;
AuStatus       *ret_status;
{
    register auResourceReq *req;
    auGetBucketAttributesReply rep;
    auBucketAttributes a;
    AuBucketAttributes *attr;

    if (ret_status)
	*ret_status = AuSuccess;

    if (attr = _AuLookupBucketInCache(aud, bucket))
	return attr;

    _AuLockServer(aud);
    _AuGetResReq(GetBucketAttributes, bucket, req, aud);

    (void) _AuReply(aud, (auReply *) & rep, 0, auFalse, ret_status);

    _AuReadPad(aud, (char *) &a, SIZEOF(auBucketAttributes));

    if (!(attr = (AuBucketAttributes *)
	  Aucalloc(1, sizeof(AuBucketAttributes))))
    {
	_AuUnlockServer(aud);
	_AuSyncHandle(aud);
	return NULL;
    }

    _xferBucketAttributes(&a, *attr);

    if ((AuBucketValueMask(attr) & AuCompCommonDescriptionMask) &&
	AuBucketDescription(attr)->len)
    {
	if (!(AuBucketDescription(attr)->data = (char *)
	      Aumalloc(AuBucketDescription(attr)->len + 1)))
	{
	    AuFreeBucketAttributes(aud, 1, attr);
	    _AuUnlockServer(aud);
	    _AuSyncHandle(aud);
	    return NULL;
	}

	_AuReadPad(aud, AuBucketDescription(attr)->data,
		   AuBucketDescription(attr)->len);

	AuBucketDescription(attr)->data[AuBucketDescription(attr)->len] = 0;
    }

    _AuUnlockServer(aud);
    _AuSyncHandle(aud);

    _AuAddToBucketCache(aud, attr);
    return attr;
}

/* ARGSUSED */
void
AuFreeBucketAttributes(aud, num, attr)
AuServer       *aud;
int             num;
AuBucketAttributes *attr;
{
    AuBucketAttributes *p = attr;

    if (!num || p->cached)
	return;

    while (num--)
    {
	if (AuBucketDescription(p)->data)
	    Aufree(AuBucketDescription(p)->data);

	p++;
    }

    Aufree(attr);
}

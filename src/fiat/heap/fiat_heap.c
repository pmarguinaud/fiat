/*
 * (C) Copyright 2022- ECMWF.
 * (C) Copyright 2022- Meteo-France.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation
 * nor does it submit to any jurisdiction.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUDDY_ALLOC_IMPLEMENTATION
#include "buddy_alloc.h"
#undef BUDDY_ALLOC_IMPLEMENTATION

typedef struct
{
  void * metadata;
  void * arena;
  struct buddy * buddy;
} fiat_heap_t;

void fiat_heap_new (fiat_heap_t ** heap, size_t size)
{
  *heap = (fiat_heap_t *)malloc (sizeof (fiat_heap_t));
  (*heap)->metadata = malloc (buddy_sizeof (size));
  (*heap)->arena    = malloc (size);
  (*heap)->buddy    = buddy_init ((*heap)->metadata, (*heap)->arena, size);
}

void fiat_heap_delete (fiat_heap_t * heap)
{
  if (heap->metadata)
    free (heap->metadata); 
  heap->metadata = NULL;
  if (heap->arena)
    free (heap->arena);    
  heap->arena = NULL;
  heap->buddy = NULL;
  free (heap);
}

void fiat_heap_allocate (fiat_heap_t * heap, size_t size, void ** ptr)
{
  *ptr = buddy_malloc (heap->buddy, size);
  if (*ptr == NULL)
    abort ();
}

void fiat_heap_deallocate (fiat_heap_t * heap, void * ptr)
{
  buddy_free (heap->buddy, ptr);
}


/* Common Lisp bindings for GTK+ v2.x
 * Copyright 1999-2005 Espen S. Johnsen <espen@users.sf.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/* $Id: callback.c,v 1.5 2005-04-23 16:48:51 espen Exp $ */

#include <glib-object.h>

/* #ifdef CMUCL */
/* #include "lisp.h" */

/* void (*log_handler) (gchar*, guint, gchar*); */
/* #endif */



GClosure*
clg_callback_closure_new (gpointer callback_id, gpointer callback,
			  gpointer destroy_notify)
{
  GClosure *closure;

  closure = g_closure_new_simple (sizeof (GClosure), (gpointer)callback_id);
  g_closure_set_meta_marshal (closure, callback_id, callback);
  g_closure_add_finalize_notifier (closure, callback_id, destroy_notify);
  
  return closure;
}


/* void */
/* g_logv (const gchar   *log_domain, */
/* 	GLogLevelFlags log_level, */
/* 	const gchar   *format, */
/* 	va_list	       args1) */
/* { */
/*   gchar *msg = g_strdup_vprintf (format, args1); */
/*   log_handler (log_domain, log_level, msg); */

/*   /\* Normally log_handler won't return, so we will be leaking this memory *\/ */
/*   g_free (msg); */
/* } */

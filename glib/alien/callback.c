/* Common Lisp bindings for GTK+ v2.0
 * Copyright (C) 1999-2002 Espen S. Johnsen <espen@users.sourceforge.net>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* $Id: callback.c,v 1.4 2005-02-14 00:38:47 espen Exp $ */

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

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

/* $Id: callback.c,v 1.2 2004-10-31 11:34:47 espen Exp $ */

#include <glib-object.h>

#ifdef CMUCL
#include "lisp.h"
#include "alloc.h"
#include "arch.h"

lispobj callback_trampoline;
lispobj destroy_user_data;
lispobj log_handler;
#endif


void callback_marshal (guint callback_id, GValue *return_value,
		       guint n_params, const GValue *param_values)
{
#ifdef CMUCL
  funcall3 (callback_trampoline, alloc_number ((unsigned int)callback_id),
	    alloc_cons (alloc_number (n_params), alloc_sap (param_values)),
	    alloc_sap (return_value));
#elif defined(CLISP)
  callback_trampoline ((unsigned long)callback_id,
		       n_params, (unsigned int)param_values,
		       (unsigned int)return_value);
#endif
}

void destroy_notify (gpointer data)
{ 
#ifdef CMUCL
  funcall1 (destroy_user_data, alloc_number ((unsigned long)data));
#elif defined(CLISP)
  destroy_user_data ((unsigned long)data);
#endif
}

/* #ifndef CMUCL */
/* void* */
/* destroy_notify_address () */
/* { */
/*   return (void*)destroy_notify; */
/* } */
/* #endif */



void closure_callback_marshal (GClosure *closure,
			       GValue *return_value,
			       guint n_params,
			       const GValue *param_values,
			       gpointer invocation_hint,
			       gpointer marshal_data)
{
  callback_marshal ((guint)closure->data, return_value, n_params, param_values);
}

void closure_destroy_notify (gpointer data, GClosure *closure)
{ 
  destroy_notify (data);
}

GClosure*
g_lisp_callback_closure_new (guint callback_id)
{
  GClosure *closure;

  closure = g_closure_new_simple (sizeof (GClosure), (gpointer)callback_id);
  g_closure_set_marshal (closure, closure_callback_marshal);
  g_closure_add_finalize_notifier (closure, (gpointer)callback_id, closure_destroy_notify);
  
  return closure;
}


/* Callback function used for idle and timeout */
gboolean source_callback_marshal (gpointer data)
{
  GValue return_value;  
  
  memset (&return_value, 0, sizeof (GValue));
  g_value_init (&return_value, G_TYPE_BOOLEAN);
  callback_marshal ((guint)data, &return_value, 0, NULL);

  return g_value_get_boolean (&return_value);
}

void
g_logv (const gchar   *log_domain,
	GLogLevelFlags log_level,
	const gchar   *format,
	va_list	       args1)
{
  gchar *msg = g_strdup_vprintf (format, args1);
  lispobj lisp_msg = alloc_string (msg);
  g_free (msg);
  
  funcall3 (log_handler, alloc_string (log_domain),
	    alloc_number ((unsigned int)log_level), lisp_msg);
}

/* Common Lisp bindings for GTK+ v2.0
 * Copyright (C) 1999-2000 Espen S. Johnsen <espejohn@online.no>
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

/* $Id: callback.c,v 1.6 2001-12-12 20:24:41 espen Exp $ */

#include <glib-object.h>

#ifdef CMUCL
#include "lisp.h"
#include "alloc.h"
#include "arch.h"

lispobj callback_trampoline;
lispobj destroy_user_data;
#endif

void destroy_notify (gpointer data);


void lisp_callback_marshal (GClosure *closure,
			    GValue *return_value,
			    guint n_params,
			    const GValue *param_values,
			    gpointer invocation_hint,
			    gpointer marshal_data)
{
#ifdef CMUCL
  funcall3 (callback_trampoline, alloc_number ((unsigned int)closure->data),
	    alloc_cons (alloc_number (n_params), alloc_sap (param_values)),
	    alloc_sap (return_value));
#elif defined(CLISP)
  callback_trampoline ((unsigned long)closure->data,
		       n_params, (unsigned int)param_values,
		       (unsigned int)return_value);
#endif
}

void closure_destroy_notify (gpointer callback_id, GClosure *closure)
{ 
  destroy_notify (callback_id);
}

void destroy_notify (gpointer data)
{ 
#ifdef CMUCL
  funcall1 (destroy_user_data, alloc_number ((unsigned long)data));
#elif defined(CLISP)
  destroy_user_data ((unsigned long)data);
#endif
}

GClosure*
g_lisp_callback_closure_new (guint callback_id)
{
  GClosure *closure;

  closure = g_closure_new_simple (sizeof (GClosure), (gpointer)callback_id);
  g_closure_set_marshal (closure, lisp_callback_marshal);
  g_closure_add_finalize_notifier (closure, (gpointer)callback_id, closure_destroy_notify);
  
  return closure;
}

#ifndef CMUCL
void*
destroy_notify_address ()
{
  return (void*)destroy_notify;
}
#endif


GEnumValue*
g_enum_class_values (GEnumClass *class, guint *n_values)
{
  *n_values = class->n_values;
  return class->values;
}

GFlagsValue*
g_flags_class_values (GFlagsClass *class, guint *n_values)
{
  *n_values = class->n_values;
  return class->values;
}


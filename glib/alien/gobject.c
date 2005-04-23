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

/* $Id: gobject.c,v 1.3 2005-04-23 16:48:51 espen Exp $ */

#include <glib-object.h>

gpointer g_object_newvv (GType object_type, guint n_parameters,
			 guchar* names[], GValue* values[])
{
  GParameter parameters[n_parameters];
  int i;

  for (i = 0; i < n_parameters; i++) {
    parameters[i].name = names[i];
    g_value_init (&parameters[i].value, G_VALUE_TYPE (values[i]));
    g_value_copy (values[i], &parameters[i].value);
  }

  return g_object_newv (object_type, n_parameters, &parameters);
}


guint size_of_gvalue ()
{
  return sizeof (GValue);
}


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


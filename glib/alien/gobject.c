/* $Id: gobject.c,v 1.2 2004-10-31 11:35:59 espen Exp $ */

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


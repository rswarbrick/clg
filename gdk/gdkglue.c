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

/* $Id: gdkglue.c,v 1.2 2001-10-21 23:02:40 espen Exp $ */


#include <gdk/gdk.h>
#include <gdk/gdkx.h>


extern Display *gdk_display;

gint gdk_connection_number ()
{
  return ConnectionNumber (gdk_display); 
}


GdkWindow*
gdk_get_root_window ()
{
  return gdk_window_foreign_new (GDK_ROOT_WINDOW ());
}

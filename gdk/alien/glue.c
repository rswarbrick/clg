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

/* $Id: glue.c,v 1.4 2007-04-06 14:25:20 espen Exp $ */


#include <gdk/gdk.h>
#include <gdk/gdkx.h>

struct _GdkDisplayX11
{
  GdkDisplay parent_instance;
  Display *xdisplay;
};

gint clg_gdk_connection_number (GdkDisplay *display)
{
  return ConnectionNumber (((struct _GdkDisplayX11 *)display)->xdisplay);
}


GdkWindow *clg_gdk_cairo_xlib_surface_get_window (cairo_surface_t *surface)
{
  return gdk_window_lookup (cairo_xlib_surface_get_drawable (surface));
}

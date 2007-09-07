/* Common Lisp bindings for GTK+ v2.x
 * Copyright 1999-2007 Espen S. Johnsen <espen@users.sf.net>
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

/* $Id: glue.c,v 1.8 2007-09-07 07:38:22 espen Exp $ */


#include <gdk/gdk.h>

#ifdef GDK_WINDOWING_X11
#include <gdk/gdkx.h>
#elif defined (G_OS_WIN32)
#include <gdk/gdkwin32.h>
#endif

gint clg_gdk_connection_number (GdkDisplay *display)
{
#ifdef GDK_WINDOWING_X11
  return ConnectionNumber (GDK_DISPLAY_XDISPLAY (display));
#else
  return -1;
#endif
}


GdkWindow *clg_gdk_cairo_surface_get_window (cairo_surface_t *surface)
{
  /* If 'surface_info_key' had been public we would have had a
     portable way to find the GdkWindow of a Cairo surface. */
  
#ifdef GDK_WINDOWING_X11
  g_return_if_fail (cairo_surface_get_type (surface) == CAIRO_SURFACE_TYPE_XLIB);
  
  Display* display = cairo_xlib_surface_get_display (surface);
  if (display) {
    Drawable window = cairo_xlib_surface_get_drawable (surface);
    if (window)
      return gdk_window_lookup_for_display (window, display);
  }

  return NULL;
#elif defined (G_OS_WIN32)
  HDC hdc = (HDC)cairo_win32_surface_get_dc (surface);
  if (hdc)
    return gdk_window_lookup ((GdkNativeWindow)hdc);
  else
    return NULL;
#else
  return NULL;
#endif
}

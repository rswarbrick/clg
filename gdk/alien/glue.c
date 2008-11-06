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

/* $Id: glue.c,v 1.9 2008-11-06 17:27:39 espen Exp $ */


#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

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

void clg_gdk_pixbuf_swap_rgb (GdkPixbuf *pixbuf)
{

  int n_channels = gdk_pixbuf_get_n_channels (pixbuf);
  guchar *p, *pixels = gdk_pixbuf_get_pixels (pixbuf);
  int width = gdk_pixbuf_get_width (pixbuf);
  int height = gdk_pixbuf_get_height (pixbuf);
  int rowstride = gdk_pixbuf_get_rowstride (pixbuf);
  int x, y;

  g_assert (gdk_pixbuf_get_bits_per_sample (pixbuf) == 8);
  g_assert (gdk_pixbuf_get_has_alpha (pixbuf));
  g_assert (n_channels == 4);

  for (y = 0; y < height; y++) {
    for (x = 0; x < width; x++) {
      p = pixels + y * rowstride + x * n_channels;
      guint tmp = p[0];
      p[0] = p[2];
      p[2] = tmp;
    }
  }
}


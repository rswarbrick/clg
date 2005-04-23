/* Common Lisp bindings for GTK+ v2.x
 * Copyright (C) 2005 Espen S. Johnsen <espen@users.sf.net>
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

/* $Id: logging.c,v 1.2 2005-04-23 16:48:51 espen Exp $ */

#include <glib.h>

#define MAX_MSG_LEN 1000

void (*log_handler) (gchar*, guint, gchar*);


void
g_logv (const gchar   *log_domain,
	GLogLevelFlags log_level,
	const gchar   *format,
	va_list	       args1)
{
  char msg[MAX_MSG_LEN];

  vsnprintf (msg, MAX_MSG_LEN, format, args1);
  log_handler (log_domain, log_level, msg);
}

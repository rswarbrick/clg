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

/* $Id: logging.c,v 1.1 2005-02-14 00:37:15 espen Exp $ */

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

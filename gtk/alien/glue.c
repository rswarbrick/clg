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

/* $Id: glue.c,v 1.4 2004-12-20 20:00:46 espen Exp $ */


#include <gtk/gtk.h>


/*
 *
 * Gtk helper functions
 *
 */

void
gtk_query_version (guint *major, guint *minor, guint *micro)
{
  *major = gtk_major_version;
  *minor = gtk_minor_version;
  *micro = gtk_micro_version;
}


/* Widget */

GdkWindow*
gtk_widget_get_window (GtkWidget *widget)
{
  return widget->window;
}

GtkStateType
gtk_widget_get_state (GtkWidget *widget)
{
  return widget->state;
}

gboolean
gtk_widget_mapped_p (GtkWidget *widget)
{
  return GTK_WIDGET_MAPPED (widget);
}

void
gtk_widget_get_size_allocation (GtkWidget *widget, int *width, int *height)
{
  *width = widget->allocation.width;
  *height = widget->allocation.height;
}


/* Container */

GtkWidget*
gtk_container_get_focus_child (GtkContainer *container)
{
  return container->focus_child;
}


/* Dialog */

GtkWidget*
gtk_dialog_get_vbox (GtkDialog *dialog)
{
  return dialog->vbox;
}

GtkWidget*
gtk_dialog_get_action_area (GtkDialog *dialog)
{
  return dialog->action_area;
}



/* Window */

GtkWidget*
gtk_window_get_default (GtkWindow *window)
{
  return window->default_widget;
}


/* Layout */

GdkWindow*
gtk_layout_get_bin_window (GtkLayout *layout)
{
  return layout->bin_window;
}


/* Drawing area */

void
gtk_drawing_area_get_size (GtkDrawingArea *darea, gint *width, gint *height)
{
  GtkWidget *widget;

  widget = GTK_WIDGET (darea);
  *width = widget->allocation.width;
  *height = widget->allocation.height;
}


/* GtkStyle accessor functions */

typedef enum {
  GTK_COLOR_FG,
  GTK_COLOR_BG,
  GTK_COLOR_LIGHT,
  GTK_COLOR_DARK,
  GTK_COLOR_MID,
  GTK_COLOR_TEXT,
  GTK_COLOR_BASE,
  GTK_COLOR_TEXT_AA,
  GTK_COLOR_WHITE,
  GTK_COLOR_BLACK
} GtkColorType;

GdkColor*
gtk_style_get_color (GtkStyle *style, GtkColorType color_type,
		     GtkStateType state)
{
  switch (color_type)
    {
    case GTK_COLOR_WHITE:
      return &style->white;
    case GTK_COLOR_BLACK:
      return &style->black;
    case GTK_COLOR_FG:
      return &style->fg[state];
    case GTK_COLOR_BG:
      return &style->bg[state];
    case GTK_COLOR_LIGHT:
      return &style->light[state];
    case GTK_COLOR_DARK:
      return &style->dark[state];
    case GTK_COLOR_MID:
      return &style->mid[state];
    case GTK_COLOR_TEXT:
      return &style->text[state];
    case GTK_COLOR_BASE:
      return &style->base[state];
    case GTK_COLOR_TEXT_AA:
      return &style->text_aa[state];
    }
}


void
gtk_style_set_color (GtkStyle *style, GtkColorType color_type,
		     GtkStateType state, GdkColor *color)
{
  switch (color_type)
    {
    case GTK_COLOR_WHITE:
      style->white = *color; break;
    case GTK_COLOR_BLACK:
      style->black = *color; break;
    case GTK_COLOR_FG:
      style->fg[state] = *color; break;
    case GTK_COLOR_BG:
      style->bg[state]  = *color; break;
    case GTK_COLOR_LIGHT:
      style->light[state]  = *color; break;
    case GTK_COLOR_DARK:
      style->dark[state]  = *color; break;
    case GTK_COLOR_MID:
      style->mid[state]  = *color; break;
    case GTK_COLOR_TEXT:
      style->text[state]  = *color; break;
    case GTK_COLOR_BASE:
      style->base[state]  = *color; break;
    case GTK_COLOR_TEXT_AA:
      style->text_aa[state]  = *color; break;
    }
}


GdkGC*
gtk_style_get_gc (GtkStyle *style, GtkColorType color_type, GtkStateType state)
{
  switch (color_type)
    {
    case GTK_COLOR_WHITE:
      return style->white_gc;
    case GTK_COLOR_BLACK:
      return style->black_gc;
    case GTK_COLOR_FG:
      return style->fg_gc[state];
    case GTK_COLOR_BG:
      return style->bg_gc[state];
    case GTK_COLOR_LIGHT:
      return style->light_gc[state];
    case GTK_COLOR_DARK:
      return style->dark_gc[state];
    case GTK_COLOR_MID:
      return style->mid_gc[state];
    case GTK_COLOR_TEXT:
      return style->text_gc[state];
    case GTK_COLOR_BASE:
      return style->base_gc[state];
    case GTK_COLOR_TEXT_AA:
      return style->text_aa_gc[state];
    }
}

int
gtk_style_font_desc_offset ()
{
  GtkStyle style;
  
  return (int)&style.font_desc - (int)&style;
}

